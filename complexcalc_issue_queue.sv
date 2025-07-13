// Out-of-order complex issue queue with countdown mechanism (refactored)
// Structure and style follow simplecalc_issue_queue.sv
`include "defs.sv"

module complexcalc_issue_queue(
    input logic clk,
    input logic rst_n,
    // interface with rename and FU
    input logic rename_valid,
    input logic fu_ready,
    input logic [1:0] rename_instr_num, // 00: 1 instr, 01: 2 instr, 10: 3 instr, 11: 4 instr
    // 指令是连续排列的
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_vec [3:0], // because these instr 可能不是连续的，所以直接给出rob_entry是最合适的
    output logic issue_ready,
    output logic issue_valid,
    output logic IQ_full,
    output logic IQ_empty,

    // necessary info to store
    input logic [3:0] gen_op_type_vec [3:0],
    input logic [3:0] spec_op_type_vec [4:0],
    input logic [3:0] imm_vec [25:0],
    input logic [3:0] imm_enable_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_vec [3:0],
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_vec [3:0],
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_vec [3:0],
    input logic [3:0] reg_rd_exist_vec,
    input logic [3:0] reg_rj_exist_vec,
    input logic [3:0] reg_rk_exist_vec,
    input logic [3:0] rj_ready_vec, // read from busy table and wb-bypass
    input logic [3:0] rk_ready_vec, // read from busy table and wb-bypass

    // wake_up signal
    input logic [3:0] write_back_rd_exist_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_3,

    // when issue is ready, give out some info to FU
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] issued_rob_entry_index,
    output logic [3:0] issued_gen_op_type,
    output logic [4:0] issued_spec_op_type,
    output logic [25:0] issued_imm,
    output logic issued_imm_enable,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rd,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rj,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rk,
    output logic issued_reg_rd_exist,
    output logic issued_reg_rj_exist,
    output logic issued_reg_rk_exist
);

// a compressed queue for maintaining the instr order
COMPLEX_IQ_ENTRY_t complex_IQ_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [25:0] imm_issue_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [8:0] op_type_issue_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [`COMPLEX_IQ_INDEX_WIDTH:0] filled_entry_cnt;

assign IQ_full = (filled_entry_cnt == `COMPLEX_IQ_DEPTH);
assign IQ_empty = (filled_entry_cnt == 0);

logic alloc_success;
assign alloc_success = (filled_entry_cnt + rename_instr_num <= `COMPLEX_IQ_DEPTH);
assign issue_ready = alloc_success;

// 使用counting_down_bits的最低位作为ready信号
logic [`COMPLEX_IQ_DEPTH-1:0] rj_ready_queue;
logic [`COMPLEX_IQ_DEPTH-1:0] rk_ready_queue;

// 专门提取counting_down_bits的3:1这几位作为updating bits
logic [`COUNTING_DOWN_BITS_WIDTH-2:0] rj_updating_counting_down_bits_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [`COUNTING_DOWN_BITS_WIDTH-2:0] rk_updating_counting_down_bits_queue [`COMPLEX_IQ_DEPTH-1:0];

always_comb begin
    for (int i = 0; i < `COMPLEX_IQ_DEPTH; i++) begin
        rj_ready_queue[i] = next_complex_IQ_queue[i].rj_ready[0];
        rk_ready_queue[i] = next_complex_IQ_queue[i].rk_ready[0];
        
        rj_updating_counting_down_bits_queue[i] = next_complex_IQ_queue[i].rj_counting_down_bits[`COUNTING_DOWN_BITS_WIDTH-1:1];
        rk_updating_counting_down_bits_queue[i] = next_complex_IQ_queue[i].rk_counting_down_bits[`COUNTING_DOWN_BITS_WIDTH-1:1];
    end
end

// shadow变量定义
logic [`COMPLEX_IQ_ENTRY_WIDTH-1:0] next_preg_issue_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [25:0] next_imm_issue_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [8:0] next_op_type_issue_queue [`COMPLEX_IQ_DEPTH-1:0];
logic [`COMPLEX_IQ_INDEX_WIDTH-1:0] next_filled_entry_cnt;
logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index;

always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        filled_entry_cnt <= 0;
        for (int i = 0; i < `COMPLEX_IQ_DEPTH; i++) begin
            complex_IQ_queue[i] <= 0;
            imm_issue_queue[i] <= 0;
            op_type_issue_queue[i] <= 0;
            // outputs initialization
            issue_valid <= 0;
            issued_rob_entry_index <= 0;
            issued_gen_op_type <= 0;
            issued_spec_op_type <= 0;
            issued_imm <= 0;
            issued_imm_enable <= 0;
            issued_preg_rd <= 0;
            issued_preg_rj <= 0;
            issued_preg_rk <= 0;
            issued_reg_rd_exist <= 0;
            issued_reg_rj_exist <= 0;
            issued_reg_rk_exist <= 0;
        end
    end else begin
        // 1. 拷贝原队列到shadow变量
        for (int i = 0; i < `COMPLEX_IQ_DEPTH; i++) begin
            next_complex_IQ_queue[i] = complex_IQ_queue[i];
            next_imm_issue_queue[i] = imm_issue_queue[i];
            next_op_type_issue_queue[i] = op_type_issue_queue[i];
        end
        next_filled_entry_cnt = filled_entry_cnt;

        // 2. 发射+pre-wakeup+计数down+compress
        if (fu_ready && !IQ_empty) begin
            for (int i = 0; i < `COMPLEX_IQ_DEPTH; i++) begin
                if (next_complex_IQ_queue[i].issued == 1'b0
                && (rj_ready_queue[i] || !next_complex_IQ_queue[i].rj_valid)
                && (rk_ready_queue[i] || !next_complex_IQ_queue[i].rk_valid)) begin
                    // issue this instr
                    issue_valid <= 1'b1; // 发射信号拉高
                    issued_rob_entry_index <= next_complex_IQ_queue[i].rob_entry_index;
                    issued_gen_op_type <= next_op_type_issue_queue[i][8:5];
                    issued_spec_op_type <= next_op_type_issue_queue[i][4:0];
                    issued_imm <= next_imm_issue_queue[i];
                    issued_imm_enable <= next_complex_IQ_queue[i].imm_enable;
                    issued_preg_rd <= next_complex_IQ_queue[i].rd_index;
                    issued_preg_rj <= next_complex_IQ_queue[i].rj_index;
                    issued_preg_rk <= next_complex_IQ_queue[i].rk_index;
                    issued_reg_rd_exist <= next_complex_IQ_queue[i].rd_valid;
                    issued_reg_rj_exist <= next_complex_IQ_queue[i].rj_valid;
                    issued_reg_rk_exist <= next_complex_IQ_queue[i].rk_valid;

                    // pre-wakeup: counting_down_enable置1
                    for (int k = 0; k < `COMPLEX_IQ_DEPTH; k++) begin
                        if (next_complex_IQ_queue[k].rj_index == next_complex_IQ_queue[i].rd_index)
                            next_complex_IQ_queue[k].rj_counting_down_enable = 1'b1;
                        if (next_complex_IQ_queue[k].rk_index == next_complex_IQ_queue[i].rd_index)
                            next_complex_IQ_queue[k].rk_counting_down_enable = 1'b1;
                    end
                    // 计数down: bits移位
                    for (int z = 0; z < `COMPLEX_IQ_DEPTH; z++) begin
                        if (next_complex_IQ_queue[z].rj_counting_down_enable == 1'b1)
                            next_complex_IQ_queue[z].rj_counting_down_bits = {1'b1, rj_updating_counting_down_bits_queue[z]};
                        if (next_complex_IQ_queue[z].rk_counting_down_enable == 1'b1)
                            next_complex_IQ_queue[z].rk_counting_down_bits = {1'b1, rk_updating_counting_down_bits_queue[z]};
                    end
                    // compress
                    for (int j = i+1; j < `COMPLEX_IQ_DEPTH; j++) begin
                        next_complex_IQ_queue[j-1] = next_complex_IQ_queue[j];
                        next_imm_issue_queue[j-1] = next_imm_issue_queue[j];
                        next_op_type_issue_queue[j-1] = next_op_type_issue_queue[j];
                    end
                    next_filled_entry_cnt = next_filled_entry_cnt - 1;
                    // 写入新指令（如果有）
                    if (rename_valid && alloc_success) begin
                        for (int x = 0; x < rename_instr_num; x = x + 1) begin
                            next_complex_IQ_queue[next_filled_entry_cnt].rob_entry_index = rob_entry_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rj_index = preg_rj_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rj_valid = reg_rj_exist_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rk_index = preg_rk_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rk_valid = reg_rk_exist_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rd_index = preg_rd_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].rd_valid = reg_rd_exist_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].imm_enable = imm_enable_vec[x];
                            next_complex_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                            next_complex_IQ_queue[next_filled_entry_cnt].rj_counting_down_enable = 1'b0;
                            next_complex_IQ_queue[next_filled_entry_cnt].rk_counting_down_enable = 1'b0;
                            next_complex_IQ_queue[next_filled_entry_cnt].rj_counting_down_bits = (rj_ready_vec[x]) ? {`COUNTING_DOWN_BITS_WIDTH{1'b1}} : {`COUNTING_DOWN_BITS_WIDTH{1'b0}};
                            next_complex_IQ_queue[next_filled_entry_cnt].rk_counting_down_bits = (rk_ready_vec[x]) ? {`COUNTING_DOWN_BITS_WIDTH{1'b1}} : {`COUNTING_DOWN_BITS_WIDTH{1'b0}};

                            next_imm_issue_queue[next_filled_entry_cnt] = imm_vec[x];
                            next_op_type_issue_queue[next_filled_entry_cnt] = {gen_op_type_vec[x], spec_op_type_vec[x]};
                            next_filled_entry_cnt = next_filled_entry_cnt + 1;
                        end
                    end
                    break;
                end
            end
        end
        // no issued instr, simply写入新指令
        if (rename_valid && alloc_success) begin
            for (int x = 0; x < rename_instr_num; x = x + 1) begin
                next_complex_IQ_queue[next_filled_entry_cnt].rob_entry_index = rob_entry_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rj_index = preg_rj_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rj_valid = reg_rj_exist_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rk_index = preg_rk_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rk_valid = reg_rk_exist_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rd_index = preg_rd_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].rd_valid = reg_rd_exist_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].imm_enable = imm_enable_vec[x];
                next_complex_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                next_complex_IQ_queue[next_filled_entry_cnt].rj_counting_down_enable = 1'b0;
                next_complex_IQ_queue[next_filled_entry_cnt].rk_counting_down_enable = 1'b0;
                next_complex_IQ_queue[next_filled_entry_cnt].rj_counting_down_bits = (rj_ready_vec[x]) ? {`COUNTING_DOWN_BITS_WIDTH{1'b1}} : {`COUNTING_DOWN_BITS_WIDTH{1'b0}};
                next_complex_IQ_queue[next_filled_entry_cnt].rk_counting_down_bits = (rk_ready_vec[x]) ? {`COUNTING_DOWN_BITS_WIDTH{1'b1}} : {`COUNTING_DOWN_BITS_WIDTH{1'b0}};

                next_imm_issue_queue[next_filled_entry_cnt] = imm_vec[x];
                next_op_type_issue_queue[next_filled_entry_cnt] = {gen_op_type_vec[x], spec_op_type_vec[x]};
                next_filled_entry_cnt = next_filled_entry_cnt + 1;
            end
        end
        // 外部wake-up（只操作shadow）
        for (int i = 0; i < 4; i++) begin
            case(i)
                0: wb_rd_index = write_back_rd_index_0;
                1: wb_rd_index = write_back_rd_index_1;
                2: wb_rd_index = write_back_rd_index_2;
                3: wb_rd_index = write_back_rd_index_3;
            endcase
            if (write_back_rd_exist_vec[i]) begin
                for (int j = 0; j < `COMPLEX_IQ_DEPTH; j++) begin
                    if (next_complex_IQ_queue[j].rj_index == wb_rd_index)
                        next_complex_IQ_queue[j].rj_counting_down_bits = {`COUNTING_DOWN_BITS_WIDTH{1'b1}};
                        // 因为外部wake-up都是已经把值算出来了的
                    if (next_complex_IQ_queue[j].rk_index == wb_rd_index)
                        next_complex_IQ_queue[j].rk_counting_down_bits = {`COUNTING_DOWN_BITS_WIDTH{1'b1}};
                end
            end
        end
        // 最后统一赋值
        for (int i = 0; i < `COMPLEX_IQ_DEPTH; i++) begin
            complex_IQ_queue[i] <= next_complex_IQ_queue[i];
            imm_issue_queue[i] <= next_imm_issue_queue[i];
            op_type_issue_queue[i] <= next_op_type_issue_queue[i];
        end
        filled_entry_cnt <= next_filled_entry_cnt;
    end
end

endmodule 