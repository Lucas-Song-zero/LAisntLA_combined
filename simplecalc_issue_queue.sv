// typical out-of-order dispatch queue
// becuase add,sub , logic ops , shift ops are all completed in 1 clk
// so we can simply use a pre-wakeup + forward logic to avoid the instrs gap
// besides, we believe mul can also be completed in 1 clk
`include "defs.sv"

module simplecalc_issue_queue(
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
    // if multiple instrs come in one cycle, must keep them close in order
    input logic [3:0] gen_op_type_vec [3:0],
    input logic [3:0] spec_op_type_vec [4:0],
    input logic [3:0] imm_vec [25:0],
    input logic [3:0] imm_enable_vec,
    input logic [3:0] preg_rd_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] preg_rj_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] preg_rk_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] reg_rd_exist_vec,
    input logic [3:0] reg_rj_exist_vec,
    input logic [3:0] reg_rk_exist_vec,
    input logic [3:0] rj_ready_vec, // read from busy table and wb-bypass
    input logic [3:0] rk_ready_vec, // read from busy table and wb-bypass
    input logic [31:0] pc_vec [3:0], // used for BJ and PCADDU12I
    input logic [3:0] pred_taken_vec_in, // input pred if this instr is a taken branch
    input logic [31:0] pred_jump_pc_vec [3:0], // used for BJ

    // wake_up signal
    // wake-up signals inside the queue, if one instr is selected in ALU_IQ
    // then in this cycle, we will wake up the related instrs
    // but the inside wake up signals don't need io
    // outer wake-up input is preg_rd
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
    // output logic issued_imm_enable,
    output logic [31:0] issued_pc,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rd,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rj,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rk,
    output logic issued_reg_rd_exist,
    output logic issued_reg_rj_exist,
    output logic issued_reg_rk_exist,
    // used for PRF read-out
    output logic [31:0] issued_pred_jump_pc, // 预测的指令跳转pc
    output logic issued_pred_taken // 预测这条指令是否taken
);

// a compressed queue for maintaining the instr order
SIMPLE_IQ_ENTRY_t simple_IQ_queue [`IQ_DEPTH-1:0]; // typical 32 instr entry
// at one cycle, at most issue one instr

// besides preg_IQ, we have imm_IQ and op_type_IQ
// because we don't need to change imm_IQ and op_type_IQ when wake-up
// so we decide to split them into 3 queus to accelerate read-out and change speed
logic [25:0] imm_issue_queue [`IQ_DEPTH-1:0];
logic [8:0] op_type_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] pc_issue_queue [`IQ_DEPTH-1:0]; // instr's pc
logic [31:0] pred_jump_pc_issue_queue [`IQ_DEPTH-1:0]; // instr's predicted jump pc
// only BPU predicted taken branch instr will have this pred jump pc, normal instr will simply have pc+4

// and if one instr is issued, we compress the following instrs in the queue
// because it's an compressed queue, we don't need queue_head
logic [`IQ_INDEX_WIDTH:0] filled_entry_cnt; // the number of filled entries in the queue (0~32)

assign IQ_full = (filled_entry_cnt == `IQ_DEPTH);
assign IQ_empty = (filled_entry_cnt == 0);

logic alloc_success;
assign alloc_success = (filled_entry_cnt+rename_instr_num <= `IQ_DEPTH);
assign issue_ready = alloc_success;
// NOTE: here we consider the worst case: no freed entry

// shadow变量定义
SIMPLE_IQ_ENTRY_t next_simple_IQ_queue [`IQ_DEPTH-1:0];
logic [25:0] next_imm_issue_queue [`IQ_DEPTH-1:0];
logic [8:0] next_op_type_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] next_pc_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] next_pred_jump_pc_issue_queue [`IQ_DEPTH-1:0];
logic [`IQ_INDEX_WIDTH-1:0] next_filled_entry_cnt;
logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index;

always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        filled_entry_cnt <= 0;
        for (int i = 0; i < `IQ_DEPTH; i++) begin
            simple_IQ_queue[i] <= 0;
            imm_issue_queue[i] <= 0;
            op_type_issue_queue[i] <= 0;
            pc_issue_queue[i] <= 0;
            pred_jump_pc_issue_queue[i] <= 0;

            // outputs initialization
            issue_valid <= 0;
            issued_rob_entry_index <= 0;
            issued_gen_op_type <= 0;
            issued_spec_op_type <= 0;
            issued_imm <= 0;
            issued_imm_enable <= 0;
            issued_pc <= 0;
            issued_preg_rd <= 0;
            issued_preg_rj <= 0;
            issued_preg_rk <= 0;
            issued_reg_rd_exist <= 0;
            issued_reg_rj_exist <= 0;
            issued_reg_rk_exist <= 0;
            issued_pred_taken <= 0;
        end
    end else begin
        // 1. 先拷贝原队列到shadow变量
        for (int i = 0; i < `IQ_DEPTH; i++) begin
            next_simple_IQ_queue[i] = simple_IQ_queue[i];
            next_imm_issue_queue[i] = imm_issue_queue[i];
            next_op_type_issue_queue[i] = op_type_issue_queue[i];
            next_pc_issue_queue[i] = pc_issue_queue[i];
            next_pred_jump_pc_issue_queue[i] = pred_jump_pc_issue_queue[i];
        end
        next_filled_entry_cnt = filled_entry_cnt;

        // 2. 发射+pre-wakeup+compress
        if (fu_ready && !IQ_empty) begin
            for(int i=0; i<`IQ_DEPTH; i++) begin
                if(next_simple_IQ_queue[i].issued == 1'b0
                && (next_simple_IQ_queue[i].rj_ready || !next_simple_IQ_queue[i].rj_valid)
                && (next_simple_IQ_queue[i].rk_ready || !next_simple_IQ_queue[i].rk_valid)
                ) begin
                    // issue this instr
                    issue_valid <= 1'b1; // 发射信号拉高
                    issued_rob_entry_index <= next_simple_IQ_queue[i].rob_entry_index;
                    issued_gen_op_type <= next_op_type_issue_queue[i][8:5];
                    issued_spec_op_type <= next_op_type_issue_queue[i][4:0];
                    issued_imm <= next_imm_issue_queue[i];
                    issued_imm_enable <= next_simple_IQ_queue[i].imm_enable;
                    issued_preg_rd <= next_simple_IQ_queue[i].rd_index;
                    issued_preg_rj <= next_simple_IQ_queue[i].rj_index;
                    issued_preg_rk <= next_simple_IQ_queue[i].rk_index;
                    issued_reg_rd_exist <= next_simple_IQ_queue[i].rd_valid;
                    issued_reg_rj_exist <= next_simple_IQ_queue[i].rj_valid;
                    issued_reg_rk_exist <= next_simple_IQ_queue[i].rk_valid;
                    issued_pred_taken <= next_simple_IQ_queue[i].pred_taken;
                    issued_pc <= next_pc_issue_queue[i];
                    issued_pred_jump_pc <= next_pred_jump_pc_issue_queue[i];

                    // pre-wakeup (只操作shadow)
                    for(int k=0; k<`IQ_DEPTH; k++) begin
                        if(next_simple_IQ_queue[k].rj_index == next_simple_IQ_queue[i].rd_index) begin
                            next_simple_IQ_queue[k].rj_ready = 1'b1;
                        end else if(next_simple_IQ_queue[k].rk_index == next_simple_IQ_queue[i].rd_index) begin
                            next_simple_IQ_queue[k].rk_ready = 1'b1;
                        end
                    end
                    // compress (只操作shadow)
                    for(int j=i+1; j<`IQ_DEPTH; j++) begin
                        next_simple_IQ_queue[j-1] = next_simple_IQ_queue[j];
                        next_imm_issue_queue[j-1] = next_imm_issue_queue[j];
                        next_op_type_issue_queue[j-1] = next_op_type_issue_queue[j];
                        next_pc_issue_queue[j-1] = next_pc_issue_queue[j];
                        next_pred_jump_pc_issue_queue[j-1] = next_pred_jump_pc_issue_queue[j];
                    end
                    // 清空最后一项 (not necessary)
                    // next_preg_issue_queue[next_filled_entry_cnt-1] = 0;
                    // next_imm_issue_queue[next_filled_entry_cnt-1] = 0;
                    // next_op_type_issue_queue[next_filled_entry_cnt-1] = 0;
                    next_filled_entry_cnt = next_filled_entry_cnt - 1;

                    // 写入新指令（如果有）
                    if (rename_valid && !IQ_full && alloc_success) begin
                        for(int x=0; x<rename_instr_num; x=x+1) begin
                            next_simple_IQ_queue[next_filled_entry_cnt].rob_entry_index = rob_entry_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rj_index = preg_rj_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rj_valid = reg_rj_exist_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rj_ready = rj_ready_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rk_index = preg_rk_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rk_valid = reg_rk_exist_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rk_ready = rk_ready_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rd_index = preg_rd_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].rd_valid = reg_rd_exist_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].imm_enable = imm_enable_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                            next_pred_jump_pc_issue_queue[next_filled_entry_cnt] = pred_jump_pc_vec[x];
                            next_simple_IQ_queue[next_filled_entry_cnt].pred_taken = pred_taken_vec_in[x];
                            
                            next_imm_issue_queue[next_filled_entry_cnt] = imm_vec[x];
                            next_op_type_issue_queue[next_filled_entry_cnt] = {gen_op_type_vec[x], spec_op_type_vec[x]};
                            next_pc_issue_queue[next_filled_entry_cnt] = pc_vec[x];
                            next_filled_entry_cnt = next_filled_entry_cnt + 1;
                        end
                    end
                    // break
                    break;
                end
            end
        end
        // no issued instr , simply write in one instr
        if (rename_valid && !IQ_full && alloc_success) begin
            for(int x=0; x<rename_instr_num+1; x=x+1) begin
                next_simple_IQ_queue[next_filled_entry_cnt].rob_entry_index = rob_entry_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rj_index = preg_rj_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rj_valid = reg_rj_exist_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rj_ready = rj_ready_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rk_index = preg_rk_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rk_valid = reg_rk_exist_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rk_ready = rk_ready_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rd_index = preg_rd_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].rd_valid = reg_rd_exist_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].imm_enable = imm_enable_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                next_pred_jump_pc_issue_queue[next_filled_entry_cnt] = pred_jump_pc_vec[x];
                next_simple_IQ_queue[next_filled_entry_cnt].pred_taken = pred_taken_vec_in[x];

                next_imm_issue_queue[next_filled_entry_cnt] = imm_vec[x];
                next_op_type_issue_queue[next_filled_entry_cnt] = {gen_op_type_vec[x], spec_op_type_vec[x]};
                next_pc_issue_queue[next_filled_entry_cnt] = pc_vec[x];
                next_filled_entry_cnt = next_filled_entry_cnt + 1;
            end
        end
        // 外部wake-up（只操作shadow）
        for(int i=0; i<4; i++) begin
            case(i)
                0: wb_rd_index = write_back_rd_index_0;
                1: wb_rd_index = write_back_rd_index_1;
                2: wb_rd_index = write_back_rd_index_2;
                3: wb_rd_index = write_back_rd_index_3;
            endcase
            if(write_back_rd_exist_vec[i]) begin
                for(int j=0; j<`IQ_DEPTH; j++) begin
                    if(next_simple_IQ_queue[j].rj_index == wb_rd_index) begin
                        next_simple_IQ_queue[j].rj_ready = 1'b1;
                    end else if(next_simple_IQ_queue[j].rk_index == wb_rd_index) begin
                        next_simple_IQ_queue[j].rk_ready = 1'b1;
                    end
                end
            end
        end
        // 最后统一赋值
        for (int i = 0; i < `IQ_DEPTH; i++) begin
            simple_IQ_queue[i] <= next_simple_IQ_queue[i];
            imm_issue_queue[i] <= next_imm_issue_queue[i];
            op_type_issue_queue[i] <= next_op_type_issue_queue[i];
            pc_issue_queue[i] <= next_pc_issue_queue[i];
            pred_jump_pc_issue_queue[i] <= next_pred_jump_pc_issue_queue[i];
        end
        filled_entry_cnt <= next_filled_entry_cnt;
    end
end

endmodule