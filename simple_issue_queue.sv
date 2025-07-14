// typical out-of-order dispatch queue
// becuase add,sub , logic ops , shift ops are all completed in 1 clk
// so we can simply use a pre-wakeup + forward logic to avoid the instrs gap
// besides, we believe mul can also be completed in 1 clk
`include "mycpu.h"

module simple_issue_queue(
    input logic clk,
    input logic rst_n,
    // interface with rename and FU
    input logic rename_valid,
    input logic fu_ready,
    input logic [2:0] rename_instr_num, // 001: 1 instr, 010: 2 instr, 011: 3 instr, 100: 4 instr
    // 指令信息数组
    input simple_single_instr_info_t instr_info [3:0],
    output logic [2:0] left_IQ_entry_cnt, // 剩余的IQ entry数量（只关心0~4条，多于4个空位的情况一律按照5条处理）
    output logic issue_ready, // simple IQ 是否有空间用于发射
    output logic issue_valid, // 发射给FU的数据是否有效

    // wake_up signal
    // wake-up signals inside the queue, if one instr is selected in ALU_IQ
    // then in this cycle, we will wake up the related instrs
    // but the inside wake up signals don't need io
    // outer wake-up input is preg_rd
    input logic [3:0] write_back_rd_exist_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_vec [3:0],

    // when issue is ready, give out some info to FU
    output simple_issue_queue_issued_info_t issued_info
    // output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] issued_rob_entry_index,
    // output logic [3:0] issued_gen_op_type,
    // output logic [4:0] issued_spec_op_type,
    // output logic [25:0] issued_imm,
    // output logic issued_imm_enable,
    // output logic issued_imm_sign_extend,
    // output logic [31:0] issued_pc,
    // output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rd,
    // output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rj,
    // output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rk,
    // output logic issued_reg_rd_exist,
    // output logic issued_reg_rj_exist,
    // output logic issued_reg_rk_exist,
    // // used for PRF read-out
    // output logic [31:0] issued_pred_jump_pc, // 预测的指令跳转pc
    // output logic issued_pred_taken, // 预测这条指令是否taken
    // output logic [1:0] issued_pred_cut_pos, // 预测的分支指令位置
    // output logic [1:0] issued_instr_pos, // 实际发射指令在这个取指块中的位置
    // output logic [31:0] issued_fetch_start_pc // 实际发射指令所在取指块的取指起始地址
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
logic [31:0] fetch_start_pc_issue_queue [`IQ_DEPTH-1:0]; // instr's 所在取指块的 fetch start pc
// only BPU predicted taken branch instr will have this pred jump pc, normal instr will simply have pc+4

// and if one instr is issued, we compress the following instrs in the queue
// because it's an compressed queue, we don't need queue_head
logic [`IQ_INDEX_WIDTH:0] filled_entry_cnt; // the number of filled entries in the queue (0~32)

alwasy_comb begin
    if(filled_entry_cnt < `IQ_DEPTH-4) begin // 剩余空位超过4条，一律按照5条处理
        left_IQ_entry_cnt = 3'd5;
    end else begin
        left_IQ_entry_cnt = filled_entry_cnt[2:0]; // 0~4条 (逻辑验证这个是对的)
    end
end

logic alloc_success;
assign alloc_success = (filled_entry_cnt+rename_instr_num <= `IQ_DEPTH);
assign issue_ready = alloc_success;

// shadow变量定义
SIMPLE_IQ_ENTRY_t next_simple_IQ_queue [`IQ_DEPTH-1:0];
logic [25:0] next_imm_issue_queue [`IQ_DEPTH-1:0];
logic [8:0] next_op_type_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] next_pc_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] next_pred_jump_pc_issue_queue [`IQ_DEPTH-1:0];
logic [31:0] next_fetch_start_pc_issue_queue [`IQ_DEPTH-1:0];
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
            fetch_start_pc_issue_queue[i] <= 0;

            // outputs initialization
            issue_valid <= 0;
            issued_rob_entry_index <= 0;
            issued_gen_op_type <= 0;
            issued_spec_op_type <= 0;
            issued_imm <= 0;
            issued_pc <= 0;
            issued_preg_rd <= 0;
            issued_preg_rj <= 0;
            issued_preg_rk <= 0;
            issued_reg_rd_exist <= 0;
            issued_reg_rj_exist <= 0;
            issued_reg_rk_exist <= 0;
            issued_pred_jump_pc <= 0;
            issued_pred_taken <= 0;
            issued_pred_cut_pos <= 0;
            issued_instr_pos <= 0;
            issued_fetch_start_pc <= 0;
        end
    end else begin
        // 1. 先拷贝原队列到shadow变量
        for (int i = 0; i < `IQ_DEPTH; i++) begin
            next_simple_IQ_queue[i] = simple_IQ_queue[i];
            next_imm_issue_queue[i] = imm_issue_queue[i];
            next_op_type_issue_queue[i] = op_type_issue_queue[i];
            next_pc_issue_queue[i] = pc_issue_queue[i];
            next_pred_jump_pc_issue_queue[i] = pred_jump_pc_issue_queue[i];
            next_fetch_start_pc_issue_queue[i] = fetch_start_pc_issue_queue[i];
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
                    issued_pc <= next_pc_issue_queue[i];
                    issued_preg_rd <= next_simple_IQ_queue[i].rd_index;
                    issued_preg_rj <= next_simple_IQ_queue[i].rj_index;
                    issued_preg_rk <= next_simple_IQ_queue[i].rk_index;
                    issued_reg_rd_exist <= next_simple_IQ_queue[i].rd_valid;
                    issued_reg_rj_exist <= next_simple_IQ_queue[i].rj_valid;
                    issued_reg_rk_exist <= next_simple_IQ_queue[i].rk_valid;
                    issued_pred_taken <= next_simple_IQ_queue[i].pred_taken;
                    issued_pred_jump_pc <= next_pred_jump_pc_issue_queue[i];
                    issued_fetch_start_pc <= next_fetch_start_pc_issue_queue[i];
                    issued_pred_cut_pos <= next_simple_IQ_queue[i].pred_cut_pos;
                    issued_instr_pos <= next_simple_IQ_queue[i].instr_pos;
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
                        next_fetch_start_pc_issue_queue[j-1] = next_fetch_start_pc_issue_queue[j];
                    end
                    // 清空最后一项 (not necessary)
                    // next_preg_issue_queue[next_filled_entry_cnt-1] = 0;
                    // next_imm_issue_queue[next_filled_entry_cnt-1] = 0;
                    // next_op_type_issue_queue[next_filled_entry_cnt-1] = 0;
                    next_filled_entry_cnt = next_filled_entry_cnt - 1;

                    // 写入新指令（如果有）
                    if (rename_valid && !IQ_full && alloc_success) begin
                        for(int x=0; x<4; x=x+1) begin
                            if(x < rename_instr_num) begin
                                                     next_simple_IQ_queue[next_filled_entry_cnt].rob_entry_index = instr_info[x].rob_entry;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_index = instr_info[x].preg_rj;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_valid = instr_info[x].reg_rj_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_ready = instr_info[x].rj_ready;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_index = instr_info[x].preg_rk;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_valid = instr_info[x].reg_rk_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_ready = instr_info[x].rk_ready;
                     next_simple_IQ_queue[next_filled_entry_cnt].rd_index = instr_info[x].preg_rd;
                     next_simple_IQ_queue[next_filled_entry_cnt].rd_valid = instr_info[x].reg_rd_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].imm_enable = instr_info[x].imm_enable;
                     next_simple_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                     next_pred_jump_pc_issue_queue[next_filled_entry_cnt] = instr_info[x].pred_jump_pc;
                     next_simple_IQ_queue[next_filled_entry_cnt].pred_taken = instr_info[x].pred_taken;
                     
                     next_imm_issue_queue[next_filled_entry_cnt] = instr_info[x].imm;
                     next_op_type_issue_queue[next_filled_entry_cnt] = {instr_info[x].gen_op_type, instr_info[x].spec_op_type};
                     next_pc_issue_queue[next_filled_entry_cnt] = instr_info[x].pc;
                     next_fetch_start_pc_issue_queue[next_filled_entry_cnt] = instr_info[x].fetch_start_pc;
                     next_simple_IQ_queue[next_filled_entry_cnt].pred_cut_pos = instr_info[x].pred_cut_pos;
                     next_simple_IQ_queue[next_filled_entry_cnt].instr_pos = instr_info[x].instr_pos;
                                next_filled_entry_cnt = next_filled_entry_cnt + 1;
                            end
                        end
                    end
                    // break
                    break;
                end
            end
        end
        // no issued instr , simply write in one instr
        if (rename_valid && !IQ_full && alloc_success) begin
            for(int x=0; x<4; x=x+1) begin
                if(x < rename_instr_num) begin
                                         next_simple_IQ_queue[next_filled_entry_cnt].rob_entry_index = instr_info[x].rob_entry;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_index = instr_info[x].preg_rj;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_valid = instr_info[x].reg_rj_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].rj_ready = instr_info[x].rj_ready;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_index = instr_info[x].preg_rk;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_valid = instr_info[x].reg_rk_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].rk_ready = instr_info[x].rk_ready;
                     next_simple_IQ_queue[next_filled_entry_cnt].rd_index = instr_info[x].preg_rd;
                     next_simple_IQ_queue[next_filled_entry_cnt].rd_valid = instr_info[x].reg_rd_exist;
                     next_simple_IQ_queue[next_filled_entry_cnt].imm_enable = instr_info[x].imm_enable;
                     next_simple_IQ_queue[next_filled_entry_cnt].issued = 1'b0;
                     next_pred_jump_pc_issue_queue[next_filled_entry_cnt] = instr_info[x].pred_jump_pc;
                     next_simple_IQ_queue[next_filled_entry_cnt].pred_taken = instr_info[x].pred_taken;

                     next_imm_issue_queue[next_filled_entry_cnt] = instr_info[x].imm;
                     next_op_type_issue_queue[next_filled_entry_cnt] = {instr_info[x].gen_op_type, instr_info[x].spec_op_type};
                     next_pc_issue_queue[next_filled_entry_cnt] = instr_info[x].pc;
                    next_filled_entry_cnt = next_filled_entry_cnt + 1;
                end
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
            fetch_start_pc_issue_queue[i] <= next_fetch_start_pc_issue_queue[i];
        end
        filled_entry_cnt <= next_filled_entry_cnt;
    end
end

endmodule