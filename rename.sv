`timescale 1ns / 1ps
`include "mycpu.h"

// in this work, we use PRF to implement rename
// 32 arch regs can be renamed to typically 128 Physical Regsters (using Macro Defines)
// first we need to get homw mamny instr we get this cycle
// then we need to get these instrs' reg exist vecs
// judged from Combinational Logic results , we get renmae_enable vecs
module rename(
    // interface with prev and next stage
    input logic clk,
    input logic rst_n,
    input logic decoder_valid, // decoder output is valid
    input logic rename_flush, 
    input logic issue_ready, // dispatch is ready to receive renamed instr
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_index_start, // 可以分配的rob index的第一个index
    output logic [2:0] rob_index_num_req, // 需要分配的rob index数量 (0~4)
    input logic rob_ready, // rob is ready to receive new instrs
    // rob_ready是根据rob剩余entry数量和rob_index_num_req决定的
    output logic rename_ready, // ready to rename next cycle (can receive new instr group)
    output logic rename_valid, // rename output is valid

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PREG_INDEX_WIDTH-1:0] recover_preg_index_vec [31:0], // 用于恢复RAT
    input logic [`PRF_NUM-1:0] recover_busy_table, // 用于恢复busy table
    // 是否要加 recover的 freelist? (待定)
    
    // write back interface
    // 从FU会写回两类信号,一类是表示开始计算，此时busy table对应位要置1
    // 另一类是计算/Load完成，此时对应busy table对应位要置0
    // 但是要注意，ST指令的rd是使用不是写回，因此ST指令的rd对应busy table的不需要置1
    // 但是这里是FU和发射队列决定的，这里只需要查看write back信号类型即可
    // 并且因为FU和发射队列可能会同时更新（尤其是一个周期完成的Simple_FU）
    // 所以还需要注意区分
    input logic [3:0] IQ_busy_table_update_vec, // 表示busy table对应位要更新的值, 0表示值准备好，1表示值没准备好
    input logic [`PREG_INDEX_WIDTH-1:0] IQ_busy_table_update_preg_index_vec [3:0], // 表示busy table对应位要更新的preg index
    input logic [3:0] FU_busy_table_update_vec, // 表示busy table对应位要更新的值, 0表示值准备好，1表示值没准备好
    input logic [`PREG_INDEX_WIDTH-1:0] FU_busy_table_update_preg_index_vec [3:0], // 表示busy table对应位要更新的preg index

    // freelist interface from ROB retires
    input logic retire_write_back_valid, // retire outputs are valid
    input logic [3:0] freed_preg_valid_vec, // at most 4 instr commit in one cycle
    // so at most 4 preg index will be freed in one cycle 0~4
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_3,

    // rename输入结构体数组
    input rename_input_t rename_input_vec [3:0],
    
    // PC输入
    input logic [31:0] start_pc_in, // 只有一个start_pc_in，其他的要根据指令是否存在进行计算

    // rename输出结构体数组
    output rename_output_t rename_output_vec [3:0],

    // 和忙表的交互
    input logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_vec [3:0],
    input logic [3:0] begin_exec_valid_vec,
    // IQ发射，置忙表对应位为1，表示开始计算
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_vec [3:0],
    input logic [3:0] wb_valid_vec,
    // FU写回，置忙表对应位为0，表示计算完成
);

// temp signals (should be wire?)
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_3;

logic [`PREG_INDEX_WIDTH-1:0] rd_busy_table_read_index_vec [3:0],
logic [`PREG_INDEX_WIDTH-1:0] rj_busy_table_read_index_vec [3:0],
logic [`PREG_INDEX_WIDTH-1:0] rk_busy_table_read_index_vec [3:0],

// 临时结构体数组用于内部计算
rename_output_t temp_rename_output_vec [3:0];

// logic checker outputs
logic [1:0] rj_0_raw_pos, rj_1_raw_pos, rj_2_raw_pos, rj_3_raw_pos;
logic [1:0] rk_0_raw_pos, rk_1_raw_pos, rk_2_raw_pos, rk_3_raw_pos;
logic [1:0] rd_0_waw_pos, rd_1_waw_pos, rd_2_waw_pos, rd_3_waw_pos;

logic_checker u_logic_checker(
    .clk(clk),
    .rst_n(rst_n),

    .instr_rd_index_vec('{rename_input_vec[0].reg_rd, rename_input_vec[1].reg_rd, 
                         rename_input_vec[2].reg_rd, rename_input_vec[3].reg_rd}),
    .instr_rj_index_vec('{rename_input_vec[0].reg_rj, rename_input_vec[1].reg_rj, 
                         rename_input_vec[2].reg_rj, rename_input_vec[3].reg_rj}),
    .instr_rk_index_vec('{rename_input_vec[0].reg_rk, rename_input_vec[1].reg_rk, 
                         rename_input_vec[2].reg_rk, rename_input_vec[3].reg_rk}),
    
    .instr_rd_exist_vec({rename_input_vec[3].reg_rd_exist, rename_input_vec[2].reg_rd_exist, 
                         rename_input_vec[1].reg_rd_exist, rename_input_vec[0].reg_rd_exist}),
    .instr_rj_exist_vec({rename_input_vec[3].reg_rj_exist, rename_input_vec[2].reg_rj_exist, 
                         rename_input_vec[1].reg_rj_exist, rename_input_vec[0].reg_rj_exist}),
    .instr_rk_exist_vec({rename_input_vec[3].reg_rk_exist, rename_input_vec[2].reg_rk_exist, 
                         rename_input_vec[1].reg_rk_exist, rename_input_vec[0].reg_rk_exist}),

    .rj_0_raw_pos(rj_0_raw_pos),
    .rj_1_raw_pos(rj_1_raw_pos),
    .rj_2_raw_pos(rj_2_raw_pos),
    .rj_3_raw_pos(rj_3_raw_pos),

    .rk_0_raw_pos(rk_0_raw_pos),
    .rk_1_raw_pos(rk_1_raw_pos),
    .rk_2_raw_pos(rk_2_raw_pos),
    .rk_3_raw_pos(rk_3_raw_pos),

    .rd_0_waw_pos(rd_0_waw_pos),
    .rd_1_waw_pos(rd_1_waw_pos),
    .rd_2_waw_pos(rd_2_waw_pos),
    .rd_3_waw_pos(rd_3_waw_pos)
);

// freelist allocate free preg index
logic [`PREG_INDEX_WIDTH-1:0] alloc_preg_index_vec [3:0];
logic freelist_empty;
logic freelist_full;
logic freelist_alloc_success;
logic [2:0] readout_num;

// arch rd=0始终映射到preg 0，不参与freelist分配
assign readout_num = (rename_input_vec[0].reg_rd_exist && (rename_input_vec[0].reg_rd != 5'h0)) + 
                     (rename_input_vec[1].reg_rd_exist && (rename_input_vec[1].reg_rd != 5'h0)) + 
                     (rename_input_vec[2].reg_rd_exist && (rename_input_vec[2].reg_rd != 5'h0)) + 
                     (rename_input_vec[3].reg_rd_exist && (rename_input_vec[3].reg_rd != 5'h0));

// freelist only needs to allocate free preg index
// specifically decide which instr needs to be renamed is done in the internal logic
freelist u_freelist(
    .clk(clk),
    .rst_n(rst_n),

    .retire_write_back_valid(retire_write_back_valid),
    .added_preg_vec(freed_preg_valid_vec),
    .add_preg_index_0(freed_preg_index_0),
    .add_preg_index_1(freed_preg_index_1),
    .add_preg_index_2(freed_preg_index_2),
    .add_preg_index_3(freed_preg_index_3),
    .readout_num(readout_num),

    .readout_preg_index_vec(alloc_preg_index_vec),
    .freelist_empty(freelist_empty),
    .freelist_full(freelist_full),
    .alloc_success(freelist_alloc_success)
);

// 计算需要分配的ROB索引数量
always_comb begin
    rob_index_num_req = rename_input_vec[0].valid + rename_input_vec[1].valid + 
                        rename_input_vec[2].valid + rename_input_vec[3].valid;
end

// rename_ready decided whether the freelist can allocate
always_comb begin
    // 检查freelist是否有足够的寄存器，以及下游模块是否准备好
    rename_ready = freelist_alloc_success && 
                   (readout_num <= 4) && // freelist能提供足够的寄存器
                   issue_ready && 
                   rob_ready;
end

// RAT
rat u_rat(
    .clk(clk),
    .rst_n(rst_n),

    .recover_valid(recover_valid),
    .recover_preg_index_vec(recover_preg_index_vec),

    .arch_rd_0_index(rename_input_vec[0].reg_rd),
    .arch_rd_1_index(rename_input_vec[1].reg_rd),
    .arch_rd_2_index(rename_input_vec[2].reg_rd),
    .arch_rd_3_index(rename_input_vec[3].reg_rd),

    .arch_rd_exist_vec({rename_input_vec[3].reg_rd_exist, rename_input_vec[2].reg_rd_exist, 
                        rename_input_vec[1].reg_rd_exist, rename_input_vec[0].reg_rd_exist}),

    .updating_valid_vec({rename_input_vec[3].reg_rd_exist, rename_input_vec[2].reg_rd_exist, 
                         rename_input_vec[1].reg_rd_exist, rename_input_vec[0].reg_rd_exist}),
    .added_preg_index_vec(alloc_preg_index_vec),

    .rd_history_preg_0(temp_rd_history_preg_0),
    .rd_history_preg_1(temp_rd_history_preg_1),
    .rd_history_preg_2(temp_rd_history_preg_2),
    .rd_history_preg_3(temp_rd_history_preg_3),

    .rj_matched_preg_0(temp_preg_rj_0),
    .rj_matched_preg_1(temp_preg_rj_1),
    .rj_matched_preg_2(temp_preg_rj_2),
    .rj_matched_preg_3(temp_preg_rj_3),

    .rk_matched_preg_0(temp_preg_rk_0),
    .rk_matched_preg_1(temp_preg_rk_1),
    .rk_matched_preg_2(temp_preg_rk_2),
    .rk_matched_preg_3(temp_preg_rk_3)
);

// temp_preg_rd_x selection logic
always_comb begin
    // arch rd=0始终映射到preg 0，不参与freelist分配
    temp_preg_rd_0 = (rename_input_vec[0].reg_rd_exist) ? ((rename_input_vec[0].reg_rd == 5'h0) ? 7'h0 : alloc_preg_index_vec[0]) : 0;
    temp_preg_rd_1 = (rename_input_vec[1].reg_rd_exist) ? ((rename_input_vec[1].reg_rd == 5'h0) ? 7'h0 : alloc_preg_index_vec[(rename_input_vec[0].reg_rd_exist && (rename_input_vec[0].reg_rd != 5'h0))]) : 0;
    temp_preg_rd_2 = (rename_input_vec[2].reg_rd_exist) ? ((rename_input_vec[2].reg_rd == 5'h0) ? 7'h0 : alloc_preg_index_vec[(rename_input_vec[0].reg_rd_exist && (rename_input_vec[0].reg_rd != 5'h0)) + (rename_input_vec[1].reg_rd_exist && (rename_input_vec[1].reg_rd != 5'h0))]) : 0;
    temp_preg_rd_3 = (rename_input_vec[3].reg_rd_exist) ? ((rename_input_vec[3].reg_rd == 5'h0) ? 7'h0 : alloc_preg_index_vec[(rename_input_vec[0].reg_rd_exist && (rename_input_vec[0].reg_rd != 5'h0)) + (rename_input_vec[1].reg_rd_exist && (rename_input_vec[1].reg_rd != 5'h0)) + (rename_input_vec[2].reg_rd_exist && (rename_input_vec[2].reg_rd != 5'h0))]) : 0;
end

// temp_preg_rj_x selection logic
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_1_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_2_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_3_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_1_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_2_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_3_sel;

always_comb begin
    // Default values from RAT
    temp_preg_rj_1_sel = temp_preg_rj_1;  // Keep RAT value
    temp_preg_rj_2_sel = temp_preg_rj_2;  // Keep RAT value
    temp_preg_rj_3_sel = temp_preg_rj_3;  // Keep RAT value
    
    // Override with forwarding logic
    if(rj_1_raw_pos == 0) begin
        temp_preg_rj_1_sel = temp_preg_rd_0;
    end

    case(rj_2_raw_pos)
        0: temp_preg_rj_2_sel = temp_preg_rd_0;
        1: temp_preg_rj_2_sel = temp_preg_rd_1;
        default: temp_preg_rj_2_sel = temp_preg_rj_2; // keep temp_preg_rj_2 value from RAT
    endcase

    case(rj_3_raw_pos)
        0: temp_preg_rj_3_sel = temp_preg_rd_0;
        1: temp_preg_rj_3_sel = temp_preg_rd_1;
        2: temp_preg_rj_3_sel = temp_preg_rd_2;
        default: temp_preg_rj_3_sel = temp_preg_rj_3; // keep temp_preg_rj_3 value from RAT
    endcase
end

// temp_preg_rk_x selection logic
always_comb begin
    // Default values from RAT
    temp_preg_rk_1_sel = temp_preg_rk_1;  // Keep RAT value
    temp_preg_rk_2_sel = temp_preg_rk_2;  // Keep RAT value
    temp_preg_rk_3_sel = temp_preg_rk_3;  // Keep RAT value
    
    // Override with forwarding logic
    if(rk_1_raw_pos == 0) begin
        temp_preg_rk_1_sel = temp_preg_rd_0;
    end

    case(rk_2_raw_pos)
        0: temp_preg_rk_2_sel = temp_preg_rd_0;
        1: temp_preg_rk_2_sel = temp_preg_rd_1;
        default: temp_preg_rk_2_sel = temp_preg_rk_2; // keep temp_preg_rk_2 value from RAT
    endcase

    case(rk_3_raw_pos)
        0: temp_preg_rk_3_sel = temp_preg_rd_0;
        1: temp_preg_rk_3_sel = temp_preg_rd_1;
        2: temp_preg_rk_3_sel = temp_preg_rd_2;
        default: temp_preg_rk_3_sel = temp_preg_rk_3; // keep temp_preg_rk_3 value from RAT
    endcase
end

// 和忙表交互
logic [3:0] rd_busy_table_readout_valid_vec,
logic [3:0] rj_busy_table_readout_valid_vec,
logic [3:0] rk_busy_table_readout_valid_vec;

busy_table busy_table(
    .clk(aclk),
    .rst_n(rst_n),

    .preg_rj_index_vec(rj_busy_table_read_index_vec),
    .preg_rk_index_vec(rk_busy_table_read_index_vec),
    .preg_rd_index_vec(rd_busy_table_read_index_vec),
    .rj_ready_vec(rj_busy_table_readout_valid_vec),
    .rk_ready_vec(rk_busy_table_readout_valid_vec),
    .rd_ready_vec(rd_busy_table_readout_valid_vec),

    .begin_exec_rd_index_vec(begin_exec_rd_index_vec),
    .begin_exec_valid_vec(begin_exec_valid_vec),
    .wb_rd_index_vec(wb_rd_index_vec),
    .wb_valid_vec(wb_valid_vec),
    .recover_valid(recover_valid),
    .recover_busy_table(recover_busy_table)
);

always_comb begin
    // arch rd=0始终映射到preg 0
    rd_busy_table_read_index_vec[0] = (rename_input_vec[0].reg_rd_exist) ? temp_preg_rd_0 : 0;
    rd_busy_table_read_index_vec[1] = (rename_input_vec[1].reg_rd_exist) ? temp_preg_rd_1 : 0;
    rd_busy_table_read_index_vec[2] = (rename_input_vec[2].reg_rd_exist) ? temp_preg_rd_2 : 0;
    rd_busy_table_read_index_vec[3] = (rename_input_vec[3].reg_rd_exist) ? temp_preg_rd_3 : 0;

    rj_busy_table_read_index_vec[0] = (rename_input_vec[0].reg_rj_exist) ? temp_preg_rj_0 : 0;
    rj_busy_table_read_index_vec[1] = (rename_input_vec[1].reg_rj_exist) ? temp_preg_rj_1_sel : 0;
    rj_busy_table_read_index_vec[2] = (rename_input_vec[2].reg_rj_exist) ? temp_preg_rj_2_sel : 0;
    rj_busy_table_read_index_vec[3] = (rename_input_vec[3].reg_rj_exist) ? temp_preg_rj_3_sel : 0;

    rk_busy_table_read_index_vec[0] = (rename_input_vec[0].reg_rk_exist) ? temp_preg_rk_0 : 0;
    rk_busy_table_read_index_vec[1] = (rename_input_vec[1].reg_rk_exist) ? temp_preg_rk_1_sel : 0;
    rk_busy_table_read_index_vec[2] = (rename_input_vec[2].reg_rk_exist) ? temp_preg_rk_2_sel : 0;
    rk_busy_table_read_index_vec[3] = (rename_input_vec[3].reg_rk_exist) ? temp_preg_rk_3_sel : 0;
end

// 组装临时结构体数组
logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_index_0, rob_index_1, rob_index_2, rob_index_3;
always_comb begin
    // 计算每个指令的PC
    logic [31:0] pc_0, pc_1, pc_2, pc_3;
    pc_0 = start_pc_in;
    pc_1 = start_pc_in + 4;
    pc_2 = start_pc_in + 8;
    pc_3 = start_pc_in + 12;
    
    // 计算ROB索引
    rob_index_0 = rob_index_start;
    rob_index_1 = rob_index_start + rename_input_vec[0].valid;
    rob_index_2 = rob_index_start + rename_input_vec[0].valid + rename_input_vec[1].valid;
    rob_index_3 = rob_index_start + rename_input_vec[0].valid + rename_input_vec[1].valid + rename_input_vec[2].valid;

    // 指令0
    temp_rename_output_vec[0].pc = pc_0;
    temp_rename_output_vec[0].valid = rename_input_vec[0].valid;
    temp_rename_output_vec[0].gen_op_type = rename_input_vec[0].gen_op_type;
    temp_rename_output_vec[0].spec_op_type = rename_input_vec[0].spec_op_type;
    temp_rename_output_vec[0].exception = rename_input_vec[0].exception;
    temp_rename_output_vec[0].imm = rename_input_vec[0].imm;
    temp_rename_output_vec[0].imm_enable = rename_input_vec[0].imm_enable;
    temp_rename_output_vec[0].imm_sign_extend = rename_input_vec[0].imm_sign_extend;
    temp_rename_output_vec[0].rd_arch_index = rename_input_vec[0].reg_rd;
    temp_rename_output_vec[0].rd_exist = rename_input_vec[0].reg_rd_exist;
    temp_rename_output_vec[0].preg_rd_ready = rd_busy_table_readout_valid_vec[0]; // 从忙表读取
    temp_rename_output_vec[0].preg_rd = temp_preg_rd_0;
    temp_rename_output_vec[0].preg_rj = temp_preg_rj_0;
    temp_rename_output_vec[0].rj_exist = rename_input_vec[0].reg_rj_exist;
    temp_rename_output_vec[0].preg_rj_ready = rj_busy_table_readout_valid_vec[0]; // 从忙表读取
    temp_rename_output_vec[0].preg_rk = temp_preg_rk_0;
    temp_rename_output_vec[0].rk_exist = rename_input_vec[0].reg_rk_exist;
    temp_rename_output_vec[0].preg_rk_ready = rk_busy_table_readout_valid_vec[0]; // 从忙表读取
    temp_rename_output_vec[0].rd_history_preg = temp_rd_history_preg_0;
    temp_rename_output_vec[0].pred_jump_target_pc = rename_input_vec[0].pred_jump_target_pc;
    temp_rename_output_vec[0].issue_distr_direction = rename_input_vec[0].issue_distr_direction;
    temp_rename_output_vec[0].store_or_load = rename_input_vec[0].store_or_load;
    temp_rename_output_vec[0].bar_type = rename_input_vec[0].bar_type;
    temp_rename_output_vec[0].completion_bit = rename_input_vec[0].completion_bit;

    // 指令1
    temp_rename_output_vec[1].pc = pc_1;
    temp_rename_output_vec[1].valid = rename_input_vec[1].valid;
    temp_rename_output_vec[1].gen_op_type = rename_input_vec[1].gen_op_type;
    temp_rename_output_vec[1].spec_op_type = rename_input_vec[1].spec_op_type;
    temp_rename_output_vec[1].exception = rename_input_vec[1].exception;
    temp_rename_output_vec[1].imm = rename_input_vec[1].imm;
    temp_rename_output_vec[1].imm_enable = rename_input_vec[1].imm_enable;
    temp_rename_output_vec[1].imm_sign_extend = rename_input_vec[1].imm_sign_extend;
    temp_rename_output_vec[1].rd_arch_index = rename_input_vec[1].reg_rd;
    temp_rename_output_vec[1].rd_exist = rename_input_vec[1].reg_rd_exist;
    temp_rename_output_vec[1].preg_rd_ready = rd_busy_table_readout_valid_vec[1]; // 从忙表读取
    temp_rename_output_vec[1].preg_rd = temp_preg_rd_1;
    temp_rename_output_vec[1].preg_rj = temp_preg_rj_1_sel;
    temp_rename_output_vec[1].rj_exist = rename_input_vec[1].reg_rj_exist;
    temp_rename_output_vec[1].preg_rj_ready = rj_busy_table_readout_valid_vec[1]; // 从忙表读取
    temp_rename_output_vec[1].preg_rk = temp_preg_rk_1_sel;
    temp_rename_output_vec[1].rk_exist = rename_input_vec[1].reg_rk_exist;
    temp_rename_output_vec[1].preg_rk_ready = rk_busy_table_readout_valid_vec[1]; // 从忙表读取
    temp_rename_output_vec[1].rd_history_preg = temp_rd_history_preg_1;
    temp_rename_output_vec[1].pred_jump_target_pc = rename_input_vec[1].pred_jump_target_pc;
    temp_rename_output_vec[1].issue_distr_direction = rename_input_vec[1].issue_distr_direction;
    temp_rename_output_vec[1].store_or_load = rename_input_vec[1].store_or_load;
    temp_rename_output_vec[1].bar_type = rename_input_vec[1].bar_type;
    temp_rename_output_vec[1].completion_bit = rename_input_vec[1].completion_bit;

    // 指令2
    temp_rename_output_vec[2].pc = pc_2;
    temp_rename_output_vec[2].valid = rename_input_vec[2].valid;
    temp_rename_output_vec[2].gen_op_type = rename_input_vec[2].gen_op_type;
    temp_rename_output_vec[2].spec_op_type = rename_input_vec[2].spec_op_type;
    temp_rename_output_vec[2].exception = rename_input_vec[2].exception;
    temp_rename_output_vec[2].imm = rename_input_vec[2].imm;
    temp_rename_output_vec[2].imm_enable = rename_input_vec[2].imm_enable;
    temp_rename_output_vec[2].imm_sign_extend = rename_input_vec[2].imm_sign_extend;
    temp_rename_output_vec[2].rd_arch_index = rename_input_vec[2].reg_rd;
    temp_rename_output_vec[2].rd_exist = rename_input_vec[2].reg_rd_exist;
    temp_rename_output_vec[2].preg_rd_ready = rd_busy_table_readout_valid_vec[2]; // 从忙表读取
    temp_rename_output_vec[2].preg_rd = temp_preg_rd_2;
    temp_rename_output_vec[2].preg_rj = temp_preg_rj_2_sel;
    temp_rename_output_vec[2].rj_exist = rename_input_vec[2].reg_rj_exist;
    temp_rename_output_vec[2].preg_rj_ready = rj_busy_table_readout_valid_vec[2]; // 从忙表读取
    temp_rename_output_vec[2].preg_rk = temp_preg_rk_2_sel;
    temp_rename_output_vec[2].rk_exist = rename_input_vec[2].reg_rk_exist;
    temp_rename_output_vec[2].preg_rk_ready = rk_busy_table_readout_valid_vec[2]; // 从忙表读取
    temp_rename_output_vec[2].rd_history_preg = temp_rd_history_preg_2;
    temp_rename_output_vec[2].pred_jump_target_pc = rename_input_vec[2].pred_jump_target_pc;
    temp_rename_output_vec[2].issue_distr_direction = rename_input_vec[2].issue_distr_direction;
    temp_rename_output_vec[2].store_or_load = rename_input_vec[2].store_or_load;
    temp_rename_output_vec[2].bar_type = rename_input_vec[2].bar_type;
    temp_rename_output_vec[2].completion_bit = rename_input_vec[2].completion_bit;

    // 指令3
    temp_rename_output_vec[3].pc = pc_3;
    temp_rename_output_vec[3].valid = rename_input_vec[3].valid;
    temp_rename_output_vec[3].gen_op_type = rename_input_vec[3].gen_op_type;
    temp_rename_output_vec[3].spec_op_type = rename_input_vec[3].spec_op_type;
    temp_rename_output_vec[3].exception = rename_input_vec[3].exception;
    temp_rename_output_vec[3].imm = rename_input_vec[3].imm;
    temp_rename_output_vec[3].imm_enable = rename_input_vec[3].imm_enable;
    temp_rename_output_vec[3].imm_sign_extend = rename_input_vec[3].imm_sign_extend;
    temp_rename_output_vec[3].rd_arch_index = rename_input_vec[3].reg_rd;
    temp_rename_output_vec[3].rd_exist = rename_input_vec[3].reg_rd_exist;
    temp_rename_output_vec[3].preg_rd_ready = rd_busy_table_readout_valid_vec[3]; // 从忙表读取
    temp_rename_output_vec[3].preg_rd = temp_preg_rd_3;
    temp_rename_output_vec[3].preg_rj = temp_preg_rj_3_sel;
    temp_rename_output_vec[3].rj_exist = rename_input_vec[3].reg_rj_exist;
    temp_rename_output_vec[3].preg_rj_ready = rj_busy_table_readout_valid_vec[3]; // 从忙表读取
    temp_rename_output_vec[3].preg_rk = temp_preg_rk_3_sel;
    temp_rename_output_vec[3].rk_exist = rename_input_vec[3].reg_rk_exist;
    temp_rename_output_vec[3].preg_rk_ready = rk_busy_table_readout_valid_vec[3]; // 从忙表读取
    temp_rename_output_vec[3].rd_history_preg = temp_rd_history_preg_3;
    temp_rename_output_vec[3].pred_jump_target_pc = rename_input_vec[3].pred_jump_target_pc;
    temp_rename_output_vec[3].issue_distr_direction = rename_input_vec[3].issue_distr_direction;
    temp_rename_output_vec[3].store_or_load = rename_input_vec[3].store_or_load;
    temp_rename_output_vec[3].bar_type = rename_input_vec[3].bar_type;
    temp_rename_output_vec[3].completion_bit = rename_input_vec[3].completion_bit;
end

// 输出结构体数组寄存器
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i = 0; i < 4; i++) begin
            rename_output_vec[i] <= '0;
        end
        rename_valid <= 1'b0;
    end else begin
        if(!rob_ready || !freelist_alloc_success || !issue_ready) begin
            // 保持当前值
            rename_output_vec[0] <= rename_output_vec[0];
            rename_output_vec[1] <= rename_output_vec[1];
            rename_output_vec[2] <= rename_output_vec[2];
            rename_output_vec[3] <= rename_output_vec[3];
            rename_valid <= rename_valid;
        end else begin
            // ready to send out
            rename_output_vec[0] <= temp_rename_output_vec[0];
            rename_output_vec[1] <= temp_rename_output_vec[1];
            rename_output_vec[2] <= temp_rename_output_vec[2];
            rename_output_vec[3] <= temp_rename_output_vec[3];
            rename_valid <= decoder_valid;
        end
    end
end
endmodule