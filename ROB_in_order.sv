`timescale 1ns / 1ps
`include "mycpu.h"

module ROB_in_order (
    input clk,
    input rst_n,
    input flush,
    input stall,
    input logic [2:0] write_in_num, // 表示每个周期写入的指令数量 (指令是顺序排列的)
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index_start, // the start index of incoming instr in the rob entry
    output logic alloc_success,
    // interface with rename
    input rob_single_instr_info_t rob_instr_info [3:0], // 指令信息结构体数组

    // interface with issue and FU
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] complete_entry_index_vec [3:0], // used to set corresponding entry completion bit to 1
    input logic [3:0] complete_valid_vec,
    input logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] fu_exception_vec [3:0], // 从FU返回的exception
    input logic [31:0] fu_jump_pc_vec [3:0], // 从FU返回的跳转PC(如果是分支指令完成了，就要写回具体的跳转地址)
    input logic [3:0] fu_jump_pc_pred_wrong, // 如果预测错误了，就要在退休的时候跳转保存的PC
    // 没有例外，返回值就是0
    // input logic FU_exception_valid_vec [3:0], // 从FU返回的exception是否有效 (感觉可以不需要？)
    
    // interface with LSU (前面的completion也包括了LSU，这里是额外的Store Buffer的控制信号)
    // because only one store instr retires, 才能写D-Cache
    output logic store_retire_valid, // 从Store Buffer中pop out 第一条写到D-Cache中
    // 每个周期最多只会退休一条Store指令

    // Exception interface and CSR interface
    output logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] rob_entry_exception,
    // 每个周期最多退休一条有exception的指令

    // aRAT interface
    output logic retire_valid, // 正常退休，更新aRAT和freelist
    output logic [4:0] aRAT_write_arch_rd_index_vec [3:0],
    output logic [`PREG_INDEX_WIDTH-1:0] recover_aRAT_write_preg_index_vec [3:0],
    output logic [3:0] write_preg_valid_vec, // exactly equal to rd_exist_vec
    // used both for aRAT and freelist

    // arch freelist and speculative freelist interface
    output logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_vec [3:0],
    
    // BPU interface // 每个周期最多退休一条分支指令
    output logic [31:0] output_instr_pc_for_bpu_update, // 可能后面会换成BPU里 table的 index？
    output logic output_bj_type_for_bpu_update,  
    // 0 表示普通分支指令, 1 表示B指令(相当于直接跳转) , 2 表示 BL (相当于JAL) , 3 表示 普通非调用间接跳转指令 , 4 表示 函数返回指令(JR $ra类似于)
    output logic BJ_retire_valid,
    output logic [31:0] real_jump_pc, // 因为把分支指令的跳转放在了retire阶段，所以需要保存这个值
    output logic pred_wrong_signal, // 如果分支预测失败，则需要这个信号，从而从指定PC开始重新执行
    // CSR interface
    // not know yet
    output logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] CSR_exception,
    output logic [31:0] CSR_exception_pc,
    output logic CSR_exception_valid,

    // debug
    input           break_point, // 后面疑似暂时没用上，先放在这里
    // input           infor_flag, // 指示CPU把 reg_num 对应的物理寄存器内容写到 rf_data端口
    // input  [ 4:0]   reg_num,  // 选择要观察的arch 寄存器号
    output          ws_valid, // 真正提交了一条指令时把这个信号拉高一个周期，表示其他的debug采样信号也是正常的
    // output [31:0]   rf_rdata, // 要观察的arch寄存器实际的内容

    output [31:0] debug0_wb_pc, // 提交的指令的PC
    output [ 3:0] debug0_wb_rf_wen, // 写回数据的字节数，默认都是4'b1111，写回一个32bit数
    output [`PREG_INDEX_WIDTH-1:0] temp_debug0_preg_rd_index, // 直接输出给PRF，然后PRF直接读出来 
    output [ 4:0] debug0_wb_rf_wnum, // 提交指令要写回的arch rd号
    // output [31:0] debug0_wb_rf_wdata, // 提交写回的数据，本质上就是把物理寄存器里面的值读出来
    // 上面这个wdata要直接从prf中读取，从这里的
    output [31:0] debug0_wb_inst // 提交指令的机器码，用于debug
);

mainbody_entry_t rob_mainbody [`ROB_DEPTH-1:0];
logic [31:0] rob_pc [`ROB_DEPTH-1:0]; // 专门用来保存每个entry的pc
BJ_jump_entry_t BJ_jump_entry [`ROB_DEPTH-1:0]; // 专门用来保存每个每个分支指令的跳转PC以及跳转预测是否正确

logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_head;
logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_tail;
logic [`ROB_ENTRY_INDEX_WIDTH:0] filled_cnt; // 用来记录当前rob中已经有多少个entry (0~128)

// 每个周期最多只能退休一条分支指令，最多只能退休一个store指令，因此要判断一下
logic [3:0] branch_vec;
logic [3:0] store_vec;
logic [3:0] exception_mask; // 1表示正常，0表示这一位是例外
logic [3:0] sec_branch_mask; // 1表示正常，0表示这一位是至少第二条分支指令
logic [3:0] sec_store_mask; // 1表示正常，0表示这一位是至少第二条store指令
logic [3:0] sec_exception_mask; // 1表示正常，0表示这一位是例外
logic [3:0] ready_mask; // 1表示ready，0表示not ready
logic [3:0] retire_mask; // 1表示可以退休，0表示不能退休
logic [2:0] retire_num; // 表示可以退休的指令数量

assign retire_num = (rob_mainbody[rob_head].completion)? 1 : 0;

// 这一块需要完全的改变，原本的for + break语法是完全不能综合的
// 现在需要的是对前四条指令进行并行判断，得到一些 logic bits
// 从而使用这些bits进一步的进行判断
// 首先要判断每条指令是否完成
assign ready_mask = {
    rob_mainbody[rob_head].ready,
    rob_mainbody[rob_head + 1].ready,
    rob_mainbody[rob_head + 2].ready,
    rob_mainbody[rob_head + 3].ready
};
// 然后是判断分支指令 vec
assign branch_vec = {
    rob_mainbody[rob_head].gen_op_type == `GENERAL_OPTYPE_BJ,
    rob_mainbody[rob_head + 1].gen_op_type == `GENERAL_OPTYPE_BJ,
    rob_mainbody[rob_head + 2].gen_op_type == `GENERAL_OPTYPE_BJ,
    rob_mainbody[rob_head + 3].gen_op_type == `GENERAL_OPTYPE_BJ
};
// 然后是判断store指令 vec
assign store_vec = {
    rob_mainbody[rob_head].gen_op_type == `GENERAL_OPTYPE_2R12I && rob_mainbody[rob_head].store_or_load,
    rob_mainbody[rob_head + 1].gen_op_type == `GENERAL_OPTYPE_2R12I && rob_mainbody[rob_head + 1].store_or_load,
    rob_mainbody[rob_head + 2].gen_op_type == `GENERAL_OPTYPE_2R12I && rob_mainbody[rob_head + 2].store_or_load,
    rob_mainbody[rob_head + 3].gen_op_type == `GENERAL_OPTYPE_2R12I && rob_mainbody[rob_head + 3].store_or_load
};
// 然后是判断exception vec
assign exception_mask = {
    rob_mainbody[rob_head].exception != 0,
    rob_mainbody[rob_head + 1].exception != 0,
    rob_mainbody[rob_head + 2].exception != 0,
    rob_mainbody[rob_head + 3].exception != 0
}; // 只有exceptin = 0才是无异常指令

// 最后是根据一些bits进一步决定 sec_branch_mask和sec_store_mask
// 以及sec_exception_mask
always_comb begin
    // 如果某一位是异常，那么其后面全部要置0
    casez(exception_mask)
        4'b0000: sec_exception_mask = 4'b1111;
        4'b???1: sec_exception_mask = 4'b0001;
        4'b??10: sec_exception_mask = 4'b0011;
        4'b?100: sec_exception_mask = 4'b0111;
        4'b1000: sec_exception_mask = 4'b1111;
        default: sec_exception_mask = 4'b1111;
    endcase
end

always_comb begin
    sec_branch_mask = 4'b1111;
    case (branch_vec)
        // 这是没有分支指令
        4'b0000: sec_branch_mask = 4'b1111;
        // 这是只有一条分支指令
        4'b0001: sec_branch_mask = 4'b1111;
        4'b0010: sec_branch_mask = 4'b1111;
        4'b0100: sec_branch_mask = 4'b1111;
        4'b1000: sec_branch_mask = 4'b1111;
        // 下面是有两条分支指令，第二条分支指令及之后的位置0
        4'b0011: sec_branch_mask = 4'b0001;
        4'b0101: sec_branch_mask = 4'b0011;
        4'b1001: sec_branch_mask = 4'b0111;
        4'b0110: sec_branch_mask = 4'b0011;
        4'b1010: sec_branch_mask = 4'b0111;
        4'b1100: sec_branch_mask = 4'b0111;
        // 下面是三条分支指令,第二条及之后的位置为0
        4'b0111: sec_branch_mask = 4'b0001;
        4'b1011: sec_branch_mask = 4'b0001;
        4'b1101: sec_branch_mask = 4'b0011;
        4'b1110: sec_branch_mask = 4'b0011;
        // 下面是四条分支指令,第二条及之后的位置为0
        4'b1111: sec_branch_mask = 4'b0001;
    endcase
end
always_comb begin // 这是store_mask
    sec_store_mask = 4'b1111;
    case (store_vec)
        // 这是没有store指令
        4'b0000: sec_store_mask = 4'b1111;
        // 这是只有一条store指令
        4'b0001: sec_store_mask = 4'b1111;
        4'b0010: sec_store_mask = 4'b1111;
        4'b0100: sec_store_mask = 4'b1111;
        4'b1000: sec_store_mask = 4'b1111;
        // 下面是有两条store指令，第二条store指令及之后的位置为0
        4'b0011: sec_store_mask = 4'b0001;
        4'b0101: sec_store_mask = 4'b0011;
        4'b1001: sec_store_mask = 4'b0111;
        4'b0110: sec_store_mask = 4'b0011;
        4'b1010: sec_store_mask = 4'b0111;
        4'b1100: sec_store_mask = 4'b0111;
        // 下面是三条store指令,第二条及之后的位置为0
        4'b0111: sec_store_mask = 4'b0001;
        4'b1011: sec_store_mask = 4'b0001;
        4'b1101: sec_store_mask = 4'b0011;
        4'b1110: sec_store_mask = 4'b0011;
        // 下面是四条store指令,第二条及之后的位置为0
        4'b1111: sec_store_mask = 4'b0001;
    endcase
end

// 下面根据一部分mask最终决定retire_mask和retire_num
assign retire_mask = ready_mask & sec_branch_mask & sec_store_mask & sec_exception_mask;
assign retire_num = retire_mask[0] + retire_mask[1] + retire_mask[2] + retire_mask[3];

// 然后是每个周期的写入，首先要根据写入的指令数量进行是否能分配空间的判断
always_comb begin
    if(write_in_num + filled_cnt > `ROB_DEPTH) begin
        alloc_success = 1'b0;
    end else begin
        alloc_success = 1'b1;
    end
end

// 然后是写入，同时退休
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        rob_head <= 0;
        rob_tail <= 0;
        filled_cnt <= 0;
        for(int i=0; i<`ROB_DEPTH; i=i+1) begin
            rob_mainbody[i] <= 0;
            rob_pc[i] <= 0;
            BJ_jump_entry[i]<= 0;
        end
    end else begin
        if(alloc_success) begin
            for(int i=0; i<4; i=i+1) begin
                if(i < write_in_num) begin
                    // 写入
                    rob_pc[rob_tail + i] <= rob_instr_info.instr[i].pc;
                    rob_mainbody[rob_tail + i].exception <= rob_instr_info.instr[i].exception;
                    rob_mainbody[rob_tail + i].completion <= rob_instr_info.instr[i].completion;
                    rob_mainbody[rob_tail + i].arch_rd_index <= rob_instr_info.instr[i].arch_rd_index;
                    rob_mainbody[rob_tail + i].preg_rd_index <= rob_instr_info.instr[i].preg_rd_index;
                    rob_mainbody[rob_tail + i].rd_history_index <= rob_instr_info.instr[i].rd_history_index;
                    rob_mainbody[rob_tail + i].gen_op_type <= rob_instr_info.instr[i].gen_op_type;
                    rob_mainbody[rob_tail + i].spec_op_type <= rob_instr_info.instr[i].spec_op_type;
                    rob_mainbody[rob_tail + i].store_or_load <= rob_instr_info.instr[i].store_or_load;
                    rob_mainbody[rob_tail + i].rd_exist <= rob_instr_info.instr[i].rd_exist;
                    BJ_jump_entry[rob_tail + i].BJ_type <= rob_instr_info.instr[i].pred_BJ_type;
                    BJ_jump_entry[rob_tail + i].real_jump_pc <= rob_instr_info.instr[i].pred_BJ_jump_pc;
                    rob_mainbody[rob_tail + i].instr <= rob_instr_info.instr[i].instr;
                end
            end
            // 退休
            // 默认赋值，防止一些关键信号没有被复位
            recover_valid <= 1'b0; // 防止一直写aRAT回RAT
            pred_wrong_signal <= 1'b0; // 防止一直重新取指
            BJ_retire_valid <= 1'b0; // 防止一直更新BPU
            store_write_dcache_valid <= 1'b0; // 防止一直写dcache

            for(int i=0; i<4; i=i+1) begin
                if(i < retire_num) begin
                    // 提交指令，需要从rob_mainbody中取出指令信息
                    debug0_wb_pc <= rob_mainbody[rob_head + i].pc;
                    debug0_wb_rf_wen <= 4'b1111;
                    debug0_wb_rf_wnum <= rob_mainbody[rob_head + i].arch_rd_index;
                    debug0_wb_inst <= rob_mainbody[rob_head + i].instr;

                    // 所有指令都需要对aRAT和freelist进行更新
                    write_preg_valid_vec[i] <= rob_mainbody[rob_head + i].rd_exist;
                    // aRAT_write_arch_rd_index_vec[i] <= (rob_mainbody[rob_head + i].rd_exist) ? rob_mainbody[rob_head + i].arch_rd_index : 5'b0;
                    // aRAT_write_preg_index_vec[i] <= (rob_mainbody[rob_head + i].rd_exist) ? rob_mainbody[rob_head + i].preg_rd_index : `PREG_INDEX_WIDTH'b0;
                    aRAT_write_arch_rd_index_vec[i] <= rob_mainbody[rob_head + i].arch_rd_index;
                    aRAT_write_preg_index_vec[i] <= rob_mainbody[rob_head + i].preg_rd_index;
                    // 退休之后要保证completion_bit被reset为0
                    rob_mainbody[rob_head + i].completion <= 0;
                    
                    // 然后是freelist的更新
                    // freed_preg_index_vec[i] <= (rob_mainbody[rob_head + i].rd_exist) ? rob_mainbody[rob_head + i].preg_rd_index : `PREG_INDEX_WIDTH'b0;
                    freed_preg_index_vec[i] <= rob_mainbody[rob_head + i].rd_history_index;
                    
                    if(rob_mainbody[rob_head + i].exception == 1) begin
                        // 0 代表没意外 ， 1表示分支跳转失败
                        // 分支预测失败，需要从BJ_jump_entry中取出正确的跳转地址
                        recover_valid <= 1'b1;
                        real_jump_pc <= BJ_jump_entry[rob_head + i].real_jump_pc; // 要跳到真正的跳转地址
                        pred_wrong_signal <= 1'b1; // 预测失败，需要重新取指
                        output_instr_pc_for_bpu_update <= rob_pc[rob_head + i]; // 用于更新BPU table
                        output_bj_type_for_bpu_update <= BJ_jump_entry[rob_head + i].BJ_type;

                    end else if (rob_mainbody[rob_head + i].exception > 1) begin
                        // 有例外，如果有例外，其后面肯定就没有指令要退休了
                        // 发射给CSR unit统一进行处理
                        CSR_exception <= rob_mainbody[rob_head + i].exception;
                        CSR_exception_pc <= rob_pc[rob_head + i];
                        CSR_exception_valid <= 1'b1;
                        // 可能还有其他的需要处理?
                    end else begin // 没有例外                
                        if(rob_mainbody[rob_head + i].gen_op_type == `GENERAL_OPTYPE_BJ) begin
                            // 分支指令
                            BJ_retire_valid <= 1'b1;
                            output_instr_pc_for_bpu_update <= rob_pc[rob_head + i]; // 用于更新BPU table
                            output_bj_type_for_bpu_update <= BJ_jump_entry[rob_head + i].BJ_type;
                            // 没有例外，说明没有出现预测失败，不需要刷掉流水线和重新取指
                        end else if (rob_mainbody[rob_head + i].gen_op_type == `GENERAL_OPTYPE_2R12I) begin
                            // some store instr
                            case(rob_mainbody[rob_head + i].spec_op_type)
                                `_2R12I_ST_B: store_write_dcache_valid <= 1'b1;
                                `_2R12I_ST_H: store_write_dcache_valid <= 1'b1;
                                `_2R12I_ST_W: store_write_dcache_valid <= 1'b1;
                                default: store_write_dcache_valid <= 1'b0;
                            endcase
                        end else if (rob_mainbody[rob_head + i].gen_op_type == `GENERAL_OPTYPE_ATOMIC && rob_mainbody[rob_head + i].store_or_load) begin
                            // SC instr
                            store_write_dcache_valid <= 1'b1;
                        end
                    end
                end
            end
            // 最后统一更新rob_head和rob_tail，以及filled_cnt
            rob_head <= rob_head + retire_num;
            rob_tail <= rob_tail + write_in_num;
            filled_cnt <= filled_cnt + write_in_num - retire_num;
        end
    end
end

// 同时还在进行completion status (包括exception) 的写回
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        rob_mainbody[`ROB_DEPTH-1:0] <= 0; // can i do nothing in rst block?
    end else begin
        for(int i=0; i<4; i=i+1) begin
            if(complete_valid_vec[i]) begin
                rob_mainbody[complete_entry_index_vec[i]].completion <= 1'b1;
                // 如果预测错误就要置exception为1
                if(FU_jump_pc_pred_wrong[i]) begin
                    rob_mainbody[complete_entry_index_vec[i]].exception <= 1; // 分支跳转失败"例外"
                    BJ_jump_entry[complete_entry_index_vec[i]].real_jump_pc <= FU_jump_pc_vec[i];
                end else begin
                    rob_mainbody[complete_entry_index_vec[i]].exception <= FU_exception_vec[i]; // 没有例外
                end
            end
        end
    end
end
endmodule