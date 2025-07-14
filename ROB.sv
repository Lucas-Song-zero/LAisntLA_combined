`timescale 1ns / 1ps
`include "mycpu.h"

module ROB (
    input clk,
    input rst_n,
    input flush,
    input stall,
    input logic [2:0] write_in_num, // 表示每个周期写入的指令数量 (指令是顺序排列的)
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index_start, // the start index of incoming instr in the rob entry
    output logic alloc_success,
    // interface with rename
    input rob_instr_info_t rob_instr_info, // 指令信息结构体数组

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
    output logic CSR_exception_valid
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
logic [3:0] sec_branch_mask; // 1表示正常，0表示这一位是至少第二条分支指令
logic [3:0] sec_store_mask; // 1表示正常，0表示这一位是至少第二条store指令
logic [3:0] exception_mask; // 1表示正常，0表示这一位是例外
logic [3:0] ready_mask; // 1表示ready，0表示not ready
logic [3:0] retire_mask; // 1表示可以退休，0表示不能退休
logic [2:0] retire_num; // 表示可以退休的指令数量

always_comb begin
    // 判断前四个entry是否是分支指令 ; 是否是store指令
    branch_vec = 4'b0;
    store_vec = 4'b0;
    sec_branch_mask = 4'b1111;
    sec_store_mask = 4'b1111;
    exception_mask = 4'b1111;
    ready_mask = 4'b1111;
    // retire_mask = 4'b0;

    for(integer i=0; i<4; i=i+1) begin
        if(rob_mainbody[rob_head + i].gen_op_type == `GENERAL_OPTYPE_BJ) begin
            branch_vec[i] = 1'b1;
        end
        if(rob_mainbody[rob_head + i].gen_op_type == `GENERAL_OPTYPE_STORE && rob_mainbody[rob_head + i].store_or_load) begin
            store_vec[i] = 1'b1;
        end
    end

    // 判断这前四个entry中是否有第二条分支指令和第二条store指令
    for(int i=0; i<4; i=i+1) begin
        if(branch_vec[i]) begin // 这一位是分支指令，后面找第二条分支指令
            for(int j=i+1; j<4; j=j+1) begin
                if(branch_vec[j]) begin
                    for(int k=j; k<4; k=k+1) begin
                        sec_branch_mask[k] = 1'b0;
                    end
                    break;
                end
            end
        end
        if(store_vec[i]) begin // 这一位是store指令，后面找第二条store指令
            for(int j=i+1; j<4; j=j+1) begin
                if(store_vec[j]) begin
                    for(int k=j; k<4; k=k+1) begin
                        sec_store_mask[k] = 1'b0;
                    end
                    break;
                end
            end
        end
    end

    // 以及还要根据前面四个指令中是否存在例外来进行处理
    // 如果存在例外，存在例外的指令之后的所有指令都不能退休
    for(int i=0; i<4; i=i+1) begin
        if(rob_mainbody[rob_head + i].exception != 0) begin
            for(int j=i+1; j<4; j=j+1) begin
                exception_mask[j] = 1'b0; // 所以后面所有的指令都没法退休
            end
            break;
        end
    end

    // 还要注意如果指令没准备好也没法退休
    for(int i=0; i<4; i=i+1) begin
        if(rob_mainbody[rob_head + i].completion == 0) begin
            for(int j=i; j<4; j=j+1) begin
                ready_mask[j] = 1'b0;
            end
            break;
        end
    end

    // 最后要综合上述所有的判断，才能决定最终哪些指令可以退休
    retire_mask = sec_branch_mask & sec_store_mask & exception_mask & ready_mask;

    retire_num = retire_mask[0] + retire_mask[1] + retire_mask[2] + retire_mask[3];
end

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