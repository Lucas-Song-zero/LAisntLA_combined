`include "mycpu.h"

module IFU(
    input logic clk,
    input logic rst_n,
    input logic stall,
    input logic flush,

    input logic [31:0] fetch_pc,
    input logic [1:0] cut_pos,
    input logic pred_taken_in,
    input logic [31:0] pred_jump_target_pc_in,
    input logic input_valid,
    input logic icache_ready, // Icache准备好取指
    output logic ifu_ready, // IFU准备好接收inserting取指请求
    // 这里写一个icache的req类型信号
    // 包含了fetch_req_valid ... etc
    // 需要结合icache接口设计具体内容

    output logic [31:0] fetch_pc_out,
    output logic [1:0] cut_pos_out, // 00 -- 取四条指令, 01 -- 取一条指令, 10 -- 取两条指令, 11 -- 取三条指令
    output logic pred_taken_out, // 预测是否跳转
    output logic [31:0] pred_jump_target_pc_out, // 预测的跳转目标PC
    output logic ifu_valid,
    
    // 从icache读取回来4条指令
    input logic [31:0] fetched_instr_group [3:0],
    input logic [`IFB_ENTRY_WIDTH-1:0] fetched_entry_index_back,
    input logic fetched_instr_valid, // 表示icache取到指令了，写回是有效的
    output logic [31:0] instr_out [3:0]
);

IFB_ENTRY_t IFB [`IFB_DEPTH-1:0]; // 32个IFB_ENTRY
logic [`IFB_ENTRY_WIDTH-1:0] IFB_head_ptr;
logic [`IFB_ENTRY_WIDTH-1:0] IFB_tail_ptr;
logic [`IFB_ENTRY_WIDTH:0] filled_entry_num;

assign ifu_valid = IFB[IFB_head_ptr].valid; // 因为取指要求顺序流下去

logic [`IFB_ENTRY_WIDTH-1:0] to_issue_entry_index; // 表示哪个表项可以发射出去了
logic [`IFB_ENTRY_WIDTH-1:0] temp_entry_index; // 只是中间变量
logic to_issue; // 表明可以issue了

logic [7:0] low_level_can_be_issued_vec [3:0];
logic [3:0] high_level_can_be_issued_vec;

genvar j, i;
generate
    for (j = 0; j < 4; j = j + 1) begin : gen_low_level_vec
        for (i = 0; i < 8; i = i + 1) begin : gen_low_level_bit
            assign low_level_can_be_issued_vec[j][i] = IFB[8*j + i].taken && !IFB[8*j + i].issued && !IFB[8*j + i].valid;
        end
    end
endgenerate

genvar j;
generate
    for (j = 0; j < 4; j = j + 1) begin : gen_high_level_vec
        assign high_level_can_be_issued_vec[j] = |low_level_can_be_issued_vec[j];
    end
endgenerate




always_comb begin
    fetch_req_valid = 1'b0;
    to_issue = 1'b0;
    
    
    if(filled_entry_num == `IFB_DEPTH) begin
        ifu_ready = 1'b0;
    end else begin
        ifu_ready = 1'b1;
    end
end

always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || flush) begin
        for (int i = 0; i < `IFB_DEPTH; i++) begin
            IFB[i] <= '0;
        end
        IFB_head_ptr <= '0;
        IFB_tail_ptr <= '0;
        filled_entry_num <= '0;
    end else if (!stall) begin
        if (IFB[IFB_head_ptr].valid) begin
            IFB[IFB_head_ptr].valid <= 1'b0;
            IFB[IFB_head_ptr].issued <= 1'b0;
            IFB[IFB_head_ptr].taken <= 1'b0;

            fetch_pc_out <= IFB[IFB_head_ptr].fetch_pc;
            cut_pos_out <= IFB[IFB_head_ptr].cut_pos;
            pred_taken_out <= IFB[IFB_head_ptr].pred_taken;
            pred_jump_target_pc_out <= IFB[IFB_head_ptr].pred_jump_target_pc;
            instr_out <= IFB[IFB_head_ptr].fetched_instr_group;

            IFB_head_ptr <= IFB_head_ptr + 1;
            if(input_valid && ifu_ready) begin
                IFB[IFB_tail_ptr].fetch_pc <= fetch_pc;
                IFB[IFB_tail_ptr].cut_pos <= cut_pos;
                IFB[IFB_tail_ptr].pred_taken <= 1'b1;
                IFB[IFB_tail_ptr].pred_jump_target_pc <= pred_jump_target_pc_in;
                IFB[IFB_tail_ptr].fetched_instr_group <= fetched_instr_group;
                IFB[IFB_tail_ptr].issued <= 1'b0; // 请求未发射
                IFB[IFB_tail_ptr].taken <= 1'b1;
                IFB[IFB_tail_ptr].valid <= 1'b0; // 请求未收到icache的结果

                IFB_tail_ptr <= IFB_tail_ptr + 1;
                filled_entry_num <= filled_entry_num;
            end else begin
                filled_entry_num <= filled_entry_num - 1;
            end
        end else begin
            if(input_valid && ifu_ready) begin
                IFB[IFB_tail_ptr].fetch_pc <= fetch_pc;
                IFB[IFB_tail_ptr].cut_pos <= cut_pos;
                IFB[IFB_tail_ptr].pred_taken <= 1'b1;
                IFB[IFB_tail_ptr].pred_jump_target_pc <= pred_jump_target_pc_in;
                IFB[IFB_tail_ptr].fetched_instr_group <= fetched_instr_group;
                IFB[IFB_tail_ptr].issued <= 1'b0; // 请求未发射
                IFB[IFB_tail_ptr].taken <= 1'b1;
                IFB[IFB_tail_ptr].valid <= 1'b0; // 请求未收到icache的结果

                IFB_tail_ptr <= IFB_tail_ptr + 1;
                filled_entry_num <= filled_entry_num + 1;
            end
        end
    end
end

// 发射给icache的req信号
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n || flush) begin
        
    end else if(!stall) begin
        // 需要根据icache req的信号来决定具体赋值操作
    end
end

endmodule