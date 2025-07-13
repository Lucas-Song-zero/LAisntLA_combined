`include "mycpu.h"

module IFU(
    input logic clk,
    input logic rst_n,
    input logic stall,
    input logic flush,

    input logic [31:0] fetch_pc,
    input logic [1:0] cut_pos,
    input logic input_valid,
    input logic icache_ready, // Icache准备好取指
    output logic ifu_ready, // IFU准备好接收inserting取指请求

    output logic [31:0] fetch_pc_out,
    output logic [1:0] cut_pos_out, // 00 -- 取四条指令, 01 -- 取一条指令, 10 -- 取两条指令, 11 -- 取三条指令
    output logic fetch_req_valid,
    
);

IFB_ENTRY_t IFB [`IFB_DEPTH-1:0]; // 32个IFB_ENTRY
logic [`IFB_ENTRY_WIDTH-1:0] IFB_head_ptr;
logic [`IFB_ENTRY_WIDTH-1:0] IFB_tail_ptr;
logic [`IFB_ENTRY_WIDTH:0] filled_entry_num;

always_comb begin
    fetch_req_valid = 1'b0;
    if(filled_entry_num != 0) begin
        fetch_req_valid = 1'b1;
    end else begin
        fetch_req_valid = 1'b0;
    end
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
        if (icache_ready && filled_entry_num != 0) begin
            fetch_pc_out <= IFB[IFB_head_ptr].fetch_pc;
            cut_pos_out <= IFB[IFB_head_ptr].cut_pos;
            IFB_head_ptr <= IFB_head_ptr + 1;
            fetch_req_valid <= 1'b1;
            if(input_valid && ifu_ready) begin
                IFB[IFB_tail_ptr].fetch_pc <= fetch_pc;
                IFB[IFB_tail_ptr].cut_pos <= cut_pos;
                IFB_tail_ptr <= IFB_tail_ptr + 1;
                filled_entry_num <= filled_entry_num;
            end else begin
                filled_entry_num <= filled_entry_num - 1;
            end
        end else begin
            fetch_req_valid <= 1'b0;
            if(input_valid && ifu_ready) begin
                IFB[IFB_tail_ptr].fetch_pc <= fetch_pc;
                IFB[IFB_tail_ptr].cut_pos <= cut_pos;
                IFB_tail_ptr <= IFB_tail_ptr + 1;
                filled_entry_num <= filled_entry_num;
            end
        end
    end
end
endmodule