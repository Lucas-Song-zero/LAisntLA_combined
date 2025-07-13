`timescale 1ns / 1ps
`include "defs.sv"

// here we combine freelist into aRAT
module aRAT(
    input logic clk,
    input logic rst_n,

    // retire interface
    input logic retire_valid,

    // read certain arch reg index -- preg index
    input logic [4:0] arch_rd_0_index,
    input logic [4:0] arch_rd_1_index,
    input logic [4:0] arch_rd_2_index,
    input logic [4:0] arch_rd_3_index,
    input logic [3:0] arch_rd_exist_vec, // 0--rd_0_exist

    // history mapping for rd
    input logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_0,
    input logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_1,
    input logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_2,
    input logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_3,

    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index_3,

    // recover (exception and branch prediction failure)
    output logic [`PREG_INDEX_WIDTH-1:0] recover_preg_index_vec [31:0],
    // combined freelist (just extract arat mappings)
    output logic [`PREG_INDEX_WIDTH-1:0] freelist_preg_index_vec [`PRF_NUM-33:0],
    output logic [`PREG_INDEX_WIDTH-1:0] freelist_queue_head,
    output logic [`PREG_INDEX_WIDTH-1:0] freelist_queue_tail,
);

logic [`PREG_INDEX_WIDTH-1:0] arat [31:0];
// Shadow variables
logic [`PREG_INDEX_WIDTH-1:0] next_freelist_preg_index_vec [`PRF_NUM-33:0];
logic [`PREG_INDEX_WIDTH-1:0] next_freelist_queue_head;
logic [`PREG_INDEX_WIDTH-1:0] next_freelist_queue_tail;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i=0;i<32;i=i+1) begin
            arat[i] <= i;
        end
        for(int i=0; i<`PRF_NUM-33; i=i+1) begin
            freelist_preg_index_vec[i] <= i+32;
        end
        freelist_queue_head <= 0;
        freelist_queue_tail <= `PRF_NUM-33;
    end
    else if(retire_valid) begin

        for(int i=0; i< `PRF_NUM-33; i=i+1) begin
            next_freelist_preg_index_vec[i] = freelist_preg_index_vec[i];
        end
        next_freelist_queue_head = freelist_queue_head;
        next_freelist_queue_tail = freelist_queue_tail;

        if(arch_rd_exist_vec[0]) begin
            arat[arch_rd_0_index] <= preg_rd_index_0;
            next_freelist_queue_head = next_freelist_queue_head + 1;
            next_freelist_queue_tail = (next_freelist_queue_tail + 1) % (`PRF_NUM-33);
            next_freelist_preg_index_vec[next_freelist_queue_tail] = preg_rd_index_0;
        end
        if(arch_rd_exist_vec[1]) begin
            arat[arch_rd_1_index] <= preg_rd_index_1;
            next_freelist_queue_head = next_freelist_queue_head + 1;
            next_freelist_queue_tail = (next_freelist_queue_tail + 1) % (`PRF_NUM-33);
            next_freelist_preg_index_vec[next_freelist_queue_tail] = preg_rd_index_1;
        end
        if(arch_rd_exist_vec[2]) begin
            arat[arch_rd_2_index] <= preg_rd_index_2;
            next_freelist_queue_head = next_freelist_queue_head + 1;
            next_freelist_queue_tail = (next_freelist_queue_tail + 1) % (`PRF_NUM-33);
            next_freelist_preg_index_vec[next_freelist_queue_tail] = preg_rd_index_2;
        end
        if(arch_rd_exist_vec[3]) begin
            arat[arch_rd_3_index] <= preg_rd_index_3;
            next_freelist_queue_head = next_freelist_queue_head + 1;
            next_freelist_queue_tail = (next_freelist_queue_tail + 1) % (`PRF_NUM-33);
            next_freelist_preg_index_vec[next_freelist_queue_tail] = preg_rd_index_3;
        end

        freelist_preg_index_vec <= next_freelist_preg_index_vec;
        freelist_queue_head <= next_freelist_queue_head;
        freelist_queue_tail <= next_freelist_queue_tail;   
    end
end

always_comb begin
    for(int i=0;i<32;i=i+1) begin
        recover_preg_index_vec[i] = arat[i];
    end
end

endmodule