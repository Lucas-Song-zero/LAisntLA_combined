`timescale 1ns / 1ps
`include "defs.sv"

module rat(
    input logic clk,
    input logic rst_n,

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PREG_INDEX_WIDTH-1:0] recover_preg_index_vec [31:0],

    // read certain arch reg index -- preg index
    input logic [4:0] arch_rd_0_index,
    input logic [4:0] arch_rd_1_index,
    input logic [4:0] arch_rd_2_index,
    input logic [4:0] arch_rd_3_index,
    input logic [3:0] arch_rd_exist_vec, // 0--rd_0_exist

    input logic [3:0] arch_rj_0_index,
    input logic [3:0] arch_rj_1_index,
    input logic [3:0] arch_rj_2_index,
    input logic [3:0] arch_rj_3_index,
    input logic [3:0] arch_rj_exist_vec,

    input logic [3:0] arch_rk_0_index,
    input logic [3:0] arch_rk_1_index,
    input logic [3:0] arch_rk_2_index,
    input logic [3:0] arch_rk_3_index,
    input logic [3:0] arch_rk_exist_vec,
    
    // write rat
    input logic [3:0] updating_valid_vec,
    // updating_valid_vec == reg_rd_exist_vec
    input logic [`PREG_INDEX_WIDTH-1:0] added_preg_index_vec [3:0],

    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_0,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_1,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_2,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_3,

    output logic [`PREG_INDEX_WIDTH-1:0] rj_matched_preg_0,
    output logic [`PREG_INDEX_WIDTH-1:0] rj_matched_preg_1,
    output logic [`PREG_INDEX_WIDTH-1:0] rj_matched_preg_2,
    output logic [`PREG_INDEX_WIDTH-1:0] rj_matched_preg_3,

    output logic [`PREG_INDEX_WIDTH-1:0] rk_matched_preg_0,
    output logic [`PREG_INDEX_WIDTH-1:0] rk_matched_preg_1,
    output logic [`PREG_INDEX_WIDTH-1:0] rk_matched_preg_2,
    output logic [`PREG_INDEX_WIDTH-1:0] rk_matched_preg_3
);

logic [`PREG_INDEX_WIDTH-1:0] rat [31:0];

// read rat
always_comb begin
    // arch rd=0始终映射到preg 0
    rd_history_preg_0 = arch_rd_exist_vec[0] ? ((arch_rd_0_index == 5'h0) ? 7'h0 : rat[arch_rd_0_index]) : 0;
    rd_history_preg_1 = arch_rd_exist_vec[1] ? ((arch_rd_1_index == 5'h0) ? 7'h0 : rat[arch_rd_1_index]) : 0;
    rd_history_preg_2 = arch_rd_exist_vec[2] ? ((arch_rd_2_index == 5'h0) ? 7'h0 : rat[arch_rd_2_index]) : 0;
    rd_history_preg_3 = arch_rd_exist_vec[3] ? ((arch_rd_3_index == 5'h0) ? 7'h0 : rat[arch_rd_3_index]) : 0;

    rj_matched_preg_0 = arch_rj_exist_vec[0] ? rat[arch_rj_0_index] : 0;
    rj_matched_preg_1 = arch_rj_exist_vec[1] ? rat[arch_rj_1_index] : 0;
    rj_matched_preg_2 = arch_rj_exist_vec[2] ? rat[arch_rj_2_index] : 0;
    rj_matched_preg_3 = arch_rj_exist_vec[3] ? rat[arch_rj_3_index] : 0;

    rk_matched_preg_0 = arch_rk_exist_vec[0] ? rat[arch_rk_0_index] : 0;
    rk_matched_preg_1 = arch_rk_exist_vec[1] ? rat[arch_rk_1_index] : 0;
    rk_matched_preg_2 = arch_rk_exist_vec[2] ? rat[arch_rk_2_index] : 0;
    rk_matched_preg_3 = arch_rk_exist_vec[3] ? rat[arch_rk_3_index] : 0;
end

// write rat
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i=0;i<32;i=i+1) begin
            rat[i] <= i[6:0];
        end
    end
    else if(recover_valid) begin
        for(int i=0;i<32;i=i+1) begin
            rat[i] <= recover_preg_index_vec[i];
        end
    end
    else begin
        // arch rd=0始终映射到preg 0，不更新RAT表
        rat[arch_rd_0_index] <= (updating_valid_vec[0] && arch_rd_exist_vec[0] && arch_rd_0_index != 5'h0) ? added_preg_index_vec[0] : rat[arch_rd_0_index];
        rat[arch_rd_1_index] <= (updating_valid_vec[1] && arch_rd_exist_vec[1] && arch_rd_1_index != 5'h0) ? added_preg_index_vec[1] : rat[arch_rd_1_index];
        rat[arch_rd_2_index] <= (updating_valid_vec[2] && arch_rd_exist_vec[2] && arch_rd_2_index != 5'h0) ? added_preg_index_vec[2] : rat[arch_rd_2_index];
        rat[arch_rd_3_index] <= (updating_valid_vec[3] && arch_rd_exist_vec[3] && arch_rd_3_index != 5'h0) ? added_preg_index_vec[3] : rat[arch_rd_3_index];
    end
end

endmodule