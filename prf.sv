`include "mycpu.h"

module prf (
    input clk,
    input rst_n,

    // interface with IQ
    input logic [`PREG_INDEX_WIDTH-1:0] iq_preg_rd_index_vec [3:0],
    input logic [3:0] iq_preg_rd_valid_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] iq_preg_rj_index_vec [3:0],
    input logic [3:0] iq_preg_rj_valid_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] iq_preg_rk_index_vec [3:0],
    input logic [3:0] iq_preg_rk_valid_vec,
    output logic [31:0] prf_rd_data_vec [3:0],
    output logic [31:0] prf_rj_data_vec [3:0],
    output logic [31:0] prf_rk_data_vec [3:0],

    // interface with FU
    input logic [31:0] fu_result_vec [3:0],
    input logic [`PREG_INDEX_WIDTH-1:0] fu_wb_rd_index_vec [3:0],
    input logic [3:0] fu_wb_valid_vec,
    
    // interface with ROB
    input logic [`PREG_INDEX_WIDTH-1:0] rob_head_preg_rd_index_wire,
    output logic [31:0] debug0_wb_rf_wdata // 也就是所谓提交写回的数据
);

logic [31:0] prf [`PRF_NUM-1:0];

logic [2:0] preg_rd_compare_with_wb_rd_index_vec [3:0];
logic [2:0] preg_rj_compare_with_wb_rd_index_vec [3:0];
logic [2:0] preg_rk_compare_with_wb_rd_index_vec [3:0];

always_comb begin
    // 将 preg_rd, rj, rk和 wb_rd_index进行比较，如果相等，则将对应的preg_rd_compare_with_wb_rd_index_vec[i]拉高
    for(int i=0; i<4; i=i+1) begin
        for(int j=0; j<4; j=j+1) begin
            if(iq_preg_rd_index_vec[i] == fu_wb_rd_index_vec[j]) begin
                preg_rd_compare_with_wb_rd_index_vec[i] = j;
            end else begin
                preg_rd_compare_with_wb_rd_index_vec[i] = 3'b100;
            end

            if(iq_preg_rj_index_vec[i] == )
        end
        
        
    end
end

always_comb begin
    for(int i=0; i<4; i=i+1) begin
        if(iq_preg_rd_valid_vec[i] && iq_preg_rd_index_vec[i] != 0) begin
            prf_rd_data_vec[i] = prf[iq_preg_rd_index_vec[i]];
        end else begin
            prf_rd_data_vec[i] = 0;
        end

        if(iq_preg_rj_valid_vec[i] && iq_preg_rj_index_vec[i] != 0) begin
            prf_rj_data_vec[i] = prf[iq_preg_rj_index_vec[i]];
        end else begin
            prf_rj_data_vec[i] = 0;
        end

        if(iq_preg_rk_valid_vec[i] && iq_preg_rk_index_vec[i] != 0) begin
            prf_rk_data_vec[i] = prf[iq_preg_rk_index_vec[i]];
        end else begin
            prf_rk_data_vec[i] = 0;
        end
    end

    for(int i=0; i<4 ;)

    debug0_wb_rf_wdata = prf[rob_head_preg_rd_index_wire];
end


endmodule