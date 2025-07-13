`timescale 1ns / 1ps
`include "defs.sv"

module logic_checker(
    input logic clk,
    input logic rst_n,

    input logic [4:0] instr_rd_index_vec [3:0],
    input logic [4:0] instr_rj_index_vec [3:0],
    input logic [4:0] instr_rk_index_vec [3:0],
    input logic [3:0] instr_rd_exist_vec,
    input logic [3:0] instr_rj_exist_vec,
    input logic [3:0] instr_rk_exist_vec,

    // rj or rk == previous rd (RAW) , pos -> previous rd pos(0~3)
    output logic [1:0] rj_0_raw_pos, 
    output logic [1:0] rj_1_raw_pos,
    output logic [1:0] rj_2_raw_pos,
    output logic [1:0] rj_3_raw_pos,

    output logic [1:0] rk_0_raw_pos,
    output logic [1:0] rk_1_raw_pos,
    output logic [1:0] rk_2_raw_pos,
    output logic [1:0] rk_3_raw_pos,

    // rd == previous rd (WAW) , pos -> previous rd pos(0~3)
    output logic [1:0] rd_0_waw_pos,
    output logic [1:0] rd_1_waw_pos,
    output logic [1:0] rd_2_waw_pos,
    output logic [1:0] rd_3_waw_pos
);

always_comb begin
    rj_0_raw_pos = 0;
    rj_1_raw_pos = 1;
    rj_2_raw_pos = 2;
    rj_3_raw_pos = 3;
    // default is no forwarding

    // rj == closest previous rd (RAW)
    if(instr_rd_exist_vec[0] && instr_rd_index_vec[0] != 0 && instr_rj_exist_vec[1] && instr_rd_index_vec[0] == instr_rj_index_vec[1]) begin
        rj_1_raw_pos = 0;
    end
    // if the first instr rd == the second instr rj,
    // then we expect we get rj_1_raw_pos = 0, meaning the second instr's rj renamed preg index
    // should be same as the first instr's rd renamed preg index (forwarding)

    // the rj_2_raw_pos - find the closest (latest) previous rd that matches rj_2
    for(int i=1;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rj_exist_vec[2] && instr_rd_index_vec[i] == instr_rj_index_vec[2]) begin
            rj_2_raw_pos = i;
            break; // found the closest one, exit loop
        end
    end

    // rj_3_raw_pos - find the closest (latest) previous rd that matches rj_3
    for(int i=2;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rj_exist_vec[3] && instr_rd_index_vec[i] == instr_rj_index_vec[3]) begin
            rj_3_raw_pos = i;
            break; // found the closest one, exit loop
        end
    end
end

always_comb begin // rk RAW checker
    rk_0_raw_pos = 0;
    rk_1_raw_pos = 1;
    rk_2_raw_pos = 2;
    rk_3_raw_pos = 3;
    // default is no forwarding
    
    // rk == closest previous rd (RAW)
    if(instr_rd_exist_vec[0] && instr_rk_exist_vec[1] && instr_rd_index_vec[0] != 0 &&  instr_rd_index_vec[0] == instr_rk_index_vec[1]) begin
        rk_1_raw_pos = 0;
    end

    // rk_2_raw_pos - find the closest (latest) previous rd that matches rk_2
    for(int i=1;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rk_exist_vec[2] && instr_rd_index_vec[i] == instr_rk_index_vec[2]) begin
            rk_2_raw_pos = i;
            break; // found the closest one, exit loop
        end
    end

    // rk_3_raw_pos - find the closest (latest) previous rd that matches rk_3
    for(int i=2;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rk_exist_vec[3] && instr_rd_index_vec[i] == instr_rk_index_vec[3]) begin
            rk_3_raw_pos = i;
            break; // found the closest one, exit loop
        end
    end
end

always_comb begin
    rd_0_waw_pos = 0;
    rd_1_waw_pos = 1;
    rd_2_waw_pos = 2;
    rd_3_waw_pos = 3;

    // rd == closest previous rd (WAW)
    if(instr_rd_exist_vec[0] && instr_rd_index_vec[0] != 0 && instr_rd_exist_vec[1] && instr_rd_index_vec[0] == instr_rd_index_vec[1]) begin
        rd_1_waw_pos = 0;
    end

    // rd_2_waw_pos - find the closest (latest) previous rd that matches rd_2
    for(int i=1;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rd_exist_vec[2] && instr_rd_index_vec[i] == instr_rd_index_vec[2]) begin
            rd_2_waw_pos = i;
            break; // found the closest one, exit loop
        end
    end

    // rd_3_waw_pos - find the closest (latest) previous rd that matches rd_3
    for(int i=2;i>=0;i=i-1) begin
        if(instr_rd_exist_vec[i] && instr_rd_index_vec[i] != 0 && instr_rd_exist_vec[3] && instr_rd_index_vec[i] == instr_rd_index_vec[3]) begin
            rd_3_waw_pos = i;
            break; // found the closest one, exit loop
        end
    end
end

endmodule