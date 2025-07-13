`include "defs.sv"

// busy table 只会在rename阶段进行读取，然后在exec阶段会进行更新
// 因为rename完成进入IQ的指令会每个周期从FU完成的 rd 广播中提取 rd_index
module busy_table(
    input logic clk,
    input logic rst_n,

    // rename rj rk data valid read
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_index_3,
    input logic [3:0] preg_rj_exist_vec,
    
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_index_3,
    input logic [3:0] preg_rk_exist_vec,

    // readout valid signals
    output logic [3:0] rj_valid_vec,
    output logic [3:0] rk_valid_vec,

    // interface with FU
    // begin exec -> valid set to 0
    output logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_0,
    output logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_1,
    output logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_2,
    output logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_3,
    output logic [3:0] begin_exec_rd_exist_vec,

    // end exec -> valid set to 1
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_3,
    input logic [3:0] wb_rd_exist_vec,

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PRF_NUM-1:0] recover_busy_table,
);

// 表示每个PRF里面的值是否求出来
logic [`PRF_NUM-1:0] busy_table ;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i=0;i<32;i=i+1) begin
            busy_table[i] <= 0; // all value set to 0, means all vals valid
        end
    end else if (recover_valid) begin
        busy_table <= recover_busy_table;
    end else begin
        // default values
        rj_valid_vec = 4'b0;
        rk_valid_vec = 4'b0;

        // rj valid readout (with bypass)
        if(preg_rj_exist_vec[0]) begin
            if(begin_exec_rd_exist_vec[0] && (begin_exec_rd_index_0 == preg_rj_index_0)) begin
                rj_valid_vec[0] = 1;
            end else if (wb_rd_exist_vec[0] && (wb_rd_index_0 == preg_rj_index_0)) begin
                rj_valid_vec[0] = 0;
            end else begin
                rj_valid_vec[0] = busy_table[preg_rj_index_0];
            end
        end
        if(preg_rj_exist_vec[1]) begin
            if(begin_exec_rd_exist_vec[1] && (begin_exec_rd_index_1 == preg_rj_index_1)) begin
                rj_valid_vec[1] = 1;
            end else if (wb_rd_exist_vec[1] && (wb_rd_index_1 == preg_rj_index_1)) begin
                rj_valid_vec[1] = 0;
            end else begin
                rj_valid_vec[1] = busy_table[preg_rj_index_1];
            end
        end
        if(preg_rj_exist_vec[2]) begin
            if(begin_exec_rd_exist_vec[2] && (begin_exec_rd_index_2 == preg_rj_index_2)) begin
                rj_valid_vec[2] = 1;
            end else if (wb_rd_exist_vec[2] && (wb_rd_index_2 == preg_rj_index_2)) begin
                rj_valid_vec[2] = 0;
            end else begin
                rj_valid_vec[2] = busy_table[preg_rj_index_2];
            end
        end
        if(preg_rj_exist_vec[3]) begin
            if(begin_exec_rd_exist_vec[3] && (begin_exec_rd_index_3 == preg_rj_index_3)) begin
                rj_valid_vec[3] = 1;
            end else if (wb_rd_exist_vec[3] && (wb_rd_index_3 == preg_rj_index_3)) begin
                rj_valid_vec[3] = 0;
            end else begin
                rj_valid_vec[3] = busy_table[preg_rj_index_3];
            end
        end

        // rk valid readout (with bypass)
        if(preg_rk_exist_vec[0]) begin
            if(begin_exec_rd_exist_vec[0] && (begin_exec_rd_index_0 == preg_rk_index_0)) begin
                rk_valid_vec[0] = 1;
            end else if (wb_rd_exist_vec[0] && (wb_rd_index_0 == preg_rk_index_0)) begin
                rk_valid_vec[0] = 0;
            end else begin
                rk_valid_vec[0] = busy_table[preg_rk_index_0];
            end
        end
        if(preg_rk_exist_vec[1]) begin
            if(begin_exec_rd_exist_vec[1] && (begin_exec_rd_index_1 == preg_rk_index_1)) begin
                rk_valid_vec[1] = 1;
            end else if (wb_rd_exist_vec[1] && (wb_rd_index_1 == preg_rk_index_1)) begin
                rk_valid_vec[1] = 0;
            end else begin
                rk_valid_vec[1] = busy_table[preg_rk_index_1];
            end
        end
        if(preg_rk_exist_vec[2]) begin
            if(begin_exec_rd_exist_vec[2] && (begin_exec_rd_index_2 == preg_rk_index_2)) begin
                rk_valid_vec[2] = 1;
            end else if (wb_rd_exist_vec[2] && (wb_rd_index_2 == preg_rk_index_2)) begin
                rk_valid_vec[2] = 0;
            end else begin
                rk_valid_vec[2] = busy_table[preg_rk_index_2];
            end
        end
        if(preg_rk_exist_vec[3]) begin
            if(begin_exec_rd_exist_vec[3] && (begin_exec_rd_index_3 == preg_rk_index_3)) begin
                rk_valid_vec[3] = 1;
            end else if (wb_rd_exist_vec[3] && (wb_rd_index_3 == preg_rk_index_3)) begin
                rk_valid_vec[3] = 0;
            end else begin
                rk_valid_vec[3] = busy_table[preg_rk_index_3];
            end
        end

        // update busy table
        if(begin_exec_rd_exist_vec[0]) begin
            busy_table[begin_exec_rd_index_0] <= 1;
        end
        if(begin_exec_rd_exist_vec[1]) begin
            busy_table[begin_exec_rd_index_1] <= 1;
        end
        if(begin_exec_rd_exist_vec[2]) begin
            busy_table[begin_exec_rd_index_2] <= 1;
        end
        if(begin_exec_rd_exist_vec[3]) begin
            busy_table[begin_exec_rd_index_3] <= 1;
        end

        if(wb_rd_exist_vec[0]) begin
            busy_table[wb_rd_index_0] <= 0;
        end
        if(wb_rd_exist_vec[1]) begin
            busy_table[wb_rd_index_1] <= 0;
        end
        if(wb_rd_exist_vec[2]) begin
            busy_table[wb_rd_index_2] <= 0;
        end
        if(wb_rd_exist_vec[3]) begin
            busy_table[wb_rd_index_3] <= 0;
        end
    end
end
endmodule