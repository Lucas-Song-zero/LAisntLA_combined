`include "defs.sv"

// busy table 只会在rename阶段进行读取，然后在exec阶段会进行更新
// 因为rename完成进入IQ的指令会每个周期从FU完成的 rd 广播中提取 rd_index
module busy_table(
    input logic clk,
    input logic rst_n,

    // rename rj rk data valid read
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rj_index_vec [3:0],
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rk_index_vec [3:0],
    input logic [`PREG_INDEX_WIDTH-1:0] preg_rd_index_vec [3:0],
    // 如果rj,rd,rk不存在就会被置0，而0的值是0，所以一定是有效的
    output logic [3:0] rj_ready_vec,
    output logic [3:0] rk_ready_vec,
    output logic [3:0] rd_ready_vec,

    // interface with FU
    // begin exec -> valid set to 0
    input logic [`PREG_INDEX_WIDTH-1:0] begin_exec_rd_index_vec [3:0],
    input logic [3:0] begin_exec_valid_vec,

    // end exec -> valid set to 1
    input logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_vec [3:0],
    input logic [3:0] wb_rd_valid_vec,

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PRF_NUM-1:0] recover_busy_table
);

// 表示每个PRF里面的值是否求出来
logic [`PRF_NUM-1:0] busy_table;

// 为了保证同步，这边的busy table readout要使用comb逻辑
always_comb begin
    rj_ready_vec = 4'b0;
    rk_ready_vec = 4'b0;
    rd_ready_vec = 4'b0;
    for(int i=0; i<4; i=i+1) begin
        rd_ready_vec[i] = busy_table[begin_exec_rd_index_vec[i]];
        rj_ready_vec[i] = busy_table[preg_rj_index_vec[i]];
        rk_ready_vec[i] = busy_table[preg_rk_index_vec[i]];
        // 下面这里做了转发，从而实现了先写后读的效果
        for(int j=0; j<4; j=j+1) begin
            if(begin_exec_valid_vec[i] && begin_exec_rd_index_vec[i] == preg_rd_index_vec[j]) begin
                rd_ready_vec[i] = 1;
            end else if (wb_rd_valid_vec[i] && wb_rd_index_vec[i] == preg_rd_index_vec[j]) begin
                rd_ready_vec[i] = 0;
            end
            break;
        end
    end
end

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        for(int i=0;i<32;i=i+1) begin
            busy_table[i] <= 0; // all value set to 0, means all vals valid
        end
    end else if (recover_valid) begin
        busy_table <= recover_busy_table;
    end else begin
        // update busy table
        if(begin_exec_valid_vec[0] && begin_exec_rd_index_vec[0] != 0) begin
            busy_table[begin_exec_rd_index_vec[0]] <= 1;
        end
        if(begin_exec_valid_vec[1] && begin_exec_rd_index_vec[1] != 0) begin
            busy_table[begin_exec_rd_index_vec[1]] <= 1;
        end
        if(begin_exec_valid_vec[2] && begin_exec_rd_index_vec[2] != 0) begin
            busy_table[begin_exec_rd_index_vec[2]] <= 1;
        end
        if(begin_exec_valid_vec[3] && begin_exec_rd_index_vec[3] != 0) begin
            busy_table[begin_exec_rd_index_vec[3]] <= 1;
        end

        if(wb_rd_valid_vec[0]) begin
            busy_table[wb_rd_index_vec[0]] <= 0;
        end
        if(wb_rd_valid_vec[1]) begin
            busy_table[wb_rd_index_vec[1]] <= 0;
        end
        if(wb_rd_valid_vec[2]) begin
            busy_table[wb_rd_index_vec[2]] <= 0;
        end
        if(wb_rd_valid_vec[3]) begin
            busy_table[wb_rd_index_vec[3]] <= 0;
        end
    end
end
endmodule