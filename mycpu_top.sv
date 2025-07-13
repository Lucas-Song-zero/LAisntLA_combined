`include "mycpu.h"

module core_top
#(
    parameter TLBNUM = 32
)
(
    input logic aclk, // 全局时钟
    input logic aresetn, // 全局复位，低有效(n)
    input logic [7:0] intrpt , // 中断输入

    //  AXI interface begin
    // read request
    output logic [3:0] arid, // AXI读地址通道ID
    output logic [31:0] araddr, // AXI读地址
    output logic [7:0] arlen, // AXI读地址长度
    output logic [2:0] arsize, // AXI读地址大小
    output logic [1:0] arburst, // AXI读地址突发类型
    output logic [1:0] arlock, // AXI读地址锁定类型
    output logic [3:0] arcache, // AXI读地址缓存类型
    output logic [2:0] arprot, // AXI读地址保护类型
    output logic arvalid, // AXI读地址有效
    input logic arready, // AXI读地址就绪

    // read response
    input logic [3:0] rid, // AXI读响应通道ID
    input logic [31:0] rdata, // AXI读响应数据
    input logic [1:0] rresp, // AXI读响应
    input logic rlast, // AXI读响应最后一个数据
    input logic rvalid, // AXI读响应有效
    output logic rready, // AXI读响应就绪

    // write request
    output logic [3:0] awid, // AXI写地址通道ID
    output logic [31:0] awaddr, // AXI写地址
    output logic [7:0] awlen, // AXI写地址长度
    output logic [2:0] awsize, // AXI写地址大小
    output logic [1:0] awburst, // AXI写地址突发类型
    output logic [1:0] awlock, // AXI写地址锁定类型
    output logic [3:0] awcache, // AXI写地址缓存类型
    output logic [2:0] awprot, // AXI写地址保护类型
    output logic awvalid, // AXI写地址有效
    input logic awready, // AXI写地址就绪

    // write data
    output logic [3:0] wid, // AXI写数据通道ID
    output logic [31:0] wdata, // AXI写数据
    output logic [3:0] wstrb, // AXI写数据字节使能
    output logic wlast, // AXI写数据最后一个数据
    output logic wvalid, // AXI写数据有效
    input logic wready, // AXI写数据就绪

    // write back
    input logic [3:0] bid, // AXI写响应通道ID
    input logic [1:0] bresp, // AXI写响应
    input logic bvalid, // AXI写响应有效
    output logic bready, // AXI写响应就绪
    
    // AXI interface ends

    // debug interface begin
    input logic break_point, // 调试断点输入
    input logic infor_flag, // 调试信息标志
    input logic [4:0] reg_num, // 调试时读取寄存器号
    output logic ws_valid, // 写状态有效
    output logic [31:0] rf_data, // 写状态数据
    
    output logic [31:0] debug0_wb_pc,
    output logic [31:0] debug0_wb_rf_wen,
    output logic [4:0] debug0_wb_rf_wnum,
    output logic [31:0] debug0_wb_rf_wdata,
    output logic [31:0] debug0_wb_inst
);
logic rst_n;
always_ff @(posedge aclk) begin
    rst_n <= aresetn;
end

// NLP
// NLP 需要pc reg
logic [31:0] pc_reg;
logic [31:0] pc_next_wire;
logic stall_pc_update;
assign stall_pc_update = !ifu_ready;
// 这里假设初始化PC为 0x1C000000
always_ff @(posedge aclk) begin
    if (!rst_n) begin
        pc_reg <= 32'h1C000000;
    end else if(!stall_pc_update) begin
        pc_reg <= pc_next_wire;
    end
end

// NLP 输出包括了 next_fetch_pc , taken , cut_pos, prediction_valid
logic taken_wire;
logic [1:0] cut_pos_wire;
logic prediction_valid_wire;
logic flush_nlp;

NLP nlp(
    .clk(aclk),
    .rst_n(rst_n),
    .flush(flush_nlp),

    .pc(pc_reg),
    .next_fetch_pc(pc_next_wire),
    .taken(taken_wire),
    .cut_pos(cut_pos_wire),
    .prediction_valid(prediction_valid_wire),

    // 更新BHT和uBTB的逻辑
    .update_valid(), // 先放在这里，最后统一处理
    .update_pc(),
    .target_pc(),
    .update_taken(),
    .update_cut_pos(),
    .update_branch_type()
);

logic flush_ifu;
logic stall_ifu;
logic ifu_ready;

logic icache_ready;
logic [31:0] fetch_pc_out;
logic [1:0] cut_pos_out;
logic fetch_req_valid;

IFU ifu(
    .clk(aclk),
    .rst_n(rst_n),
    .stall(stall_ifu),
    .flush(flush_ifu),

    .fetch_pc(pc_reg),
    .cut_pos(cut_pos_wire),
    .input_valid(1'b1), // 因为还有ifu_ready来防止重复塞入取指请求，所以这里可以直接置1
    // 这主要是因为前面我们认为BPU是连续输出的，中间没有无效值
    .icache_ready(icache_ready),
    .ifu_ready(ifu_ready),

    .fetch_pc_out(fetch_pc_out),
    .cut_pos_out(cut_pos_out),
    .fetch_req_valid(fetch_req_valid)
);
// IFU 要连接到icache上

// 这里可以放一个icache top实例
logic icache_resp_valid; // icache响应有效(至少有一条指令取出了)
logic [31:0] fetched_instr [3:0]; // 一次最多取四条指令
logic [1:0] icache_resp_instr_num; // 一次取指的指令数量
logic [3:0] fetch_valid; // 一次取指的指令四条中哪几条是有效的,应该一定是连续的1
always_comb begin
    case (icache_resp_instr_num)
        2'b00: fetch_valid = 4'b0001; // 第一位是1，其他位是0
        2'b01: fetch_valid = 4'b0011;
        2'b10: fetch_valid = 4'b0111;
        2'b11: fetch_valid = 4'b1111;
    endcase
end
// icache_top ...

// 然后是decoder
decoded_instr_t decoded_instr_out [3:0];
logic decoder_ready;


decoder_stage decoder(
    .clk(aclk),
    .rst_n(rst_n),
    .ifu_valid(icache_resp_valid),
    .flush(decoder_flush), // 只需要flush output就行

    .instr_0(fetched_instr[0]),
    .instr_1(fetched_instr[1]),
    .instr_2(fetched_instr[2]),
    .instr_3(fetched_instr[3]),
    .fetch_valid(fetch_valid),
    .decoder_ready(decoder_ready), // decoder_ready == rename_ready

    .decoded_instr(decoded_instr_out),
    .rename_ready(rename_ready)
);

logic rename_ready;
logic 
// 

endmodule