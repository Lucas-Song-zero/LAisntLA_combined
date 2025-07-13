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
logic [31:0] fetch_pc_ifu_out;
logic [1:0] cut_pos_ifu_out;
logic pred_taken_ifu_out;
logic [31:0] pred_jump_target_pc_ifu_out;

IFU ifu(
    .clk(aclk),
    .rst_n(rst_n),
    .stall(stall_ifu),
    .flush(flush_ifu),

    .fetch_pc(pc_reg),
    .cut_pos(cut_pos_wire),
    .pred_taken_in(taken_wire),
    .pred_jump_target_pc_in(pc_next_wire),

    .input_valid(1'b1), // 因为还有ifu_ready来防止重复塞入取指请求，所以这里可以直接置1
    // 这主要是因为前面我们认为BPU是连续输出的，中间没有无效值
    .icache_ready(icache_ready),
    .ifu_ready(ifu_ready),

    .fetch_pc_out(fetch_pc_ifu_out),
    .cut_pos_out(cut_pos_ifu_out),
    .pred_taken_out(pred_taken_ifu_out),
    .pred_jump_target_pc_out(pred_jump_target_pc_ifu_out),
    .ifu_valid(ifu_valid)
);
// IFU 要连接到icache上

// 这里可以放一个icache top实例
logic ifu_valid; // ifu取指回来了，可以输出给decoder
logic [31:0] fetched_instr [3:0]; // 一次最多取四条指令
logic [1:0] icache_resp_instr_num; // 一次取指的指令数量
logic [3:0] fetch_valid; // 一次取指的指令四条中哪几条是有效的,应该一定是连续的1
// fetch_valid 是由 cut_pos决定的，每次icache都会取4条指令回来
// icache_top ...

// 然后是decoder
decoded_instr_t decoded_instr_out [3:0];
logic decoder_ready;
logic [31:0] start_pc_decoder_out;
logic pred_wrong_decoder_out;
logic [1:0] real_cut_pos_decoder_out;
logic [31:0] real_jump_target_pc_decoder_out;
logic pred_taken_decoder_out;
logic [1:0] pred_cut_pos_decoder_out;
logic [31:0] pred_next_fetch_targer_pc_decoder_out;

decoder_stage decoder(
    .clk(aclk),
    .rst_n(rst_n),
    .ifu_valid(ifu_valid),
    .flush(decoder_flush), // 只需要flush output就行

    .instr_0(fetched_instr[0]),
    .instr_1(fetched_instr[1]),
    .instr_2(fetched_instr[2]),
    .instr_3(fetched_instr[3]),
    .fetch_valid(fetch_valid),
    .decoder_ready(decoder_ready), // decoder_ready == rename_ready

    .start_pc_in(fetch_pc_ifu_out),
    .start_pc_out(start_pc_decoder_out),
    .pred_taken(pred_taken_ifu_out),
    .pred_taken_decoder_out(pred_taken_decoder_out),
    .pred_cut_pos(cut_pos_ifu_out),
    .pred_cut_pos_out(pred_cut_pos_decoder_out),
    .pred_next_fetch_target_pc(pred_jump_target_pc_ifu_out),
    .pred_next_fetch_targer_pc_out(pred_next_fetch_targer_pc_decoder_out),
    .pred_wrong(pred_wrong_decoder_out),
    .real_cut_pos_out(real_cut_pos_decoder_out),
    .real_jump_target_pc(real_jump_target_pc_decoder_out),
    
    .decoded_instr(decoded_instr_out),
    .decoded_instr_valid(decoded_instr_valid_out_vec),
    .decoder_valid(decoder_valid),
    .rename_ready(rename_ready),
    .rename_rd_request(rename_rd_request_decoder_out)
);

logic rename_ready;
logic decoder_valid;
logic [3:0] decoded_instr_valid_out_vec;
logic [2:0] rename_rd_request_decoder_out;

// 然后是rename
logic rename_flush;
logic [1:0] pred_cut_pos_decoder_out_minus_1; // 预测的cut_pos - 1
assign pred_cut_pos_decoder_out_minus_1 = pred_cut_pos_decoder_out - 2'b1;

rename_input_t rename_input_vec [3:0];
always_comb begin
    for(int i=0; i<4; i=i+1) begin
        rename_input_vec[i].valid = decoded_instr_valid_out_vec[i];
        rename_input_vec[i].gen_op_type = decoded_instr_out[i].gen_op_type;
        rename_input_vec[i].spec_op_type = decoded_instr_out[i].spec_op_type;
        rename_input_vec[i].exception = decoded_instr_out[i].exception;
        rename_input_vec[i].imm = decoded_instr_out[i].imm;
        rename_input_vec[i].imm_enable = decoded_instr_out[i].imm_enable;
        rename_input_vec[i].imm_sign_extend = decoded_instr_out[i].imm_sign_extend;
        rename_input_vec[i].reg_rd = decoded_instr_out[i].reg_rd;
        rename_input_vec[i].reg_rd_exist = decoded_instr_out[i].reg_rd_exist;
        rename_input_vec[i].reg_rj = decoded_instr_out[i].reg_rj;
        rename_input_vec[i].reg_rj_exist = decoded_instr_out[i].reg_rj_exist;
        rename_input_vec[i].reg_rk = decoded_instr_out[i].reg_rk;
        rename_input_vec[i].reg_rk_exist = decoded_instr_out[i].reg_rk_exist;
        rename_input_vec[i].store_or_load = decoded_instr_out[i].store_or_load;
        rename_input_vec[i].bar_type = decoded_instr_out[i].bar_type;
        rename_input_vec[i].bar_type = decoded_instr_out[i].bar_type;
        rename_input_vec[i].completion_bit = decoded_instr_out[i].completion_bit;
        rename_input_vec[i].issue_distr_direction = decoded_instr_out[i].issue_distr_direction;
        // 开始记性pred_jump_target_pc的计算
        // 这里我们假设分支预测在简单判断下是正确的
        if(pred_taken_decoder_out) begin
            // 预测跳转
            if(i == pred_cut_pos_decoder_out_minus_1) begin
                rename_input_vec[i].pred_jump_target_pc = pred_next_fetch_targer_pc_decoder_out;
            end else begin
                rename_input_vec[i].pred_jump_target_pc = start_pc_decoder_out + 4 * i + 4;
            end
        end else begin
            rename_input_vec[i].pred_jump_target_pc = start_pc_decoder_out + 4 * i + 4;
        end
    end
end


rename rename(
    .clk(aclk),
    .rst_n(rst_n),
    .decoder_valid(decoder_valid),
    .rename_flush(rename_flush),
    .issue_ready(issue_ready),
    .rob_index_start(rob_alloc_index_start), // ROB能给rename输出的指令分配的index的开始的index
    .rob_index_num_req(rob_index_num_req_rename_out), // 需要rename分配的index数量
    .rob_ready(rob_ready), // ROB是否准备好接收指令
    .rename_ready(rename_ready_out),
    .rename_valid(rename_valid_out),

    // 如果分支预测失败等情况出现，就需要recover
    .recover_valid(),
    .recover_preg_index_vec(), // 使用aRAT恢复RAT
    // 可能还有freelist比如，但是现在先不考虑这么多

    // 退休指令把rd history写回
    .retire_write_back_valid(), // 退休指令把rd history写回
    .freed_preg_valid_vec(), // 写回的rd history preg index是否有效
    .freed_preg_index_0(),
    .freed_preg_index_1(),
    .freed_preg_index_2(),
    .freed_preg_index_3(), // 写回的最多四个freed preg index
    
    // rename输入结构体数组
    .rename_input_vec(rename_input_vec), // 基本上是decoder的输出
    .start_pc_in(start_pc_decoder_out), // 取指开始的pc

    .rename_output_vec(rename_output_vec) // rename之后的输出结构体
)

rename_output_t rename_output_vec [3:0];
logic dispatch_ready;
logic issue_ready;
logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_alloc_index_start;
logic [2:0] rob_index_num_req_rename_out;
logic [2:0] rename_rd_request_out; 

endmodule