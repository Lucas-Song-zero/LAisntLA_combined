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
    output logic [3:0] debug0_wb_rf_wen, // 写回字节数，一般都设置为4‘b1111
    output logic[ 4:0] debug0_wb_rf_wnum, // 提交指令要写回的arch rd号
    output logic [31:0] debug0_wb_rf_wdata, // 提交写回的数据，本质上就是把物理寄存器里面的值读出来
    output logic [31:0] debug0_wb_inst // 提交指令的机器码，用于debug
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
// always_ff @(posedge aclk) begin
//     if (!rst_n) begin
//         pc_reg <= 32'h1C000000;
//     end else if(!stall_pc_update) begin
//         pc_reg <= pc_next_wire;
//     end
// end

// 这里可以先不考虑预测，先写一个简单的always pred no jump的逻辑
assign taken_wire = 1'b0; // 默认不跳转
assign cut_pos_wire = 2'b00; // 默认没有跳转分支指令
always_ff @(posedge aclk or negedge rst_n) begin
    if(!rst_n) begin
        pc_reg <= 32'h1C000000;
    end else if(!stall_pc_update) begin
        pc_reg <= pc_reg + 16; // 每次取4条指令
    end
end

// NLP 输出包括了 next_fetch_pc , taken , cut_pos, prediction_valid
logic taken_wire;
logic [1:0] cut_pos_wire;
logic prediction_valid_wire;
logic flush_nlp;

// 后面再来验证NLP可行性
// NLP nlp(
//     .clk(aclk),
//     .rst_n(rst_n),
//     .flush(flush_nlp),

//     .pc(pc_reg),
//     .next_fetch_pc(pc_next_wire),
//     .taken(taken_wire),
//     .cut_pos(cut_pos_wire),
//     .prediction_valid(prediction_valid_wire),

//     // 更新BHT和uBTB的逻辑
//     .update_valid(), // 先放在这里，最后统一处理
//     .update_pc(),
//     .target_pc(),
//     .update_taken(),
//     .update_cut_pos(),
//     .update_branch_type()
// );

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
    .ifu_valid(ifu_valid),

    // 从icache那边读取回来的4条指令
    .fetched_instr_group(), // icache resp的一部分
    .fetched_entry_index_back(), // 从icache resp接受读取到的是哪一次的取指请求信息
    .fetched_instr_valid(), // 表示icache取到指令了，写回是有效的
    .instr_out(fetched_instr_out) // 输出给decoder的指令
);
// IFU 要连接到icache上


// 这里可以放一个icache top实例
logic ifu_valid; // ifu的最前面的指令需求取指回来了(valid=1)，可以输出给decoder
logic [31:0] fetched_instr_out [3:0]; // 一次最多取四条指令
// instr_valid 是由 cut_pos决定的，每次icache都会取4条指令回来
logic [3:0] instr_valid;
always_comb begin
    instr_valid = 4'b0000;
    case(cut_pos_ifu_out)
        2'b00: instr_valid = 4'b1111;
        2'b01: instr_valid = 4'b0001;
        2'b10: instr_valid = 4'b0011;
        2'b11: instr_valid = 4'b0111;
    endcase
end
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
    .instr_valid(instr_valid),
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

    .rename_output_vec(rename_output_vec), // rename之后的输出结构体
    // 和忙表的交互
    .begin_exec_rd_index_vec(),
    .begin_exec_valid_vec(),
    .wb_rd_index_vec(),
    .wb_valid_vec()
);

rename_output_t rename_output_vec [3:0];
logic issue_ready;
logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_alloc_index_start;
logic [2:0] rob_index_num_req_rename_out;
logic [2:0] rename_rd_request_out;

// 这里需要增加一个根据renamed之后的指令的 issue_direction 来每个IQ需要分配的空位数量的逻辑
logic [2:0] total_simple_iq_input_instr_num;
logic [2:0] simple_iq_0_input_instr_num;
logic [2:0] simple_iq_1_input_instr_num;
logic [2:0] complex_iq_input_instr_num;
logic [2:0] lsu_iq_input_instr_num;
// 如果可以会尽可能给一个IQ分配满4个指令，因为这样可以增加内部pre-wakeup的概率（大概可以）
// 如果没有分配，则对应的 issue_valid也要置0
assign total_simple_iq_input_instr_num = (
    (rename_output_vec[0].issue_distr_direction == 2'b00) +
    (rename_output_vec[1].issue_distr_direction == 2'b00) +
    (rename_output_vec[2].issue_distr_direction == 2'b00) +
    (rename_output_vec[3].issue_distr_direction == 2'b00)
);
assign complex_iq_input_instr_num = (
    (rename_output_vec[0].issue_distr_direction == 2'b01) +
    (rename_output_vec[1].issue_distr_direction == 2'b01) +
    (rename_output_vec[2].issue_distr_direction == 2'b01) +
    (rename_output_vec[3].issue_distr_direction == 2'b01)
);
assign lsu_iq_input_instr_num = (
    (rename_output_vec[0].issue_distr_direction == 2'b10) +
    (rename_output_vec[1].issue_distr_direction == 2'b10) +
    (rename_output_vec[2].issue_distr_direction == 2'b10) +
    (rename_output_vec[3].issue_distr_direction == 2'b10)
);
// 还要有每个iq input instr的mask
logic [3:0] total_iq_input_instr_mask;
logic [3:0] complex_iq_input_instr_mask;
logic [3:0] lsu_iq_input_instr_mask;
assign total_iq_input_instr_mask = {
    (rename_output_vec[0].issue_distr_direction == 2'b00),
    (rename_output_vec[1].issue_distr_direction == 2'b00),
    (rename_output_vec[2].issue_distr_direction == 2'b00),
    (rename_output_vec[3].issue_distr_direction == 2'b00)
};
assign complex_iq_input_instr_mask = {
    (rename_output_vec[0].issue_distr_direction == 2'b01),
    (rename_output_vec[1].issue_distr_direction == 2'b01),
    (rename_output_vec[2].issue_distr_direction == 2'b01),
    (rename_output_vec[3].issue_distr_direction == 2'b01)
};
assign lsu_iq_input_instr_mask = {
    (rename_output_vec[0].issue_distr_direction == 2'b10),
    (rename_output_vec[1].issue_distr_direction == 2'b10),
    (rename_output_vec[2].issue_distr_direction == 2'b10),
    (rename_output_vec[3].issue_distr_direction == 2'b10)
};

logic [2:0] simple_iq_0_left_entry_cnt;
logic [2:0] simple_iq_1_left_entry_cnt;
logic [2:0] complex_iq_left_entry_cnt;
logic [2:0] lsu_iq_left_entry_cnt;

// 为了平衡两个simple IQ的entry数量，这里会定义一个每个周期flip的变量表示优先分配给哪个IQ
logic simple_IQ_distr_flip;
always_ff @(posedge aclk) begin
    if(!rst_n) begin
        simple_IQ_distr_flip <= 1'b0;
    end else begin
        simple_IQ_distr_flip <= ~simple_IQ_distr_flip;
    end
end
// 结合上面的逻辑来分配IQ空位数量
// 首先确定是否能分配
// 这里能分配的条件是：
// 1. 存在一个simple IQ中有空位
assign issue_ready = (
    ((simple_iq_0_left_entry_cnt >= total_simple_iq_input_instr_num) ||
    (simple_iq_1_left_entry_cnt >= total_simple_iq_input_instr_num)) 
    && (complex_iq_left_entry_cnt >= complex_iq_input_instr_num)
    && (lsu_iq_left_entry_cnt >= lsu_iq_input_instr_num)
);
// 用于映射 rename_output 到各个 IQ 输入结构体的 task 定义
// 映射到 Simple IQ 输入结构体
task automatic map_rename_to_simple_iq(
    input rename_output_t rename_output,
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index,
    input logic [31:0] fetch_start_pc,
    // input logic pred_taken,
    // input logic [1:0] pred_cut_pos,
    // input logic [1:0] instr_pos,
    output simple_single_instr_info_t result
);
    result = '0;
    
    result.valid = rename_output.valid;
    result.rob_entry_index = rob_entry_index;
    result.gen_op_type = rename_output.gen_op_type;
    result.spec_op_type = rename_output.spec_op_type;
    result.imm = rename_output.imm;
    result.imm_enable = rename_output.imm_enable;
    result.imm_sign_extend = rename_output.imm_sign_extend;
    result.preg_rd = rename_output.preg_rd;
    result.preg_rj = rename_output.preg_rj;
    result.preg_rk = rename_output.preg_rk;
    result.reg_rd_exist = rename_output.rd_exist;
    result.reg_rj_exist = rename_output.rj_exist;
    result.reg_rk_exist = rename_output.rk_exist;
    result.rj_ready = rename_output.preg_rj_ready;
    result.rk_ready = rename_output.preg_rk_ready;
    result.pc = rename_output.pc;
    result.fetch_start_pc = fetch_start_pc;
    result.pred_taken = rename_output.pred_taken;
    result.pred_cut_pos = rename_output.pred_cut_pos;
    result.instr_pos = rename_output.instr_pos;
    result.pred_jump_pc = rename_output.pred_jump_target_pc;
endtask

// 映射到 Complex IQ 输入结构体
task automatic map_rename_to_complex_iq(
    input rename_output_t rename_output,
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index,
    output complex_single_instr_info_t result
);
    result = '0;
    
    result.valid = rename_output.valid;
    result.rob_entry_index = rob_entry_index;
    result.gen_op_type = rename_output.gen_op_type;
    result.spec_op_type = rename_output.spec_op_type;
    result.imm = rename_output.imm;
    result.imm_enable = rename_output.imm_enable;
    result.rob_entry = rob_entry_index;
    result.preg_rd = rename_output.preg_rd;
    result.preg_rj = rename_output.preg_rj;
    result.preg_rk = rename_output.preg_rk;
    result.reg_rd_exist = rename_output.rd_exist;
    result.reg_rj_exist = rename_output.rj_exist;
    result.reg_rk_exist = rename_output.rk_exist;
    result.rj_ready = rename_output.preg_rj_ready;
    result.rk_ready = rename_output.preg_rk_ready;
endtask

// 映射到 LSU IQ 输入结构体
task automatic map_rename_to_lsu_iq(
    input rename_output_t rename_output,
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_index,
    output lsu_single_instr_info_t result
);
    result = '0;
    result.valid = rename_output.valid;
    result.gen_op_type = rename_output.gen_op_type;
    result.spec_op_type = rename_output.spec_op_type;
    result.store_or_load = rename_output.store_or_load;
    result.bar_type = rename_output.bar_type;
    result.imm = rename_output.imm;
    result.imm_enable = rename_output.imm_enable;
    result.arch_rd_index = rename_output.rd_arch_index;
    result.rob_entry = rob_entry_index;
    result.preg_rd = rename_output.preg_rd;
    result.preg_rj = rename_output.preg_rj;
    result.preg_rk = rename_output.preg_rk;
    result.reg_rd_exist = rename_output.rd_exist;
    result.reg_rj_exist = rename_output.rj_exist;
    result.reg_rk_exist = rename_output.rk_exist;
    result.rj_ready = rename_output.preg_rj_ready;
    result.rk_ready = rename_output.preg_rk_ready;
    result.rd_ready = rename_output.preg_rd_ready;
endtask

// 通用的 pos_to_pos 映射 task
// 根据 input_instr_mask 生成 pos_to_pos_map
task automatic generate_pos_to_pos_map(
    input logic [3:0] input_instr_mask,
    output logic [1:0] pos_to_pos_map [3:0]
);
    pos_to_pos_map = '0;
    case(input_instr_mask)
        4'b0001: begin
            pos_to_pos_map[0] = 2'd0;
        end
        4'b0010: begin
            pos_to_pos_map[0] = 2'd1;
        end
        4'b0100: begin
            pos_to_pos_map[0] = 2'd2;
        end
        4'b1000: begin
            pos_to_pos_map[0] = 2'd3;
        end
        4'b0011: begin
            pos_to_pos_map[0] = 2'd0;
            pos_to_pos_map[1] = 2'd1;
        end
        4'b0101: begin
            pos_to_pos_map[0] = 2'd0;
            pos_to_pos_map[1] = 2'd2;
        end
        4'b0110: begin
            pos_to_pos_map[0] = 2'd1;
            pos_to_pos_map[1] = 2'd2;
        end
        4'b1001: begin
            pos_to_pos_map[0] = 2'd0;
            pos_to_pos_map[1] = 2'd3;
        end
        4'b1010: begin
            pos_to_pos_map[0] = 2'd1;
            pos_to_pos_map[1] = 2'd3;
        end
        4'b1100: begin
            pos_to_pos_map[0] = 2'd2;
            pos_to_pos_map[1] = 2'd3;
        end
        4'b1101: begin
            pos_to_pos_map[0] = 2'd0;
            pos_to_pos_map[1] = 2'd2;
        end
        4'b1110: begin
            pos_to_pos_map[0] = 2'd1;
            pos_to_pos_map[1] = 2'd2;
        end
        4'b1111: begin
            pos_to_pos_map[0] = 2'd0;
            pos_to_pos_map[1] = 2'd1;
        end
        default: begin
            pos_to_pos_map = '0;
        end
    endcase
endtask

// 然后首先确定complex iq和lsu iq的分配，这两个直接分配就行，不涉及内部二分的问题
// 为了尽可能减少代码的复杂度，这里会先由case进行 pos->pos的分配，后面再直接调用map函数
logic [1:0] complex_iq_pos_to_pos_map [3:0];
logic [1:0] lsu_iq_pos_to_pos_map [3:0];
// Complex IQ 的 pos_to_pos 映射
always_comb begin
    generate_pos_to_pos_map(complex_iq_input_instr_mask, complex_iq_pos_to_pos_map);
end
// Complex IQ 的指令映射
always_comb begin
    complex_iq_instr_info = '0;
    
    for(int i=0; i<4; i=i+1) begin
        if(i<complex_iq_input_instr_num) begin
            map_rename_to_complex_iq(
                rename_output_vec[complex_iq_pos_to_pos_map[i]],
                rob_alloc_index_start + complex_iq_pos_to_pos_map[i],
                complex_iq_instr_info[i]
            );
        end
    end
end
// LSU IQ 的 pos_to_pos 映射
always_comb begin
    generate_pos_to_pos_map(lsu_iq_input_instr_mask, lsu_iq_pos_to_pos_map);
end
// LSU IQ 的指令映射
always_comb begin
    lsu_iq_instr_info = '0;
    
    for(int i=0; i<4; i=i+1) begin
        if(i<lsu_iq_input_instr_num) begin
            map_rename_to_lsu_iq(
                rename_output_vec[lsu_iq_pos_to_pos_map[i]],
                rob_alloc_index_start + lsu_iq_pos_to_pos_map[i],
                lsu_iq_instr_info[i]
            );
        end
    end
end

// 然后是simple IQ的分配，首先要考虑能否全部分配给一个simple IQ
// 如果不行再考虑每个要分配多少个
logic [1:0] simple_iq_0_pos_to_pos_map [3:0];
logic [1:0] simple_iq_1_pos_to_pos_map [3:0]; 
logic simple_iq_final_distr; // 0表示最终分配给0，1表示最终分配给1
always_comb begin
    simple_iq_0_pos_to_pos_map = '0;
    simple_iq_1_pos_to_pos_map = '0;
    if(!simple_IQ_distr_flip) begin // 优先给 simple iq 0 分配
        for(int i=0; i<4; i=i+1) begin
            if(i<total_simple_iq_input_instr_num) begin
                map_rename_to_simple_iq(
                    rename_output_vec[simple_iq_0_pos_to_pos_map[i]],
                    rob_alloc_index_start + simple_iq_0_pos_to_pos_map[i],
                    simple_iq_0_instr_info[i]
                );
            end else begin
                map_rename_to_simple_iq(
                    rename_output_vec[simple_iq_1_pos_to_pos_map[i]],
                    rob_alloc_index_start + simple_iq_1_pos_to_pos_map[i],
                    simple_iq_1_instr_info[i]
                );
            end
        end
    end else begin
        for(int i=0; i<4; i=i+1) begin
            if(i<total_simple_iq_input_instr_num) begin
                map_rename_to_simple_iq(
                    rename_output_vec[simple_iq_1_pos_to_pos_map[i]],
                    rob_alloc_index_start + simple_iq_1_pos_to_pos_map[i],
                    simple_iq_1_instr_info[i]
                );
            end else begin
                map_rename_to_simple_iq(
                    rename_output_vec[simple_iq_0_pos_to_pos_map[i]],
                    rob_alloc_index_start + simple_iq_0_pos_to_pos_map[i],
                    simple_iq_0_instr_info[i]
                );
            end
        end
    end
end

// 然后是simple IQ的指令映射
always_comb begin
    simple_iq_0_instr_info = '0;
    simple_iq_1_instr_info = '0;

    for(int i=0; i<4; i=i+1) begin
        if(simple_iq_final_distr) begin // 最终分配给 IQ 0
            map_rename_to_simple_iq(
                rename_output_vec[simple_iq_0_pos_to_pos_map[i]],
                rob_alloc_index_start + simple_iq_0_pos_to_pos_map[i],
                simple_iq_0_instr_info[i]
            );
        end else begin // 最终分配给 IQ 1
            map_rename_to_simple_iq(
                rename_output_vec[simple_iq_1_pos_to_pos_map[i]],
                rob_alloc_index_start + simple_iq_1_pos_to_pos_map[i],
                simple_iq_1_instr_info[i]
            );
        end
    end
end


// IQ输入输出的结构体
// 输入
simple_single_instr_info_t simple_iq_0_instr_info [3:0];
simple_single_instr_info_t simple_iq_1_instr_info [3:0];
complex_single_instr_info_t complex_iq_instr_info [3:0];
lsu_single_instr_info_t lsu_iq_instr_info [3:0];
// 输出
simple_issue_queue_issued_info_t simple_iq_0_issued_info;
simple_issue_queue_issued_info_t simple_iq_1_issued_info;
complex_issue_queue_issued_info_t complex_iq_issued_info;
lsu_issue_queue_issued_info_t lsu_iq_issued_info;

// 和忙表的交互
logic [3:0] begin_exec_valid_vec;
assign begin_exec_valid_vec = {
    simple_iq_0_issued_info.valid && simple_iq_0_issued_info.issued_reg_rd_exist,
    simple_iq_1_issued_info.valid && simple_iq_1_issued_info.issued_reg_rd_exist,
    complex_iq_issued_info.valid && complex_iq_issued_info.issued_reg_rd_exist,
    lsu_iq_issued_info.valid && lsu_iq_issued_info.issued_reg_rd_exist
};

logic [`PREG_INDEX_WIDTH-1:0] begin_exec_preg_index_vec;
assign begin_exec_preg_index_vec = {
    simple_iq_0_issued_info.issued_preg_rd,
    simple_iq_1_issued_info.issued_preg_rd,
    complex_iq_issued_info.issued_preg_rd,
    lsu_iq_issued_info.issued_preg_rd
};

logic [3:0] wb_valid_vec;
assign wb_valid_vec = {
    simple_fu_0_result_valid && simple_fu_0_preg_rd_exist,
    simple_fu_1_result_valid && simple_fu_1_preg_rd_exist,
    complex_fu_result_valid && complex_fu_preg_rd_exist,
    lsu_fu_result_valid && lsu_fu_preg_rd_exist
};
logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index_vec;
assign wb_rd_index_vec = {
    simple_fu_0_wb_rd_index,
    simple_fu_1_wb_rd_index,
    complex_fu_wb_rd_index,
    lsu_wb_rd_index
}

simple_issue_queue simple_issue_queue_0(
    .clk(aclk),
    .rst_n(rst_n),
    .rename_valid(rename_valid_out),
    .fu_ready(simple_fu_0_ready),
    .rename_instr_num(simple_iq_0_input_instr_num),
    .left_IQ_entry_cnt(simple_iq_0_left_entry_cnt),
    .issue_ready(simple_iq_0_ready),
    .issue_valid(simple_iq_0_issue_valid),

    .instr_info(simple_iq_0_instr_info), // 输入指令信息结构体数组

    .write_back_rd_exist_vec(),
    .write_back_rd_index(),

    .issued_info(simple_iq_0_issued_info)
);

simple_issue_queue simple_issue_queue_1(
    .clk(aclk),
    .rst_n(rst_n),
    .rename_valid(rename_valid_out),
    .fu_ready(simple_fu_1_ready),
    .rename_instr_num(simple_iq_1_input_instr_num),
    .left_IQ_entry_cnt(simple_iq_1_left_entry_cnt),
    .issue_ready(simple_iq_1_ready),
    .issue_valid(simple_iq_1_issue_valid),

    .instr_info(simple_iq_1_instr_info), // 输入指令信息结构体数组

    .write_back_rd_exist_vec(),
    .write_back_rd_index_vec(),

    .issued_info(simple_iq_1_issued_info)
);

complex_issue_queue complex_issue_queue(
    .clk(aclk),
    .rst_n(rst_n),
    .rename_valid(rename_valid_out),
    .fu_ready(complex_fu_ready),
    .rename_instr_num(complex_iq_input_instr_num),
    .left_IQ_entry_cnt(complex_iq_left_entry_cnt),
    .issue_ready(complex_iq_ready),
    .issue_valid(complex_iq_issue_valid),

    .instr_info(complex_iq_instr_info), // 输入指令信息结构体数组

    .write_back_rd_exist_vec(),
    .write_back_rd_index_vec(),

    .issued_info(complex_iq_issued_info)
);

LSU_issue_queue LSU_issue_queue(
    .clk(aclk),
    .rst_n(rst_n),
    .rename_valid(rename_valid_out),
    .fu_ready(lsu_fu_ready),
    .rename_instr_num(lsu_iq_input_instr_num),
    .left_IQ_entry_cnt(lsu_iq_left_entry_cnt),
    .issue_ready(lsu_iq_ready),
    .issue_valid(lsu_iq_issue_valid),

    .instr_info(lsu_iq_instr_info), // 输入指令信息结构体数组

    .write_back_rd_exist_vec(),
    .write_back_rd_index_vec(),

    .issued_info(lsu_iq_issued_info)
);

logic simple_fu_0_ready;
logic simple_fu_1_ready;
logic complex_fu_ready;
logic lsu_fu_ready;

simple_fu simple_fu_0(
    .clk(aclk),
    .rst_n(rst_n),
    .issued_info(simple_iq_0_issued_info),
    .issue_valid(simple_iq_0_issue_valid),
    .fu_ready(simple_fu_0_ready),

    .prf_read_rj_index(simple_fu_0_output_prf_read_rj_index),
    .prf_read_rk_index(simple_fu_0_output_prf_read_rk_index),

    .rj_val(simple_fu_0_prf_readback_rj_val),
    .rk_val(simple_fu_0_prf_readback_rk_val),

    .result(simple_fu_0_result),
    .wb_rd_index(simple_fu_0_wb_rd_index),
    .bj_taken(simple_fu_0_bj_taken),
    .real_cut_pos(simple_fu_0_real_cut_pos),
    .pred_wrong(simple_fu_0_pred_wrong),
    .exception(simple_fu_0_exception),
    .result_valid(simple_fu_0_result_valid),
    .wb_rob_entry_index(simple_fu_0_wb_rob_entry_index),
    .preg_rd_exist(simple_fu_0_preg_rd_exist)
);

simple_fu simple_fu_1(
    .clk(aclk),
    .rst_n(rst_n),
    .issued_info(simple_iq_1_issued_info),
    .issue_valid(simple_iq_1_issue_valid),
    .fu_ready(simple_fu_1_ready),

    .prf_read_rj_index(simple_fu_1_output_prf_read_rj_index),
    .prf_read_rk_index(simple_fu_1_output_prf_read_rk_index),

    .rj_val(simple_fu_1_prf_readback_rj_val),
    .rk_val(simple_fu_1_prf_readback_rk_val),

    .result(simple_fu_1_result),
    .wb_rd_index(simple_fu_1_wb_rd_index),
    .bj_taken(simple_fu_1_bj_taken),
    .real_cut_pos(simple_fu_1_real_cut_pos),
    .pred_wrong(simple_fu_1_pred_wrong),
    .exception(simple_fu_1_exception),
    .result_valid(simple_fu_1_result_valid),
    .wb_rob_entry_index(simple_fu_1_wb_rob_entry_index),
    .preg_rd_exist(simple_fu_1_preg_rd_exist)
);

complex_fu complex_fu(
    .clk(aclk),
    .rst_n(rst_n),
    .issued_info(complex_iq_issued_info),
    .issue_valid(complex_iq_issue_valid),
    .fu_ready(complex_fu_ready),

    .prf_read_rj_index(complex_fu_output_prf_read_rj_index),
    .prf_read_rk_index(complex_fu_output_prf_read_rk_index),

    .rj_val(complex_fu_prf_readback_rj_val),
    .rk_val(complex_fu_prf_readback_rk_val),

    .result(complex_fu_result),
    .wb_rd_index(complex_fu_wb_rd_index),
    .wb_rob_entry_index(complex_fu_wb_rob_entry_index),
    .result_valid(complex_fu_result_valid),
    .preg_rd_exist(complex_fu_preg_rd_exist)
);

LSU lsu(
    .clk(aclk),
    .rst_n(rst_n),
    .issue_valid(lsu_iq_issue_valid),
    .fu_ready(lsu_fu_ready),

    .prf_read_rj_index(lsu_fu_output_prf_read_rj_index),
    .prf_read_rk_index(lsu_fu_output_prf_read_rk_index),

    .rj_val(lsu_fu_prf_readback_rj_val),
    .rk_val(lsu_fu_prf_readback_rk_val),


    .load_data(),
    .complete_rob_entry_index(),
    .complete_valid(),
    .exception(),
    .wb_rd_valid(lsu_wb_preg_rd_valid),
    .wb_preg_rd_index(lsu_wb_rd_index),

    // 这里应该还有一个dcache的有效信号
    .cache_req(),
    .cache_resp(),

    // 下面还有一些其他的，和cache的交互

    // 下面是store retire的交互
    .store_retire_valid(),
    .write_back_index_to_IQ(),
    .write_back_index_valid(),
    // 先这样勉强写着，后面再改....
);

ROB rob(
    .clk(aclk),
    .rst_n(rst_n),
    .flush(rob_flush),
    .stall(rob_stall),

    .write_in_num(),
    .rob_entry_index_start(),
    .alloc_success(),
    
    .rob_instr_info(),

    .complete_entry_index_vec(),
    .complete_valid_vec(),
    .fu_exception_vec(),
    .fu_jump_pc_vec(),
    .fu_jump_pc_pred_wrong(),

    .store_retire_valid(),
    .rob_entry_exception(),

    .recover_valid(),
    .retire_aRAT_write_arch_rd_index_vec(),
    .recover_aRAT_write_preg_index_vec(),

);



// csr csr(

// );

endmodule