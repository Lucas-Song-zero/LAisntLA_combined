`include "mycpu.h"

module decoder_stage (
    // Clock and control inputs
    input logic clk,             // Clock signal for sequential operation
    input logic rst_n,           // Active-low reset signal
    input logic ifu_valid,      // ifu取指有效
    input logic flush,   // Flush output signal to clear pipeline
    
    // Input from fetch stage (up to 4 instructions)
    input logic [31:0] instr_0,      // Instruction 0 from fetch
    input logic [31:0] instr_1,      // Instruction 1 from fetch
    input logic [31:0] instr_2,      // Instruction 2 from fetch
    input logic [31:0] instr_3,      // Instruction 3 from fetch
    input logic [3:0] fetch_valid,   // Valid signals for each instruction
    output logic decoder_ready,      // Decoder stage ready signal to fetch
    input logic [31:0] start_pc_in,     // 取指开始的pc
    output logic [31:0] start_pc_out,
    input logic pred_taken,
    output logic pred_taken_decoder_out,
    input logic [1:0] pred_cut_pos, // 如果存在B指令，需要其实际位置和预测的cut_pos进行比较
    output logic [1:0] pred_cut_pos_out, // 如果存在B指令，需要其实际位置和预测的cut_pos进行比较
    input logic [31:0] pred_next_fetch_target_pc, // 预测的下一步取指的pc
    output logic [31:0] pred_next_fetch_targer_pc_out, // 预测的下一步取指的pc
    output logic pred_wrong, // 如果pred_taken = 0,但是B指令存在，说明预测错误
    // 或者发现预测的下一步取指地址和实际的B跳转不一致，也是pred_wrong
    output logic [1:0] real_cut_pos_out, // 如果有B指令，则输出实际的cut_pos
    output logic [31:0] real_jump_target_pc, // 如果有B指令，则输出实际的跳转目标pc

    // Output to rename stage (up to 4 decoded instructions)
    output decoded_instr_t decoded_instrs [3:0],  // 4个解码指令的结构体数组
    output logic [3:0] decoded_instr_valid, // 表明数组中四个依次是否有效
    output logic decoder_valid, // 表明输出是有效的，但是要看上面才能确定到底是解码了几个指令
    // Handshake with rename stage
    input logic rename_ready,         // Ready signal from rename stage
    output logic [2:0] rename_rd_request    // decode解码出来的rename所需分配的new preg数量，要和freelist counter相比从而决定是否rename_ready
);

always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || flush) begin
        start_pc_out <= 32'b0;
        pred_taken_decoder_out <= 1'b0;
        pred_cut_pos_out <= 2'b0; 
        pred_next_fetch_targer_pc_out <= 32'b0;
    end else begin
        start_pc_out <= start_pc_in;
        pred_taken_decoder_out <= pred_taken;
        pred_cut_pos_out <= pred_cut_pos;
        pred_next_fetch_targer_pc_out <= pred_next_fetch_target_pc;
    end
end

// Internal signals for individual decoders
// Each decoder outputs its decoded instruction
logic [3:0] decoder_gen_op_type [3:0];      // General operation types from decoders
logic [4:0] decoder_spec_op_type [3:0];     // Specific operation types from decoders
logic [25:0] decoder_imm [3:0];             // Immediate values from decoders
logic decoder_imm_enable [3:0];             // Immediate enables from decoders
logic decoder_imm_sign_extend [3:0];        // Immediate sign extends from decoders
logic [4:0] decoder_reg_rd [3:0];           // Destination registers from decoders
logic [4:0] decoder_reg_rj [3:0];           // Source register 1 from decoders
logic [4:0] decoder_reg_rk [3:0];           // Source register 2 from decoders
logic decoder_reg_rd_exist [3:0];           // Destination register exists from decoders
logic decoder_reg_rj_exist [3:0];           // Source register 1 exists from decoders
logic decoder_reg_rk_exist [3:0];           // Source register 2 exists from decoders
logic decoder_valid [3:0];                  // Valid signals from decoders 这里的意思是指令有效，并非不存在的指令
logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] decoder_exception_state [3:0]; // 异常状态，主要是CSR指令或者不存在的指令
logic decoder_store_or_load [3:0]; // 是否是store或者load指令
logic [1:0] decoder_bar_type [3:0]; // BAR指令的类型 00--normal 01--dbar 10--ibar
logic decoder_completion_bit [3:0]; // 比如PRELD指令需要发射就完成
logic [1:0] decoder_issue_distr_direction [3:0]; // 决定指令要发射到哪个issue队列
logic [3:0] B_exist_vec; // 表明B指令是否存在
logic [31:0] B_jump_offset [3:0]; // 表明B指令的跳转偏移量
logic [3:0] branch_exist_vec; // 表明分支指令是否存在，如果不存在也就是自然是不能跳转，从而如果跳转就是错的

// handshake
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || flush) begin
        decoded_instr_valid <= 4'b0000;
        decoder_valid <= 1'b0;
    end else begin
        if (ifu_valid && !pred_wrong) begin
            decoded_instr_valid <= fetch_valid;
            decoder_valid <= 1'b1;
        end else begin
            decoded_instr_valid <= 4'b0000;
            decoder_valid <= 1'b0;
        end
    end
end

always_comb begin
    decoder_ready = rename_ready;
end

// Instantiate 4 individual decoders
// Each decoder handles one instruction independently
decoder decoder_inst_0 (
    .instr(instr_0),
    .gen_op_type(decoder_gen_op_type[0]),
    .spec_op_type(decoder_spec_op_type[0]),
    .imm(decoder_imm[0]),
    .imm_enable(decoder_imm_enable[0]),
    .imm_sign_extend(decoder_imm_sign_extend[0]),
    .reg_rd(decoder_reg_rd[0]),
    .reg_rj(decoder_reg_rj[0]),
    .reg_rk(decoder_reg_rk[0]),
    .reg_rd_exist(decoder_reg_rd_exist[0]),
    .reg_rj_exist(decoder_reg_rj_exist[0]),
    .reg_rk_exist(decoder_reg_rk_exist[0]),
    .valid(decoder_valid[0]),
    .exception_state(decoder_exception_state[0]),
    .store_or_load(decoder_store_or_load[0]),
    .bar_type(decoder_bar_type[0]),
    .completion_bit(decoder_completion_bit[0]),
    .issue_distr_direction(decoder_issue_distr_direction[0]),
    .B_exist_vec(B_exist_vec[0]),
    .B_jump_offset(B_jump_offset[0]),
    .branch_exist_vec(branch_exist_vec[0])
);

decoder decoder_inst_1 (
    .instr(instr_1),
    .gen_op_type(decoder_gen_op_type[1]),
    .spec_op_type(decoder_spec_op_type[1]),
    .imm(decoder_imm[1]),
    .imm_enable(decoder_imm_enable[1]),
    .reg_rd(decoder_reg_rd[1]),
    .reg_rj(decoder_reg_rj[1]),
    .reg_rk(decoder_reg_rk[1]),
    .reg_rd_exist(decoder_reg_rd_exist[1]),
    .reg_rj_exist(decoder_reg_rj_exist[1]),
    .reg_rk_exist(decoder_reg_rk_exist[1]),
    .valid(decoder_valid[1]),
    .exception_state(decoder_exception_state[1]),
    .store_or_load(decoder_store_or_load[1]),
    .bar_type(decoder_bar_type[1]),
    .completion_bit(decoder_completion_bit[1]),
    .issue_distr_direction(decoder_issue_distr_direction[1]),
    .B_exist_vec(B_exist_vec[1]),
    .B_jump_offset(B_jump_offset[1]),
    .branch_exist_vec(branch_exist_vec[1])
);

decoder decoder_inst_2 (
    .instr(instr_2),
    .gen_op_type(decoder_gen_op_type[2]),
    .spec_op_type(decoder_spec_op_type[2]),
    .imm(decoder_imm[2]),
    .imm_enable(decoder_imm_enable[2]),
    .reg_rd(decoder_reg_rd[2]),
    .reg_rj(decoder_reg_rj[2]),
    .reg_rk(decoder_reg_rk[2]),
    .reg_rd_exist(decoder_reg_rd_exist[2]),
    .reg_rj_exist(decoder_reg_rj_exist[2]),
    .reg_rk_exist(decoder_reg_rk_exist[2]),
    .valid(decoder_valid[2]),
    .exception_state(decoder_exception_state[2]),
    .store_or_load(decoder_store_or_load[2]),
    .bar_type(decoder_bar_type[2]),
    .completion_bit(decoder_completion_bit[2]),
    .issue_distr_direction(decoder_issue_distr_direction[2]),
    .B_exist_vec(B_exist_vec[2]),
    .B_jump_offset(B_jump_offset[2]),
    .branch_exist_vec(branch_exist_vec[2])
);

decoder decoder_inst_3 (
    .instr(instr_3),
    .gen_op_type(decoder_gen_op_type[3]),
    .spec_op_type(decoder_spec_op_type[3]),
    .imm(decoder_imm[3]),
    .imm_enable(decoder_imm_enable[3]),
    .reg_rd(decoder_reg_rd[3]),
    .reg_rj(decoder_reg_rj[3]),
    .reg_rk(decoder_reg_rk[3]),
    .reg_rd_exist(decoder_reg_rd_exist[3]),
    .reg_rj_exist(decoder_reg_rj_exist[3]),
    .reg_rk_exist(decoder_reg_rk_exist[3]),
    .valid(decoder_valid[3]),
    .exception_state(decoder_exception_state[3]),
    .store_or_load(decoder_store_or_load[3]),
    .bar_type(decoder_bar_type[3]),
    .completion_bit(decoder_completion_bit[3]),
    .issue_distr_direction(decoder_issue_distr_direction[3]),
    .B_exist_vec(B_exist_vec[3]),
    .B_jump_offset(B_jump_offset[3]),
    .branch_exist_vec(branch_exist_vec[3])
);

// 计算rename_rd_request 和 判断分支预测是否错误
always_comb begin
    real_jump_target_pc = 32'b0;
    cut_pos_out = 2'b0;
    for(int i = 0; i < 4; i = i + 1) begin
        if(B_exist_vec[i]) begin
            real_jump_target_pc = start_pc_in + 4 * i + B_jump_offset[i];
            cut_pos_out = i+1;
        end
    end
end

always_comb begin
    pred_wrong = 1'b0;
    if(|branch_exist_vec == 1'b0 && pred_taken) begin
        pred_wrong = 1'b1;
    end else begin
        if(pred_taken && (pred_next_fetch_target_pc != real_jump_target_pc || pred_cut_pos != cut_pos_out)) begin
            pred_wrong = 1'b1;
        end
    end
end

always_comb begin
    rename_rd_request = 3'b000;
    for (int i = 0; i < 4; i = i + 1) begin
        if (decoder_reg_rd_exist[i]) begin
            rename_rd_request = rename_rd_request + 1;
        end
    end
end

// Main sequential decoder stage logic
// This block manages the pipeline and handshake protocol for all 4 decoder slots
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || flush) begin
        // Reset all outputs and control signals to default values
        for (int i = 0; i < 4; i = i + 1) begin
            decoded_instrs[i] <= 0;
        end
    end else begin
        // Process new instructions when enabled and ready
        if (ifu_valid && decoder_ready && rename_ready) begin
            // 实际上现在decoder_ready就是rename_ready
            // Process each instruction slot independently
            for (int i = 0; i < 4; i = i + 1) begin
                // Only process if fetch has valid instruction and decoder slot is not busy
                if (fetch_valid[i]) begin
                    // Assign decoded outputs directly using array indexing
                    decoded_instrs[i].gen_op_type <= decoder_gen_op_type[i];
                    decoded_instrs[i].spec_op_type <= decoder_spec_op_type[i];
                    decoded_instrs[i].imm <= decoder_imm[i];
                    decoded_instrs[i].imm_enable <= decoder_imm_enable[i];
                    decoded_instrs[i].imm_sign_extend <= decoder_imm_sign_extend[i];
                    decoded_instrs[i].reg_rd <= decoder_reg_rd[i];
                    decoded_instrs[i].reg_rj <= decoder_reg_rj[i];
                    decoded_instrs[i].reg_rk <= decoder_reg_rk[i];
                    decoded_instrs[i].reg_rd_exist <= decoder_reg_rd_exist[i];
                    decoded_instrs[i].reg_rj_exist <= decoder_reg_rj_exist[i];
                    decoded_instrs[i].reg_rk_exist <= decoder_reg_rk_exist[i];
                    decoded_instrs[i].valid <= 1'b1;
                    decoded_instrs[i].exception_state <= decoder_exception_state[i];
                    decoded_instrs[i].store_or_load <= decoder_store_or_load[i];
                    decoded_instrs[i].bar_type <= decoder_bar_type[i];
                    decoded_instrs[i].completion_bit <= decoder_completion_bit[i];
                    decoded_instrs[i].issue_distr_direction <= decoder_issue_distr_direction[i];
                end
            end
        end
    end
end

endmodule 