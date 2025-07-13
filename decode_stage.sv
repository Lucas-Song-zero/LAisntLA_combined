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
    
    // Output to rename stage (up to 4 decoded instructions)
    output decoded_instr_t decoded_instrs [3:0],  // 4个解码指令的结构体数组
    
    // Handshake with rename stage
    input logic rename_ready,         // Ready signal from rename stage

);
// 需要增加一些信号，比如exception , store_or_load
// TODO


// Internal signals for individual decoders
// Each decoder outputs its decoded instruction
logic [3:0] decoder_gen_op_type [3:0];      // General operation types from decoders
logic [4:0] decoder_spec_op_type [3:0];     // Specific operation types from decoders
logic [25:0] decoder_imm [3:0];             // Immediate values from decoders
logic decoder_imm_enable [3:0];             // Immediate enables from decoders
logic [4:0] decoder_reg_rd [3:0];           // Destination registers from decoders
logic [4:0] decoder_reg_rj [3:0];           // Source register 1 from decoders
logic [4:0] decoder_reg_rk [3:0];           // Source register 2 from decoders
logic decoder_reg_rd_exist [3:0];           // Destination register exists from decoders
logic decoder_reg_rj_exist [3:0];           // Source register 1 exists from decoders
logic decoder_reg_rk_exist [3:0];           // Source register 2 exists from decoders
logic decoder_valid [3:0];                  // Valid signals from decoders 这里的意思是指令有效，并非不存在的指令
// Pipeline control signals
logic [3:0] decoder_busy;                   // Busy flags for each decoder slot
logic [3:0] stage_valid;                    // Valid flags for output stage

// 后面新增了一些信号
logic 

// Instantiate 4 individual decoders
// Each decoder handles one instruction independently
decoder decoder_inst_0 (
    .instr(instr_0),
    .gen_op_type(decoder_gen_op_type[0]),
    .spec_op_type(decoder_spec_op_type[0]),
    .imm(decoder_imm[0]),
    .imm_enable(decoder_imm_enable[0]),
    .reg_rd(decoder_reg_rd[0]),
    .reg_rj(decoder_reg_rj[0]),
    .reg_rk(decoder_reg_rk[0]),
    .reg_rd_exist(decoder_reg_rd_exist[0]),
    .reg_rj_exist(decoder_reg_rj_exist[0]),
    .reg_rk_exist(decoder_reg_rk_exist[0]),
    .valid(decoder_valid[0])
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
    .valid(decoder_valid[1])
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
    .valid(decoder_valid[2])
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
    .valid(decoder_valid[3])
);

// Ready signal logic - decoder stage is ready when all slots are not busy and next stage is ready
// Note: decoder_ready is sent to fetch stage to indicate decoder can accept new instructions
always_comb begin
    decoder_ready = !(|decoder_busy) && rename_ready;
end

// Main sequential decoder stage logic
// This block manages the pipeline and handshake protocol for all 4 decoder slots
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || flush) begin
        // Reset all outputs and control signals to default values
        for (int i = 0; i < 4; i = i + 1) begin
            decoded_instrs[i] <= 0;
        end
        decoder_busy <= 4'b0000;
        stage_valid <= 4'b0000;
    end else begin
        // Process new instructions when enabled and ready
        if (ifu_valid && decoder_ready) begin
            // Process each instruction slot independently
            for (int i = 0; i < 4; i = i + 1) begin
                // Only process if fetch has valid instruction and decoder slot is not busy
                if (fetch_valid[i] && !decoder_busy[i] && decoder_valid[i]) begin
                    // Set busy flag for this slot
                    decoder_busy[i] <= 1'b1;
                    stage_valid[i] <= 1'b1;
                    
                    // Assign decoded outputs directly using array indexing
                    decoded_instrs[i].gen_op_type <= decoder_gen_op_type[i];
                    decoded_instrs[i].spec_op_type <= decoder_spec_op_type[i];
                    decoded_instrs[i].imm <= decoder_imm[i];
                    decoded_instrs[i].imm_enable <= decoder_imm_enable[i];
                    decoded_instrs[i].reg_rd <= decoder_reg_rd[i];
                    decoded_instrs[i].reg_rj <= decoder_reg_rj[i];
                    decoded_instrs[i].reg_rk <= decoder_reg_rk[i];
                    decoded_instrs[i].reg_rd_exist <= decoder_reg_rd_exist[i];
                    decoded_instrs[i].reg_rj_exist <= decoder_reg_rj_exist[i];
                    decoded_instrs[i].reg_rk_exist <= decoder_reg_rk_exist[i];
                    decoded_instrs[i].valid <= 1'b1;
                end
            end
        end
        // Handle handshake completion - clear busy flags when next stage accepts data
        else if (next_stage_ready) begin
            // Clear valid signals and busy flags for slots that were valid
            for (int i = 0; i < 4; i = i + 1) begin
                if (decoded_instrs[i].valid) begin
                    decoded_instrs[i].valid <= 1'b0;
                    decoder_busy[i] <= 1'b0;
                    stage_valid[i] <= 1'b0;
                end
            end
        end
        // When not enabled or not ready, explicitly maintain current state
        else begin
            // Explicitly maintain all outputs to ensure proper behavior
            // This is important for synthesis and simulation consistency
            for (int i = 0; i < 4; i = i + 1) begin
                decoded_instrs[i].gen_op_type <= decoded_instrs[i].gen_op_type;
                decoded_instrs[i].spec_op_type <= decoded_instrs[i].spec_op_type;
                decoded_instrs[i].imm <= decoded_instrs[i].imm;
                decoded_instrs[i].imm_enable <= decoded_instrs[i].imm_enable;
                decoded_instrs[i].reg_rd <= decoded_instrs[i].reg_rd;
                decoded_instrs[i].reg_rj <= decoded_instrs[i].reg_rj;
                decoded_instrs[i].reg_rk <= decoded_instrs[i].reg_rk;
                decoded_instrs[i].reg_rd_exist <= decoded_instrs[i].reg_rd_exist;
                decoded_instrs[i].reg_rj_exist <= decoded_instrs[i].reg_rj_exist;
                decoded_instrs[i].reg_rk_exist <= decoded_instrs[i].reg_rk_exist;
                decoded_instrs[i].valid <= decoded_instrs[i].valid;
            end
            decoder_busy <= decoder_busy;
            stage_valid <= stage_valid;
        end
    end
end

endmodule 