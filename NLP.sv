`include "defs.sv"

module NLP(
    input logic clk,
    input logic rst_n,
    input logic flush,
    
    // Prediction interface
    input logic [31:0] pc,
    output logic [31:0] next_fetch_pc,
    output logic taken,
    output logic [1:0] cut_pos, // if branch is at 0-index position and is taken,
    // from 1 to 3 instr should be cut off
    // cut_pos = br_pos + 1 , if br_pos = 3, then cut_pos = 0 meaning no instr needs cut off
    output logic prediction_valid,
    
    // Update interface
    input logic update_valid,        // Valid update signal
    input logic [31:0] update_pc,    // PC to update
    input logic [31:0] target_pc,    // Branch target PC
    input logic update_taken,        // Actual branch result (1=taken, 0=not taken)
    input logic [1:0] update_cut_pos, // Cut position for this branch
    input logic [1:0] update_branch_type   // Branch type (direct/indirect/return/call)
);

// NLP should be composed of uBTB and BHT
// BHT pred whether the pc-indexed fetch group has a taken branch
// uBTB pred next_fetch_pc and cut_pos

// Internal signals for BHT
logic bht_taken;

// Internal signals for uBTB
logic [31:0] ubtb_next_fetch_pc;
logic [1:0] ubtb_cut_pos;
logic ubtb_hit;
logic [1:0] ubtb_branch_type;

// Update control signals
logic bht_update_valid, ubtb_update_valid;

// BHT should be updated for all branches (taken and not taken)
assign bht_update_valid = update_valid;

// uBTB should only be updated for taken branches
assign ubtb_update_valid = update_valid && update_taken;

// BHT instance - predicts whether branch is taken
BHT bht_inst (
    .clk(clk),
    .rst_n(rst_n),
    .flush(flush),
    .pc(pc),
    .taken(bht_taken),
    .update_pc(update_pc),
    .update_taken(update_taken),
    .update_valid(bht_update_valid)
);

// uBTB instance - predicts target address and cut position
uBTB ubtb_inst (
    .clk(clk),
    .rst_n(rst_n),
    .flush(flush),
    .pc(pc),
    .next_fetch_pc(ubtb_next_fetch_pc),
    .cut_pos(ubtb_cut_pos),
    .hit(ubtb_hit),
    .branch_type(ubtb_branch_type),
    .update_valid(ubtb_update_valid),
    .update_pc(update_pc),
    .target_pc(target_pc),
    .update_cut_pos(update_cut_pos),
    .update_branch_type(update_branch_type)
);

// Combined prediction logic
always_comb begin
    if (ubtb_hit) begin
        // uBTB hit - use uBTB prediction for target and cut position
        next_fetch_pc = ubtb_next_fetch_pc;
        cut_pos = ubtb_cut_pos;
        prediction_valid = 1'b1;
        // Use BHT for taken prediction
        taken = bht_taken;
    end else begin
        // uBTB miss - use BHT prediction only
        if (bht_taken) begin
            // BHT predicts taken but no target info
            // This is a fallback case - could use default target or stall
            next_fetch_pc = pc + 16;  // Default sequential (should be improved)
            cut_pos = 2'b00;         // No cut
            prediction_valid = 1'b1; // Incomplete prediction
            taken = bht_taken; // honestly pass the bht prediction(for update)
            // if the bht predicts it taken but actually no entry matched
            // thus we should pretend it as a normal no-branch

        end else begin
            // BHT predicts not taken - sequential execution
            next_fetch_pc = pc + 16;
            cut_pos = 2'b00;
            prediction_valid = 1'b1;
            taken = bht_taken;  // Add missing assignment to prevent latch
        end
    end
end



endmodule