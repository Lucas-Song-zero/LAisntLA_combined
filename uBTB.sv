`include "defs.sv"

module uBTB(
    input logic clk,
    input logic rst_n,
    
    // Prediction interface
    input logic [31:0] pc,
    output logic [31:0] next_fetch_pc,
    output logic [1:0] cut_pos,
    output logic hit,
    output logic [1:0] branch_type,
    
    // Update interface
    input logic update_valid,        // Valid update signal
    input logic [31:0] update_pc,    // PC to update
    input logic [31:0] target_pc,    // Branch target PC
    input logic [1:0] update_cut_pos, // Cut position for this branch
    input logic [1:0] update_branch_type   // Branch type (direct/indirect/return/call)
);

// uBTB entry structure
typedef struct packed {
    logic valid;                    // Valid bit
    logic [31:0] tag;              // PC tag
    logic [31:0] target;           // Branch target
    logic [1:0] cut_pos;           // Cut position (where to cut in fetch group)
    logic [1:0] branch_type;       // Branch type
} ubtb_entry_t;

// uBTB memory
ubtb_entry_t ubtb_mem [`uBTB_SIZE-1:0];

// Hash function for PC indexing (same as BHT for consistency)
function automatic logic [`uBTB_INDEX_WIDTH-1:0] pc_hash(input logic [31:0] pc_in);
    logic [`uBTB_INDEX_WIDTH-1:0] hash;
    // Use same hash function as BHT for consistency
    // For 1024 entries, INDEX_WIDTH = 10, so we use bits [11:2] and [31:22]
    logic [`uBTB_INDEX_WIDTH-1:0] lower_bits = pc_in[`uBTB_INDEX_WIDTH+1:2];
    logic [`uBTB_INDEX_WIDTH-1:0] upper_bits = pc_in[31:(32-`uBTB_INDEX_WIDTH)];
    hash = lower_bits ^ upper_bits;
    return hash;
endfunction

// Get uBTB index from PC
logic [`uBTB_INDEX_WIDTH-1:0] pred_index, update_index;
assign pred_index = pc_hash(pc);
assign update_index = pc_hash(update_pc);

// Forwarding logic: detect read-write conflict and forward updated value
logic forwarding_enable;
ubtb_entry_t current_entry, forwarded_entry, final_entry;

// Detect if we need forwarding (same index for read and write)
assign forwarding_enable = update_valid && (pred_index == update_index);

// Get current entry from uBTB
assign current_entry = ubtb_mem[pred_index];

// Calculate forwarded entry (what the entry will become after update)
always_comb begin
    if (forwarding_enable) begin
        // Forward the updated values
        forwarded_entry.valid = `VALID;
        forwarded_entry.tag = update_pc;
        forwarded_entry.target = target_pc;
        forwarded_entry.cut_pos = update_cut_pos;
        forwarded_entry.branch_type = update_branch_type;
    end else begin
        // Keep current values (this should never be used when forwarding_enable=0)
        forwarded_entry = current_entry;
    end
end

// Select final entry: forwarded if conflict, otherwise current
assign final_entry = forwarding_enable ? forwarded_entry : current_entry;

// Prediction logic using final entry
always_comb begin
    // Check if we have a valid entry for this PC
    if (final_entry.valid && (final_entry.tag == pc)) begin
        hit = `VALID;
        next_fetch_pc = final_entry.target;
        cut_pos = final_entry.cut_pos;
        branch_type = final_entry.branch_type;
    end else begin
        hit = `INVALID;
        next_fetch_pc = pc + 4;  // Default: sequential next instruction
        cut_pos = 2'b00;         // Default: no cut
        branch_type = `BRANCH_TYPE_DIRECT;
    end
end

// Update logic
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        // Reset all entries to invalid
        for (int i = 0; i < `uBTB_SIZE; i++) begin
            ubtb_mem[i].valid <= `INVALID;
            ubtb_mem[i].tag <= 32'h0;
            ubtb_mem[i].target <= 32'h0;
            ubtb_mem[i].cut_pos <= 2'b00;
            ubtb_mem[i].branch_type <= `BRANCH_TYPE_DIRECT;
        end
    end else if (update_valid) begin
        // Update the uBTB entry
        ubtb_mem[update_index].valid <= `VALID;
        ubtb_mem[update_index].tag <= update_pc;
        ubtb_mem[update_index].target <= target_pc;
        ubtb_mem[update_index].cut_pos <= update_cut_pos;
        ubtb_mem[update_index].branch_type <= update_branch_type;
    end
end

// Optional: Add debug signals for monitoring
// synthesis translate_off
initial begin
    $display("uBTB initialized with %d entries", `uBTB_SIZE);
    $display("uBTB index width: %d", `uBTB_INDEX_WIDTH);
end
// synthesis translate_on

endmodule