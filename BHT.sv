`include "defs.sv"

module BHT
(
    input logic clk,
    input logic rst_n,
    
    // Prediction interface
    input logic [31:0] pc,
    output logic taken,
    
    // Update interface
    input logic [31:0] update_pc,    // PC to update
    input logic update_taken,        // Actual branch result (1=taken, 0=not taken)
    input logic update_valid         // Valid update signal (1=update this cycle)
);

// BHT entry type: parameterized width saturating counter
typedef logic [`BHT_WIDTH-1:0] bht_entry_t;

// BHT memory
bht_entry_t bht_mem [`BHT_SIZE-1:0];

// Hash function for PC indexing (simple XOR-based hash)
function automatic logic [`BHT_INDEX_WIDTH-1:0] pc_hash(input logic [31:0] pc_in);
    logic [`BHT_INDEX_WIDTH-1:0] hash;
    // Ensure both operands have the same width by using the same bit range
    // For 1024 entries, INDEX_WIDTH = 10, so we use bits [11:2] and [31:22]
    // Use lower bits for first operand
    logic [`BHT_INDEX_WIDTH-1:0] lower_bits = pc_in[`BHT_INDEX_WIDTH+1:2];
    // Use higher bits for second operand, truncate to match width
    logic [`BHT_INDEX_WIDTH-1:0] upper_bits = pc_in[31:(32-`BHT_INDEX_WIDTH)];
    hash = lower_bits ^ upper_bits;
    return hash;
endfunction

// Get BHT index from PC
logic [`BHT_INDEX_WIDTH-1:0] pred_index, update_index;
assign pred_index = pc_hash(pc);
assign update_index = pc_hash(update_pc);

// Forwarding logic: detect read-write conflict and forward updated value
logic forwarding_enable;
bht_entry_t forwarded_value, current_value, final_value;

// Detect if we need forwarding (same index for read and write)
assign forwarding_enable = update_valid && (pred_index == update_index);

// Get current value from BHT
assign current_value = bht_mem[pred_index];

// Calculate forwarded value (what the entry will become after update)
always_comb begin
    if (update_taken) begin
        // Branch was taken - increment counter (saturating)
        if (current_value < ((1 << `BHT_WIDTH) - 1)) begin
            forwarded_value = current_value + 1;
        end else begin
            forwarded_value = current_value; // Already saturated
        end
    end else begin
        // Branch was not taken - decrement counter (saturating)
        if (current_value > 0) begin
            forwarded_value = current_value - 1;
        end else begin
            forwarded_value = current_value; // Already saturated
        end
    end
end

// Select final value: forwarded if conflict, otherwise current
assign final_value = forwarding_enable ? forwarded_value : current_value;

// Prediction logic using final value
always_comb begin
    // Predict taken if counter value >= midpoint
    // For BHT_WIDTH=1: taken if counter >= 1
    // For BHT_WIDTH=2: taken if counter >= 2  
    // For BHT_WIDTH=3: taken if counter >= 4
    // For BHT_WIDTH=4: taken if counter >= 8
    // General: taken if counter >= (2^(BHT_WIDTH-1))
    taken = (final_value >= (1 << (`BHT_WIDTH-1)));
end

// Update logic - parameterized saturating counter
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        // Reset all entries to middle value (weakly not taken)
        // For BHT_WIDTH=1: reset to 0
        // For BHT_WIDTH=2: reset to 1
        // For BHT_WIDTH=3: reset to 2
        // General: reset to (2^(BHT_WIDTH-1) - 1)
        bht_entry_t reset_value = (1 << (`BHT_WIDTH-1)) - 1;
        for (int i = 0; i < `BHT_SIZE; i++) begin
            bht_mem[i] <= reset_value;
        end
    end else if (update_valid) begin
        // Update the BHT entry based on actual branch result
        if (update_taken) begin
            // Branch was taken - increment counter (saturating)
            if (bht_mem[update_index] < ((1 << `BHT_WIDTH) - 1)) begin
                bht_mem[update_index] <= bht_mem[update_index] + 1;
            end
        end else begin
            // Branch was not taken - decrement counter (saturating)
            if (bht_mem[update_index] > 0) begin
                bht_mem[update_index] <= bht_mem[update_index] - 1;
            end
        end
    end
end

// Optional: Add debug signals for monitoring
// synthesis translate_off
initial begin
    $display("BHT initialized with %d entries, %d-bit counters", `BHT_SIZE, `BHT_WIDTH);
    $display("Prediction threshold: %d", (1 << (`BHT_WIDTH-1)));
    $display("Reset value: %d", (1 << (`BHT_WIDTH-1)) - 1);
    $display("Max counter value: %d", (1 << `BHT_WIDTH) - 1);
end
// synthesis translate_on

endmodule