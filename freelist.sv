`timescale 1ns / 1ps
`include "defs.sv"

module freelist(
    input logic clk,
    input logic rst_n,

    // commit interface, adding new free preg index to freelist
    input logic commit_valid,
    input logic [3:0] added_preg_vec, // e.g. 1001 means only the first and the last instr free preg index
    input logic [`PREG_INDEX_WIDTH-1:0] add_preg_index_0, // preg_index added (freed)
    input logic [`PREG_INDEX_WIDTH-1:0] add_preg_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] add_preg_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] add_preg_index_3,

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PREG_INDEX_WIDTH-1:0] recover_freelist [`PRF_NUM-33:0],
    input logic [`PREG_INDEX_WIDTH-1:0] recover_freelist_head,
    input logic [`PREG_INDEX_1:0] recover_freelist_tail,

    // rename interface, poping free preg index from freelist
    input logic [2:0] readout_num, // at most 4 arch reg should be renamed to preg index
    // output logic [`PREG_INDEX_WIDTH-1:0] readout_preg_index_0,
    // output logic [`PREG_INDEX_WIDTH-1:0] readout_preg_index_1,
    // output logic [`PREG_INDEX_WIDTH-1:0] readout_preg_index_2,
    // output logic [`PREG_INDEX_WIDTH-1:0] readout_preg_index_3,
    output logic [`PREG_INDEX_WIDTH-1:0] readout_preg_index_vec [3:0],

    // freelist status interface
    output logic freelist_empty,
    output logic freelist_full,
    output logic alloc_success,

    // debug
    // output logic [`PREG_INDEX_WIDTH-1:0] debug_freelist_queue [`PRF_NUM-33:0]
);
// debug use
// always_comb begin
//     for(int i=0;i<`PRF_NUM-33;i=i+1) begin
//         debug_freelist_queue[i] = freelist_queue[i];
//     end
// end

// freelist is actually a queue
logic [`PREG_INDEX_WIDTH-1:0] freelist_queue [`PRF_NUM-33:0];
logic [`PREG_INDEX_WIDTH-1:0] freelist_queue_head;
logic [`PREG_INDEX_WIDTH-1:0] freelist_queue_tail;
logic [`PREG_INDEX_WIDTH-1:0] free_preg_num; // from 0 to `PRF_NUM-32 (typical 128)

assign freelist_empty = (free_preg_num == 0);
assign freelist_full = (free_preg_num == `PRF_NUM-32);

// Internal signals for forwarding logic
logic [`PREG_INDEX_WIDTH-1:0] temp_readout_vec [3:0];
logic [2:0] added_preg_num; // how many preg are added from commit 
logic forwarding_enable; // whether we need to forward the added preg to the readout position
logic [1:0] forwarding_pos_added_preg_0; // which position should added_preg_0 be forwarded to
logic [1:0] forwarding_pos_added_preg_1; // which position should added_preg_1 be forwarded to
logic [1:0] forwarding_pos_added_preg_2; // which position should added_preg_2 be forwarded to
logic [1:0] forwarding_pos_added_preg_3; // which position should added_preg_3 be forwarded to

always_comb begin
    if(readout_num > free_preg_num) begin
        if(added_preg_num+free_preg_num >= readout_num) begin
            alloc_success = 1;
        end else begin
            alloc_success = 0;
        end
    end else begin
        alloc_success = 1;
    end
end


assign added_preg_num = added_preg_vec[0] + added_preg_vec[1] + added_preg_vec[2] + added_preg_vec[3];
assign forwarding_enable = (readout_num > free_preg_num) && ((added_preg_num + free_preg_num) >= readout_num) && commit_valid;

always_comb begin
    // judge forwarding_pos_added_preg_x
    if(forwarding_enable) begin
        forwarding_pos_added_preg_0 = free_preg_num; // forwarding to the free_preg_num next position
        forwarding_pos_added_preg_1 = free_preg_num + added_preg_vec[0];
        forwarding_pos_added_preg_2 = free_preg_num + added_preg_vec[0] + added_preg_vec[1];
        forwarding_pos_added_preg_3 = free_preg_num + added_preg_vec[0] + added_preg_vec[1] + added_preg_vec[2];
    end else begin
        forwarding_pos_added_preg_0 = 0;
        forwarding_pos_added_preg_1 = 0;
        forwarding_pos_added_preg_2 = 0;
        forwarding_pos_added_preg_3 = 0;
    end
end
// e.g. we have 1001 meaning the first and the last commit instr freed preg index
// we have 1 left freelist preg and we need 3 preg readout
// so we need to forward the first and the last freed preg index to 
// respectively 1 and 2 position
// so the forwarding_pos_added_preg_0 is 1 and the forwarding_pos_added_preg_3 is 2
// the forwarding_pos_added_preg_1 and xxx_preg_2 is 2 (added_preg_vec[0]=1 and added_preg_vec[1]=0)
// but because the added_preg_vec[1,2] are both 0, so these 2 forwarding won't happen
 

// Readout logic with forwarding - combined into single block to avoid conflicts
always_comb begin
    // Default readout from queue
    temp_readout_vec[0] = freelist_queue[(freelist_queue_head + 0) % `PRF_NUM];
    temp_readout_vec[1] = freelist_queue[(freelist_queue_head + 1) % `PRF_NUM];
    temp_readout_vec[2] = freelist_queue[(freelist_queue_head + 2) % `PRF_NUM];
    temp_readout_vec[3] = freelist_queue[(freelist_queue_head + 3) % `PRF_NUM];
    
    // Apply forwarding if needed
    if(forwarding_enable) begin
        if(added_preg_vec[0]) begin // the first instr contains preg index
            temp_readout_vec[forwarding_pos_added_preg_0] = add_preg_index_0;
        end
        if(added_preg_vec[1]) begin // the second instr contains preg index
            temp_readout_vec[forwarding_pos_added_preg_1] = add_preg_index_1;
        end
        if(added_preg_vec[2]) begin // the third instr contains preg index
            temp_readout_vec[forwarding_pos_added_preg_2] = add_preg_index_2;
        end
        if(added_preg_vec[3]) begin // the fourth instr contains preg index
            temp_readout_vec[forwarding_pos_added_preg_3] = add_preg_index_3;
        end
    end
end

logic [2:0] actual_read_from_queue;
// Sequential logic for freelist operations
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        // Reset freelist to initial state
        freelist_queue_head <= 0;
        freelist_queue_tail <= 0;
        free_preg_num <= `PRF_NUM-32;
        
        // Initialize freelist with all physical registers (except r0 which is always 0)
        for (int i = 0; i < `PRF_NUM-33; i++) begin
            freelist_queue[i] <= i+33; // we still count into Arch reg0 -> PREG 0 
        end
        
        // Reset readout outputs
        readout_preg_index_vec[0] <= 0;
        readout_preg_index_vec[1] <= 0;
        readout_preg_index_vec[2] <= 0;
        readout_preg_index_vec[3] <= 0;

    end else if(recover_valid) begin
        freelist_queue_head <= recover_freelist_head;
        freelist_queue_tail <= recover_freelist_tail;
        free_preg_num <= `PRF_NUM-32;
        for(int i=0; i<`PRF_NUM-33; i=i+1) begin
            freelist_queue[i] <= recover_freelist[(freelist_queue_head + i) % `PRF_NUM];
        end
    end else begin
        // Update readout outputs
        readout_preg_index_vec[0] <= temp_readout_vec[0];
        readout_preg_index_vec[1] <= temp_readout_vec[1];
        readout_preg_index_vec[2] <= temp_readout_vec[2];
        readout_preg_index_vec[3] <= temp_readout_vec[3];
        
        // Handle commit (write) operations
        if (commit_valid && added_preg_num > 0) begin
            // Add freed registers to the tail of the queue
            if (added_preg_num >= 1) begin
                freelist_queue[freelist_queue_tail] <= add_preg_index_0;
            end
            if (added_preg_num >= 2) begin
                freelist_queue[(freelist_queue_tail + 1) % `PRF_NUM] <= add_preg_index_1;
            end
            if (added_preg_num >= 3) begin
                freelist_queue[(freelist_queue_tail + 2) % `PRF_NUM] <= add_preg_index_2;
            end
            if (added_preg_num >= 4) begin
                freelist_queue[(freelist_queue_tail + 3) % `PRF_NUM] <= add_preg_index_3;
            end
            
            // Update tail pointer
            freelist_queue_tail <= (freelist_queue_tail + added_preg_num) % `PRF_NUM;
            
            // Update free register count
            free_preg_num <= free_preg_num + added_preg_num;
        end
        
        // Handle readout (read) operations
        if (readout_num > 0) begin
            // Calculate how many registers were actually read from queue
            actual_read_from_queue = (readout_num > free_preg_num) ? free_preg_num : readout_num;
            
            // Advance head pointer only for registers actually read from queue
            freelist_queue_head <= (freelist_queue_head + actual_read_from_queue) % `PRF_NUM;
            
            // Update free register count
            free_preg_num <= free_preg_num - actual_read_from_queue;
        end
    end
end

endmodule
