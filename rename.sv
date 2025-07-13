 `timescale 1ns / 1ps
`include "defs.sv"

// in this work, we use PRF to implement rename
// 32 arch regs can be renamed to typically 128 Physical Regsters (using Macro Defines)
// first we need to get homw mamny instr we get this cycle
// then we need to get these instrs' reg exist vecs
// judged from Combinational Logic results , we get renmae_enable vecs
module rename(
    // interface with prev and next stage
    input logic clk,
    input logic rst_n,
    input logic decoder_valid, // decoder output is valid
    input logic rename_flush, 
    input logic dispatch_ready, // dispatch is ready to receive renamed instr
    // input logic rob_ready, // rob is ready to receive new instrs
    output logic rename_ready, // ready to rename next cycle (can receive new instr group)
    output logic rename_valid, // rename output is valid

    // recover (exception and branch prediction failure)
    input logic recover_valid,
    input logic [`PREG_INDEX_WIDTH-1:0] recover_preg_index_vec [31:0],
    // 是否还差了一点其他的信号？

    // freelist interface from ROB commit
    input logic commit_valid, // commit outputs are valid
    input logic [3:0] freed_preg_vec, // at most 4 instr commit in one cycle
    // so at most 4 preg index will be freed in one cycle 0~4
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] freed_preg_index_3,

    // fetch group interface
    input logic [31:0] instr_0,
    input logic [31:0] instr_1,
    input logic [31:0] instr_2,
    input logic [31:0] instr_3,
    input logic [3:0] instr_valid_vec,
    
    // reg_rd_exist_vec and reg_rd index
    input logic reg_rd_exist_0,
    input logic reg_rd_exist_1,
    input logic reg_rd_exist_2,
    input logic reg_rd_exist_3,
    input logic [4:0] reg_rd_0,
    input logic [4:0] reg_rd_1,
    input logic [4:0] reg_rd_2,
    input logic [4:0] reg_rd_3,
 
    // reg_rj_exist_vec and reg_rj index
    input logic reg_rj_exist_0,
    input logic reg_rj_exist_1,
    input logic reg_rj_exist_2,
    input logic reg_rj_exist_3,
    input logic [4:0] reg_rj_0,
    input logic [4:0] reg_rj_1,
    input logic [4:0] reg_rj_2,
    input logic [4:0] reg_rj_3,

    // reg_rk_exist_vec and reg_rk index
    input logic reg_rk_exist_0,
    input logic reg_rk_exist_1,
    input logic reg_rk_exist_2,
    input logic reg_rk_exist_3,
    input logic [4:0] reg_rk_0,
    input logic [4:0] reg_rk_1,
    input logic [4:0] reg_rk_2,
    input logic [4:0] reg_rk_3,

    // pc and instr_valid_vec
    output logic [31:0] pc_vec [3:0], // 4 instr's pc
    // because we have IFB(buffer), so continuous 4 instr's may contain jump or branch
    output logic [3:0] instr_valid_vec, // 4 instr's valid

    // output rd arch index
    output logic [4:0] rd_arch_index_0,
    output logic [4:0] rd_arch_index_1,
    output logic [4:0] rd_arch_index_2,
    output logic [4:0] rd_arch_index_3,
    output logic [3:0] rd_exist_vec,

    // preg_rd index
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rd_0,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rd_1,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rd_2,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rd_3,
    // the exist vec is the same as the rd_exist_vec

    // when rename -> issue, we need to read the busy table
    output logic [`PREG_INDEX_WIDTH-1:0] busy_table_read_preg_rd_index_vec [3:0], 
    // if rd[i]_exist == 0, then busy_table_read_preg_rd_index_vec[i] = 0
    output logic [`PREG_INDEX_WIDTH-1:0] busy_table_read_preg_rj_index_vec [3:0],
    output logic [`PREG_INDEX_WIDTH-1:0] busy_table_read_preg_rk_index_vec [3:0],

    input logic [3:0] preg_rd_busy_vec, // 0 -- valid , 1 -- data invalid
    input logic [3:0] preg_rj_busy_vec,
    input logic [3:0] preg_rk_busy_vec,

    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_vec [3:0],
    input logic [3:0] write_back_rd_valid_vec, // 0 -- valid , 1 -- invalid
    // maybe the valid vec is unnecessary? I think the non-existent write_back_rd_index will be 0

    // preg_rj index
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rj_0,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rj_1,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rj_2,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rj_3,
    output logic [3:0] rj_exist_vec,

    // preg_rk index
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rk_0,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rk_1,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rk_2,
    output logic [`PREG_INDEX_WIDTH-1:0] preg_rk_3,
    output logic [3:0] rk_exist_vec,

    // rd history preg index
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_0,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_1,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_2,
    output logic [`PREG_INDEX_WIDTH-1:0] rd_history_preg_3
);

// temp signals (should be wire?)
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rd_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_3;

logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_0;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_1;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_2;
logic [`PREG_INDEX_WIDTH-1:0] temp_rd_history_preg_3;


// logic checker outputs
logic [1:0] rj_0_raw_pos, rj_1_raw_pos, rj_2_raw_pos, rj_3_raw_pos;
logic [1:0] rk_0_raw_pos, rk_1_raw_pos, rk_2_raw_pos, rk_3_raw_pos;
logic [1:0] rd_0_waw_pos, rd_1_waw_pos, rd_2_waw_pos, rd_3_waw_pos;

logic_checker u_logic_checker(
    .clk(clk),
    .rst_n(rst_n),

    .instr_rd_index_vec('{reg_rd_0, reg_rd_1, reg_rd_2, reg_rd_3}),
    .instr_rj_index_vec('{reg_rj_0, reg_rj_1, reg_rj_2, reg_rj_3}),
    .instr_rk_index_vec('{reg_rk_0, reg_rk_1, reg_rk_2, reg_rk_3}),
    
    .instr_rd_exist_vec({reg_rd_exist_3, reg_rd_exist_2, reg_rd_exist_1, reg_rd_exist_0}),
    .instr_rj_exist_vec({reg_rj_exist_3, reg_rj_exist_2, reg_rj_exist_1, reg_rj_exist_0}),
    .instr_rk_exist_vec({reg_rk_exist_3, reg_rk_exist_2, reg_rk_exist_1, reg_rk_exist_0}),

    .rj_0_raw_pos(rj_0_raw_pos),
    .rj_1_raw_pos(rj_1_raw_pos),
    .rj_2_raw_pos(rj_2_raw_pos),
    .rj_3_raw_pos(rj_3_raw_pos),

    .rk_0_raw_pos(rk_0_raw_pos),
    .rk_1_raw_pos(rk_1_raw_pos),
    .rk_2_raw_pos(rk_2_raw_pos),
    .rk_3_raw_pos(rk_3_raw_pos),

    .rd_0_waw_pos(rd_0_waw_pos),
    .rd_1_waw_pos(rd_1_waw_pos),
    .rd_2_waw_pos(rd_2_waw_pos),
    .rd_3_waw_pos(rd_3_waw_pos)
);

// freelist allocate free preg index
logic [`PREG_INDEX_WIDTH-1:0] alloc_preg_index_vec [3:0];
logic freelist_empty;
logic freelist_full;
logic freelist_alloc_success;
logic [2:0] readout_num;

assign readout_num = reg_rd_exist_0 + reg_rd_exist_1 + reg_rd_exist_2 + reg_rd_exist_3;

// freelist only needs to allocate free preg index
// specifically decide which instr needs to be renamed is done in the internal logic
freelist u_freelist(
    .clk(clk),
    .rst_n(rst_n),

    .commit_valid(commit_valid),
    .added_preg_vec(freed_preg_vec),
    .add_preg_index_0(freed_preg_index_0),
    .add_preg_index_1(freed_preg_index_1),
    .add_preg_index_2(freed_preg_index_2),
    .add_preg_index_3(freed_preg_index_3),
    .readout_num(readout_num),

    .readout_preg_index_vec(alloc_preg_index_vec),
    .freelist_empty(freelist_empty),
    .freelist_full(freelist_full),
    .alloc_success(freelist_alloc_success)
);

// rename_ready decided whether the freelist can allocate
always_comb begin
    rename_ready = freelist_alloc_success;
end

// RAT
rat u_rat(
    .clk(clk),
    .rst_n(rst_n),

    .recover_valid(recover_valid),
    .recover_preg_index_vec(recover_preg_index_vec),

    .arch_rd_0_index(reg_rd_0),
    .arch_rd_1_index(reg_rd_1),
    .arch_rd_2_index(reg_rd_2),
    .arch_rd_3_index(reg_rd_3),

    .arch_rd_exist_vec({reg_rd_exist_3, reg_rd_exist_2, reg_rd_exist_1, reg_rd_exist_0}),

    .updating_valid_vec({reg_rd_exist_3, reg_rd_exist_2, reg_rd_exist_1, reg_rd_exist_0}),
    .added_preg_index_vec(alloc_preg_index_vec),

    .rd_history_preg_0(temp_rd_history_preg_0),
    .rd_history_preg_1(temp_rd_history_preg_1),
    .rd_history_preg_2(temp_rd_history_preg_2),
    .rd_history_preg_3(temp_rd_history_preg_3),

    .rj_matched_preg_0(temp_preg_rj_0),
    .rj_matched_preg_1(temp_preg_rj_1),
    .rj_matched_preg_2(temp_preg_rj_2),
    .rj_matched_preg_3(temp_preg_rj_3),

    .rk_matched_preg_0(temp_preg_rk_0),
    .rk_matched_preg_1(temp_preg_rk_1),
    .rk_matched_preg_2(temp_preg_rk_2),
    .rk_matched_preg_3(temp_preg_rk_3)
);

// temp_preg_rd_x selection logic
always_comb begin
    temp_preg_rd_0 = (reg_rd_exist_0) ? alloc_preg_index_vec[0] : 0;
    temp_preg_rd_1 = (reg_rd_exist_1) ? alloc_preg_index_vec[reg_rd_exist_0] : 0;
    temp_preg_rd_2 = (reg_rd_exist_2) ? alloc_preg_index_vec[reg_rd_exist_0 + reg_rd_exist_1] : 0;
    temp_preg_rd_3 = (reg_rd_exist_3) ? alloc_preg_index_vec[reg_rd_exist_0 + reg_rd_exist_1 + reg_rd_exist_2] : 0;
end

// temp_preg_rj_x selection logic
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_1_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_2_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rj_3_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_1_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_2_sel;
logic [`PREG_INDEX_WIDTH-1:0] temp_preg_rk_3_sel;

always_comb begin
    // Default values from RAT
    temp_preg_rj_0 = temp_preg_rj_0;  // Keep RAT value
    temp_preg_rj_1 = temp_preg_rj_1;  // Keep RAT value
    temp_preg_rj_2 = temp_preg_rj_2;  // Keep RAT value
    temp_preg_rj_3 = temp_preg_rj_3;  // Keep RAT value
    
    // Override with forwarding logic
    if(rj_1_raw_pos == 0) begin
        temp_preg_rj_1_sel = temp_preg_rd_0;
    end

    case(rj_2_raw_pos)
        0: temp_preg_rj_2 = temp_preg_rd_0;  // Fix typo
        1: temp_preg_rj_2 = temp_preg_rd_1;
        default: // keep temp_preg_rj_2 value from RAT
    endcase

    case(rj_3_raw_pos)
        0: temp_preg_rj_3 = temp_preg_rd_0;
        1: temp_preg_rj_3 = temp_preg_rd_1;
        2: temp_preg_rj_3 = temp_preg_rd_2;
        default: // keep temp_preg_rj_3 value from RAT
    endcase
end

// temp_preg_rk_x selection logic
always_comb begin
    // Default values from RAT
    temp_preg_rk_0 = temp_preg_rk_0;  // Keep RAT value
    temp_preg_rk_1 = temp_preg_rk_1;  // Keep RAT value
    temp_preg_rk_2 = temp_preg_rk_2;  // Keep RAT value
    temp_preg_rk_3 = temp_preg_rk_3;  // Keep RAT value
    
    // Override with forwarding logic
    if(rk_1_raw_pos == 0) begin
        temp_preg_rk_1_sel = temp_preg_rd_0;
    end

    case(rk_2_raw_pos)
        0: temp_preg_rk_2 = temp_preg_rd_0;
        1: temp_preg_rk_2 = temp_preg_rd_1;
        default: // keep temp_preg_rk_2 value from RAT
    endcase

    case(rk_3_raw_pos)
        0: temp_preg_rk_3 = temp_preg_rd_0;
        1: temp_preg_rk_3 = temp_preg_rd_1;
        2: temp_preg_rk_3 = temp_preg_rd_2;
        default: // keep temp_preg_rk_3 value from RAT
    endcase
end

// rename_ready logic
always_comb begin
    // Count how many registers we need to allocate
    logic [2:0] needed_regs;
    needed_regs = reg_rd_exist_0 + reg_rd_exist_1 + reg_rd_exist_2 + reg_rd_exist_3;
    
    // Ready if freelist has enough registers and downstream modules are ready
    rename_ready = !freelist_empty && 
                   (needed_regs <= readout_num) && 
                   dispatch_ready && 
                   rob_ready;
end

// last step: free preg index dispatch logic
// rd_history_preg
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        rd_history_preg_0 <= 0;
        rd_history_preg_1 <= 0;
        rd_history_preg_2 <= 0;
        rd_history_preg_3 <= 0;
    end else begin
        if(!rob_ready || !freelist_alloc_success || !dispatch_ready) begin
            rd_history_preg_0 <= rd_history_preg_0;
            rd_history_preg_1 <= rd_history_preg_1;
            rd_history_preg_2 <= rd_history_preg_2;
            rd_history_preg_3 <= rd_history_preg_3;
        end else begin
            // ready to send out
            rd_history_preg_0 <= temp_rd_history_preg_0;
            rd_history_preg_1 <= temp_rd_history_preg_1;
            rd_history_preg_2 <= temp_rd_history_preg_2;
            rd_history_preg_3 <= temp_rd_history_preg_3;
        end
    end
end

// preg_rd
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        preg_rd_0 <= 0;
        preg_rd_1 <= 0;
        preg_rd_2 <= 0;
        preg_rd_3 <= 0;
    end else begin
        if(!rob_ready || !freelist_alloc_success || !dispatch_ready) begin
            preg_rd_0 <= preg_rd_0;
            preg_rd_1 <= preg_rd_1;
            preg_rd_2 <= preg_rd_2;
            preg_rd_3 <= preg_rd_3;
        end else begin
            // ready to send out
            preg_rd_0 <= temp_preg_rd_0;
            preg_rd_1 <= temp_preg_rd_1;
            preg_rd_2 <= temp_preg_rd_2;
            preg_rd_3 <= temp_preg_rd_3;
        end
    end
end

// preg_rj
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        preg_rj_0 <= 0;
        preg_rj_1 <= 0;
        preg_rj_2 <= 0;
        preg_rj_3 <= 0;
    end else begin
        if(!rob_ready || !freelist_alloc_success || !dispatch_ready) begin
            preg_rj_0 <= preg_rj_0;
            preg_rj_1 <= preg_rj_1;
            preg_rj_2 <= preg_rj_2;
            preg_rj_3 <= preg_rj_3;
        end else begin
            // ready to send out
            preg_rj_0 <= temp_preg_rj_0;
            preg_rj_1 <= temp_preg_rj_1_sel;
            preg_rj_2 <= temp_preg_rj_2_sel;
            preg_rj_3 <= temp_preg_rj_3_sel;
        end
    end
end

// preg_rk
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        preg_rk_0 <= 0;
        preg_rk_1 <= 0;
        preg_rk_2 <= 0;
        preg_rk_3 <= 0;
    end else begin
        if(!rob_ready || !freelist_alloc_success || !dispatch_ready) begin
            preg_rk_0 <= preg_rk_0;
            preg_rk_1 <= preg_rk_1;
            preg_rk_2 <= preg_rk_2;
            preg_rk_3 <= preg_rk_3;
        end else begin
            // ready to send out
            preg_rk_0 <= temp_preg_rk_0;
            preg_rk_1 <= temp_preg_rk_1_sel;
            preg_rk_2 <= temp_preg_rk_2_sel;
            preg_rk_3 <= temp_preg_rk_3_sel;
        end
    end
end

// need to complete the busy table read and write back logic

endmodule