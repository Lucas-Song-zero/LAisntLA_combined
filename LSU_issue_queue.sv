`timescale 1ns/1ps
`include "defs.sv"

// all the stores are all in order
// but all the loads between the issued sotres to next un-issued stores
// are out-of-order

// NOTE: all load instrs' addr must be aligned to 4 bytes else will tigger exception: ADEF
// NOTE: all LS instr must check their addr-alignment condition:
// // if not naturally aligned, then it will set exception: ALE
// NOTE: all synchronous ops must be in-order
// // that includes: dbar ibar LL-SC
// // before one sync op is issued, all normal LS ops prior to this sync op must be completed
// // before one normal LS op is issued, all sync ops prior to this normal LS op must be completed

// we use 1 issue bit and 1completion bit to represent a state:
// when issue bit is 1, queue won't pop this issued insttr
// when LSU completes any LSU, it will set the corresponding completion bit to 1
// at each cycle, lsu_iq will pop one completed LSU (scanning from the beginning)
// at each cycle, lsu at most returns 1 completion signal (using completion_index to index the LSU_IQ)

module LSU_issue_queue(
    input logic clk,
    input logic rst_n,
    // interface with rename and FU
    input logic rename_valid,
    input logic fu_ready,
    input logic [1:0] rename_instr_num,
    input logic [`ROB_ENTRY_INDEX_WIDTH-1:0] rob_entry_vec [3:0],
    input logic [`LSU_IQ_INDEX_WIDTH:0] completion_index, // 用于LSU返回完成情况的时候index使用
    output logic issue_ready,
    output logic issue_valid, // issue info valid
    output logic IQ_full,
    output logic IQ_empty,
    

    // necessary info to store
    input logic [3:0] gen_op_type_vec [3:0],
    input logic [3:0] spec_op_type_vec [4:0],
    input logic [3:0] store_or_load_vec, // received from decode 1 means store 0 means load
    input logic [1:0] bar_type_vec [3:0], // 00 -- normal , 01 -- dbar , 10 -- ibar
    input logic [3:0] imm_vec [25:0],
    input logic [3:0] imm_enable_vec,
    input logic [4:0] arch_rd_index,
    input logic [3:0] preg_rd_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] preg_rj_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] preg_rk_vec [`PREG_INDEX_WIDTH-1:0],
    input logic [3:0] reg_rd_exist_vec,
    input logic [3:0] reg_rj_exist_vec,
    input logic [3:0] reg_rk_exist_vec,
    input logic [3:0] rj_ready_vec,
    input logic [3:0] rk_ready_vec,
    input logic [3:0] rd_ready_vec,

    // wake-up signal
    // wake-up signals inside the queue, if one instr is selected in ALU_IQ
    // then in this cycle, we will wake up the related instrs
    // but the inside wake up signals don't need io
    input logic [3:0] write_back_rd_exist_vec,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_0,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_1,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_2,
    input logic [`PREG_INDEX_WIDTH-1:0] write_back_rd_index_3,

    // when issue is ready, give out some info to FU
    output logic [3:0] issued_gen_op_type,
    output logic [4:0] issued_spec_op_type,
    output logic [25:0] issued_imm,
    output logic issued_imm_enable,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rd,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rj,
    output logic [`PREG_INDEX_WIDTH-1:0] issued_preg_rk,
    output logic issued_reg_rd_exist,
    output logic issued_reg_rj_exist,
    output logic issued_reg_rk_exist,
    output logic [`LSU_IQ_INDEX_WIDTH:0] issued_lsu_index, // for tracking the instr
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] issued_rob_entry_index,
    output logic issued_store_or_load,
    output logic issued_bar_type,
    
    input logic [`LSU_IQ_INDEX_WIDTH-1:0] completion_index_in, // LSU completion index back from ROB
    input logic completion_in_valid // completion info write back in valid signal
    // exception outputs (LSU should output it, not the LSU_IQ)
    
    // output logic exception_ALE, // LS instr addr not naturally aligned (when issue need check the alignment)

    // write back interface
    input logic [3:0] write_back_rd_exist_vec,
    input logic [`LSU_IQ_INDEX_WIDTH-1:0] wb_completion_index, // index for setting completion bit to 1
);

logic [`LSU_IQ_ENTRY_WIDTH-1:0] lsu_issue_queue [`LSU_IQ_DEPTH-1:0];
logic [25:0] lsu_imm_queue [`LSU_IQ_DEPTH-1:0];
logic [8:0] lsu_op_type_queue [`LSU_IQ_DEPTH-1:0];
// for tracking instr completing status, we need to index each instr in the LSU_IQ
logic [`LSU_IQ_INDEX_WIDTH-1:0] lsu_index_queue [`LSU_IQ_DEPTH-1:0];
logic [`LSU_IQ_INDEX_WIDTH:0] filled_entry_cnt; // 0~64
logic [`LSU_IQ_INDEX_WIDTH:0] latest_input_index; // used for tracking the wb completion instr index

assign IQ_full = (filled_entry_cnt == `LSU_IQ_DEPTH);
assign IQ_empty = (filled_entry_cnt == 0);
logic alloc_success;
assign alloc_success = (filled_entry_cnt+rename_instr_num <= `LSU_IQ_DEPTH);
assign issue_ready = alloc_success;

// shadow variables definition
logic [`LSU_IQ_ENTRY_WIDTH-1:0] next_lsu_issue_queue [`LSU_IQ_DEPTH-1:0];
logic [25:0] next_lsu_imm_queue [`LSU_IQ_DEPTH-1:0];
logic [8:0] next_lsu_op_type_queue [`LSU_IQ_DEPTH-1:0];
logic [`LSU_IQ_INDEX_WIDTH-1:0] next_lsu_index_queue [`LSU_IQ_DEPTH-1:0];
logic [`LSU_IQ_INDEX_WIDTH:0] next_latest_input_index; // used for tracking the wb completion instr index
logic [`LSU_IQ_INDEX_WIDTH-1:0] next_filled_entry_cnt;

logic [`PREG_INDEX_WIDTH-1:0] wb_rd_index;

logic [`LSU_IQ_INDEX_WIDTH-1:0] store_block_index; // 0~63
logic [`LSU_IQ_INDEX_WIDTH-1:0] issued_store_block_index; // 0~63
logic issued_page_diff_index; // 0 or 1
// when the page_diff_index is same, the priority is LESS_PRIO
// but when the page_diff_index is different, the priority is MORE_PRIO
logic [`LSU_IQ_INDEX_WIDTH-1:0] next_store_block_index; // 0~63
logic [`LSU_IQ_INDEX_WIDTH-1:0] next_issued_store_block_index; // 0~63
logic next_issued_page_diff_index; // 0 or 1

// DBAR IBAR signal
logic [`LSU_IQ_INDEX_WIDTH-1:0] dbar_index;
logic [`LSU_IQ_INDEX_WIDTH-1:0] ibar_index;
logic store_before_ibar; // signal represents if there exists store before ibar
logic not_issued_before_dbar; // signal repre if exists not issued L/S instr before dbar

always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        filled_entry_cnt <= 0;
        store_block_index <= 0;
        issued_store_block_index <= 0;
        issued_page_diff_index <= 0;
        latest_input_index <= 0;
        for (int i = 0; i < `LSU_IQ_DEPTH; i++) begin
            lsu_issue_queue[i] <= 0;
            lsu_imm_queue[i] <= 0;
            lsu_op_type_queue[i] <= 0;
            lsu_index_queue[i] <= 0;
        end
    end else begin
        // 1. copy the original queue to shadow variables
        for (int i = 0; i < `LSU_IQ_DEPTH; i++) begin
            next_lsu_issue_queue[i] = lsu_issue_queue[i];
            next_lsu_imm_queue[i] = lsu_imm_queue[i];
            next_lsu_op_type_queue[i] = lsu_op_type_queue[i];
            next_lsu_index_queue[i] = lsu_index_queue[i];
        end
        next_latest_input_index = latest_input_index;
        next_filled_entry_cnt = filled_entry_cnt;
        next_store_block_index = store_block_index;
        next_issued_store_block_index = issued_store_block_index;
        next_issued_page_diff_index = issued_page_diff_index;

        // check DBAR or IBAR
        dbar_index = 0;
        ibar_index = 0;
        store_before_ibar = 0;
        not_completed_before_dbar = 0;
        // check DBAR
        for(int i=0; i<`LSU_IQ_DEPTH; i++) begin
            if(next_lsu_issue_queue[i].bar_type == `BAR_NORMAL && !next_lsu_issue_queue[i].completion) begin
                not_completed_before_dbar = 1;
                break;
            end
            if(not_completed_before_dbar && next_lsu_issue_queue[i].bar_type == `BAR_DBAR) begin
                dbar_index = i;
                break;
            end
        end
        // check IBAR
        for(int i=0; i<`LSU_IQ_DEPTH; i++) begin
            if(next_lsu_issue_queue[i].store_or_load == 1'b1 && next_lsu_issue_queue[i].bar_type == `BAR_NORMAL && !next_lsu_issue_queue[i].completion) begin
                // 找到IBAR之前没有完成的store指令
                store_before_ibar = 1;
            end
            if(store_before_ibar && next_lsu_issue_queue[i].bar_type == `BAR_IBAR) begin // 10 -- ibar
                ibar_index = i;
                break;
            end
        end // if ibar_index == 0 means no ibar 

        // 2. issue + pre-wakeup + compress
        if (fu_ready && !IQ_empty) begin
            issue_valid <= 0;
            for(int i=0; i<`LSU_IQ_DEPTH; i++) begin
                if(next_lsu_issue_queue[i].issued == 1'b0
                && (next_lsu_issue_queue[i].rj_ready || !next_lsu_issue_queue[i].rj_valid)
                && (next_lsu_issue_queue[i].rk_ready || !next_lsu_issue_queue[i].rk_valid)
                && (next_lsu_issue_queue[i].store_or_load && next_lsu_issue_queue[i].rd_ready || !next_lsu_issue_queue[i].store_or_load)
                && (next_lsu_issue_queue[i].store_or_load == 1'b1
                    || (next_lsu_issue_queue[i].page_diff_index == next_issued_page_diff_index && next_lsu_issue_queue[i].store_block_index <= next_issued_store_block_index)
                    || (next_lsu_issue_queue[i].page_diff_index != next_issued_page_diff_index && next_lsu_issue_queue[i].store_block_index >= next_issued_store_block_index))
                && (i<=ibar_index && i<=dbar_index)) begin
                    // set lsu_issue_valid to 1
                    issue_valid <= 1;
                    // issue the instr
                    issued_gen_op_type <= next_lsu_op_type_queue[i][8:5];
                    issued_spec_op_type <= next_lsu_op_type_queue[i][4:0];
                    issued_imm <= next_lsu_imm_queue[i];
                    issued_imm_enable <= next_lsu_issue_queue[i].imm_enable;
                    issued_rob_entry_index <= next_lsu_issue_queue[i].rob_entry_index;
                    issued_store_or_load <= next_lsu_issue_queue[i].store_or_load;
                    issued_bar_type <= next_lsu_issue_queue[i].bar_type;
                    issued_preg_rd <= next_lsu_issue_queue[i].rd_index;
                    issued_preg_rj <= next_lsu_issue_queue[i].rj_index;
                    issued_preg_rk <= next_lsu_issue_queue[i].rk_index;
                    issued_reg_rd_exist <= next_lsu_issue_queue[i].rd_valid;
                    issued_reg_rj_exist <= next_lsu_issue_queue[i].rj_valid;
                    issued_reg_rk_exist <= next_lsu_issue_queue[i].rk_valid;
                    issued_lsu_index <= {next_lsu_issue_queue[i].page_diff_index, next_lsu_issue_queue[i].store_block_index}; // for tracking specific instr completion status

                    // set the issue bit to 1
                    next_lsu_issue_queue[i].issued = 1'b1;

                    // LSU don't have pre-wakeup mechanism (the penalty on design is too high)
                    // when issue store (store_or_load == 1) , we will increment
                    next_store_block_index = next_store_block_index + next_lsu_issue_queue[i].store_or_load;
                    
                    // preld instr dont care the real completion status
                    // so we just set the completion bit to 1 when issued
                    if(next_lsu_op_type_queue[i][8:5] == `GENERAL_OPTYPE_LS && next_lsu_op_type_queue[i][4:0] == `LS_PRELD) begin
                        next_lsu_issue_queue[i].completion = 1'b1;
                    end else begin
                        next_lsu_issue_queue[i].completion = 1'b0;
                    end
                    break;
                end
            end
        end
        
        // compress (only operate shadow var)
        // and we should first find the first completed LSU
        // and then compress the after instrs
        for(int j=0; j<`LSU_IQ_DEPTH; j++) begin
            if(next_lsu_issue_queue[j].completion == 1'b1) begin
                // find the first completed LSU
                for(int k=j+1; k<`LSU_IQ_DEPTH; k++) begin
                    next_lsu_issue_queue[k-1] = next_lsu_issue_queue[k];
                    next_lsu_imm_queue[k-1] = next_lsu_imm_queue[k];
                    next_lsu_op_type_queue[k-1] = next_lsu_op_type_queue[k];
                    next_lsu_index_queue[k-1] = next_lsu_index_queue[k];
                end
                next_filled_entry_cnt = next_filled_entry_cnt - 1;
                break;
            end
        end

        // 3. write in new instr
        if(rename_valid && !IQ_full && alloc_success) begin
            for(int x=0; x<rename_instr_num; x=x+1) begin
                next_store_block_index = next_store_block_index + store_or_load_vec[x];
                // next_lsu_issue_queue[next_filled_entry_cnt] = 
                // {
                //     bar_type_vec[x], // 38-37
                //     store_or_load_vec[x], // 36
                //     next_store_block_index, // 35-29                   
                //     1'b0, // completion bit // 28
                //     preg_rj_vec[x], // 27-21
                //     reg_rj_exist_vec[x], // 20
                //     1'b0, // rj_ready (19)
                //     preg_rk_vec[x], // 18-12
                //     reg_rk_exist_vec[x], // 11
                //     1'b0, // rk_ready (10)
                //     preg_rd_vec[x], // 9-3
                //     reg_rd_exist_vec[x], // 2
                //     imm_enable_vec[x], // 1
                //     1'b0 // issued (0)
                // };
                next_lsu_issue_queue[next_filled_entry_cnt].rob_entry_index = rob_entry_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rj_index = preg_rj_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rj_valid = reg_rj_exist_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rj_ready = rj_ready_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rk_index = preg_rk_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rk_valid = reg_rk_exist_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rk_ready = rk_ready_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rd_index = preg_rd_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rd_valid = reg_rd_exist_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].rd_ready = rd_ready_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].imm_enable = imm_enable_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].store_or_load = store_or_load_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].bar_type = bar_type_vec[x];
                next_lsu_issue_queue[next_filled_entry_cnt].page_diff_index = next_latest_input_index[`LSU_IQ_INDEX_WIDTH];
                next_lsu_issue_queue[next_filled_entry_cnt].store_block_index = next_latest_input_index[`LSU_IQ_INDEX_WIDTH-1:0];
                next_lsu_issue_queue[next_filled_entry_cnt].completion = 1'b0;

                next_lsu_imm_queue[next_filled_entry_cnt] = imm_vec[x];
                next_lsu_op_type_queue[next_filled_entry_cnt] = {gen_op_type_vec[x], spec_op_type_vec[x]};
                next_latest_input_index = next_latest_input_index + 1;
                next_filled_entry_cnt = next_filled_entry_cnt + 1;
            end
        end

        // 4. external wake-up
        for(int i=0; i<4; i++) begin
            case(i)
                0: wb_rd_index = write_back_rd_index_0;
                1: wb_rd_index = write_back_rd_index_1;
                2: wb_rd_index = write_back_rd_index_2;
                3: wb_rd_index = write_back_rd_index_3;
            endcase
            if(write_back_rd_exist_vec[i]) begin
                for(int j=0; j<`LSU_IQ_DEPTH; j++) begin
                    if(next_lsu_issue_queue[j][`LSU_IQ_ENTRY_rj_index_pos] == wb_rd_index) begin
                        next_lsu_issue_queue[j][`LSU_IQ_ENTRY_rj_ready_pos] = 1'b1;
                    end else if(next_lsu_issue_queue[j][`LSU_IQ_ENTRY_rk_index_pos] == wb_rd_index) begin
                        next_lsu_issue_queue[j][`LSU_IQ_ENTRY_rk_ready_pos] = 1'b1;
                    end
                end
            end
        end

        // 5. finally, copy the shadow variables to the original variables
        for (int i = 0; i < `LSU_IQ_DEPTH; i++) begin
            lsu_issue_queue[i] <= next_lsu_issue_queue[i];
            lsu_imm_queue[i] <= next_lsu_imm_queue[i];
            lsu_op_type_queue[i] <= next_lsu_op_type_queue[i];
        end
        filled_entry_cnt <= next_filled_entry_cnt;
        store_block_index <= next_store_block_index;
        issued_store_block_index <= next_issued_store_block_index;
    end
end

always_comb begin
    write_back_preg_rd_index_out = 0;
    if(write_back_index_valid) begin
        for(int i=0; i<`LSU_IQ_DEPTH; i++) begin
            if(lsu_index_queue[i] == write_back_index_to_IQ) begin
                write_back_preg_rd_index_out = lsu_issue_queue[i][`LSU_IQ_ENTRY_rd_index_pos];
            end
        end
    end
end
endmodule