`include "defs.sv"

module LSU(
    input logic clk,
    input logic rst_n,

    // interface with LSU_IQ
    input lsu_issue_queue_issued_info_t issued_info,
    input logic issue_valid, // LSU_IQ output valid signal
    output logic fu_ready,

    // to read from PRF
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rj_index,
    output logic [`PREG_INDEX_WIDTH-1:0] prf_read_rd_index,
    // read from PRF or imm
    input logic [31:0] rj_val, // read from PRF using preg_rj_index_out (=preg_rj_index)
    input logic [31:0] rd_val, // read from PRF using preg_rd_index_out (=preg_rd_index)

    // lsu output
    output logic [31:0] load_data,
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] complete_rob_entry_index,
    output logic wb_rd_valid,
    output logic [`PREG_INDEX_WIDTH-1:0] wb_preg_rd_index,
    output logic complete_valid,
    output logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception,

    // cache interface
    output cache_req_t cache_req,
    input cache_resp_t cache_resp,
    
    // cacop interface
    output cacop_req_t cacop_req,
    input cacop_resp_t cacop_resp,

    // preld interface
    output preld_req_t preld_req,
    // if issue a preld, we will directly complete it (we dont care the real completion status)

    input logic llbit_active,
    input logic [31:0] llbit_addr,
    output logic llbit_clear,

    // barrier interface
    output bar_req_t bar_req,
    input bar_resp_t bar_resp,

    // write back interface
    // store retire interface
    input logic store_retire_valid;
    // need to read the preg_rd_index from LSU_IQ
    output logic [`ROB_ENTRY_INDEX_WIDTH-1:0] write_back_index_to_IQ, // use rob index to search for instr
    output logic write_back_index_valid,
    output logic write_prf_valid, // write back to PRF valid signal
    output logic [31:0] write_prf_data, // write back to PRF data
    output logic [`PREG_INDEX_WIDTH-1:0] write_back_preg_rd_index_out,
    // all these outputs are combinatorial outputs
    // input logic [`PREG_INDEX_WIDTH-1:0] write_back_preg_rd_index_in  
);

// 令preg_rj_index_out = preg_rj_index / 0 和 preg_rk_index_out = preg_rk_index / 0
always_comb begin
    if (issued_info.issued_reg_rj_exist) begin
        preg_rj_index_out = issued_info.issued_preg_rj;
    end else begin
        preg_rj_index_out = 0;
    end
    if (issued_info.issued_reg_rk_exist) begin
        preg_rk_index_out = issued_info.issued_preg_rk;
    end else begin
        preg_rk_index_out = 0;
    end
    if (issued_info.issued_reg_rd_exist) begin
        preg_rd_index_out = issued_info.issued_preg_rd;
    end else begin
        preg_rd_index_out = 0;
    end
end
// then we will read 2 values from PRF

logic normal_ls_signal; // normal load/store (including LL and SC) and Query-Index CACOP
logic cacop_signal; // other ROOT-mode CACOP
logic preld_signal; // preld
logic bar_signal; // barrier instruction

logic [31:0] final_addr; // add the reg readout value and the extended imm
logic [31:0] store_data; // store data
logic [3:0] wstrb; // 1111 -- 4bytes=1word, 0011 -- halfword, 0001 -- 1byte
logic is_write; // 1: write, 0: read
logic is_ll; // 1: LL
logic is_sc; // 1: SC
logic is_prefetch; // 1: prefetch, 0: normal load/store
logic is_qry_inv; // 1: Query-Index CACOP, 0: other ROOT-mode CACOP
logic qry_writeback; // 1: writeback, 0: not writeback
logic [1:0] size; // 00: 1byte, 01: 2bytes, 10: 4bytes
logic unsigned_load; // 1: usigned load, 0: signed load

logic is_ibar; // 1:IBAR, 0:DBAR

logic [4:0] hint; // hint code (5bits)
logic alignment_error; // check the alignment according to the instr type

STORE_BUFFER_ENTRY_t store_buffer [`STORE_BUFFER_DEPTH-1:0];
logic [`STORE_BUFFER_INDEX_WIDTH-1:0] store_buffer_head;
logic [`STORE_BUFFER_INDEX_WIDTH-1:0] store_buffer_tail;
logic [`STORE_BUFFER_INDEX_WIDTH:0] store_buffer_cnt; // 0~32

always_comb begin
    alignment_error = 0;
    normal_ls_signal = 0;
    cacop_signal = 0;
    preld_signal = 0;
    bar_signal = 0;

    final_addr = 0;
    store_data = 0;
    wstrb = 0;

    is_write = 0;
    is_ll = 0;
    is_prefetch = 0;
    is_qry_inv = 0;
    qry_writeback = 0;
    size = 2'b0;
    unsigned_load = 0;
    is_ibar = 0;

    if(issued_info.issued_gen_op_type == `GENERAL_OPTYPE_2R12I) begin
        normal_ls_signal = 1;
        case (issued_info.issued_spec_op_type)
            `_2R12I_LD_B: begin
                normal_ls_signal = 1; // 因为store buffer的存在，只能把LD指令的valid信号设置为1，ST指令只有在退休的时候才能发出dcache req
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                // because the memory is byte-addressable, so LD.B dont need to check alignment
                size = 2'b0; // load 1 byte
                unsigned_load = 0; // signed load
            end
            `_2R12I_LD_H: begin
                normal_ls_signal = 1;
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                // halfword load = 2 bytes load ,need alignment check
                size = 2'b1; // load 2 bytes
                unsigned_load = 0; // signed load
                alignment_error = final_addr[0]; // check the alignment
            end
            `_2R12I_LD_W: begin
                normal_ls_signal = 1;
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                // word load = 4 bytes load, need alignment check
                size = 2'b10; // load 4 bytes
                unsigned_load = 0; // signed load
                alignment_error = final_addr[0] || final_addr[1]; // check the alignment
            end
            `_2R12I_LD_BU: begin
                normal_ls_signal = 1;
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                size = 2'b0;
                unsigned_load = 1;
                alignment_error = 0;
            end
            `_2R12I_LD_HU: begin
                normal_ls_signal = 1;
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                size = 2'b1;
                unsigned_load = 1;
                alignment_error = final_addr[0];
            end
            // Store instr
            `_2R12I_ST_B: begin
                normal_ls_signal = 0; // 先要保存到store buffer中
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                // because the memory is byte-addressable, so ST.B dont need to check alignment
                store_data = rd_val;
                wstrb = 4'b0001; // only one byte
                is_write = 1;
            end
            `_2R12I_ST_H: begin
                normal_ls_signal = 0; // 先要保存到store buffer中
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                store_data = rd_val;
                wstrb = 4'b0011; // two bytes
                is_write = 1;
                alignment_error = final_addr[0];
            end
            `_2R12I_ST_W: begin
                normal_ls_signal = 0; // 先要保存到store buffer中
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                store_data = rd_val;
                wstrb = 4'b1111; // four bytes
                is_write = 1;
                alignment_error = final_addr[0] || final_addr[1];
            end
            // Preld instr
            `_2R12I_PRELD: begin
                preld_signal = 1;
                final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
                is_prefetch = 1;
                hint = issued_info.issued_imm[25:21]; // hint code (5bits)
            end
            default: begin
                normal_ls_signal = 0;
            end
        endcase
    end else if (issued_info.issued_gen_op_type == `GENERAL_OPTYPE_CSR) begin
        if (issued_info.issued_spec_op_type == `CSR_CACOP) begin
            cacop_signal = 1;
            final_addr = rj_val + {{20{issued_info.issued_imm[11]}}, issued_info.issued_imm[11:0]};
            hint = issued_info.issued_imm[25:21]; // cacop's code (5bits)
            if (hint[4:3] == 2'b10) begin
                // Query-Index CACOP
                normal_ls_signal = 1; // we treat the Query-Index CACOP as normal load/store instr
                is_qry_inv = 1; // 1: Query-Index CACOP
                qry_writeback = (hint[2:0] != 3'b0); // only when hint[2:0] == 0, 
                // which means we put ops on instr-cache
                // then we need to writeback the data
            end
        end
    end else if (issued_info.issued_gen_op_type == `GENERAL_OPTYPE_ATOMIC) begin
        if (issued_info.issued_spec_op_type == `ATOMIC_LL) begin
            normal_ls_signal = 1;
            is_ll = 1;
            final_addr = rj_val + {{16{issued_info.issued_imm[13]}}, issued_info.issued_imm[13:0], 2'b0};
            unsigned_load = 0;
            size = 2'b10; // load 1 word
            alignment_error = final_addr[0] || final_addr[1]; // load 1 word
        end else if (issued_info.issued_spec_op_type == `ATOMIC_SC) begin
            normal_ls_signal = 0; // 先要保存到store buffer中
            store_data = rd_val;
            is_sc = 1;
            final_addr = rj_val + {{16{issued_info.issued_imm[13]}}, issued_info.issued_imm[13:0], 2'b0};
            alignment_error = final_addr[0] || final_addr[1]; // load 1 word
        end

    end else if (issued_info.issued_gen_op_type == `GENERAL_OPTYPE_BAR) begin
        if (issued_info.issued_spec_op_type == `BAR_DBAR) begin
            bar_signal = 1;
            is_ibar = 0;
            hint = issued_info.issued_imm[4:0];  // actually we only extract the 15bit hint's LSB 5bits
        end else if (issued_info.issued_spec_op_type == `BAR_IBAR) begin
            bar_signal = 1;
            is_ibar = 1;
            hint = issued_info.issued_imm[4:0];  // actually we only extract the 15bit hint's LSB 5bits
        end
    end
end

always_comb begin
    FU_ready = (store_buffer_cnt < `STORE_BUFFER_DEPTH) && store_retire_valid;
end

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n || flush) begin
        store_buffer_head <= 0;
        store_buffer_tail <= 0;
        store_buffer_cnt <= 0;
    end else begin
        if(store_retire_valid) begin
            cache_req.rob_entry_index <= 0; // store 指令只有退休才会发出cache req，所以没有必要使用req_index
            cache_req.valid <= 1;
            cache_req.addr <= store_buffer[store_buffer_head].final_addr;
            cache_req.data <= store_buffer[store_buffer_head].rd_data;
            cache_req.wstrb <= store_buffer[store_buffer_head].wstrb;
            cache_req.is_write <= 1;
            cache_req.is_ll <= 0;
            cache_req.is_sc <= store_buffer[store_buffer_head].is_sc;
            cache_req.is_prefetch <= store_buffer[store_buffer_head].is_prefetch;
            // CACOP is not store instr
            if(issued_info.issued_store_or_load) begin
                store_buffer[store_buffer_tail].rd_data <= rd_val;
                store_buffer[store_buffer_tail].final_addr <= final_addr;
                store_buffer[store_buffer_tail].wstrb <= wstrb;
                store_buffer[store_buffer_tail].is_sc <= is_sc;
                store_buffer[store_buffer_tail].is_prefetch <= is_prefetch;
                store_buffer_tail <= store_buffer_tail + 1;
                store_buffer_cnt <= store_buffer_cnt; // 抵消了
            end else begin
                store_buffer_cnt <= store_buffer_cnt - 1;
            end
        end else begin // 没有store retire但是有load指令
            // if the store_or_load is 1, means we should insert this store instr into the store buffer
            if(issued_info.issued_store_or_load) begin
                store_buffer[store_buffer_tail].rd_data <= rd_val;
                store_buffer[store_buffer_tail].final_addr <= final_addr;
                store_buffer[store_buffer_tail].wstrb <= wstrb;
                store_buffer[store_buffer_tail].is_sc <= is_sc;
                store_buffer[store_buffer_tail].is_prefetch <= is_prefetch;
                store_buffer_tail <= store_buffer_tail + 1;
                store_buffer_cnt <= store_buffer_cnt + 1;
            end
        end
    end
end

// send out the LSU request to the cache module
always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        cache_req.rob_entry_index <= 0;
        cache_req.addr <= 0;
        cache_req.data <= 0;
        cache_req.wstrb <= 0;
        cache_req.valid <= 0;
        cache_req.is_write <= 0;
        cache_req.is_ll <= 0;
        cache_req.is_sc <= 0;
        cache_req.is_prefetch <= 0;
        cache_req.is_qry_inv <= 0;
        cache_req.qry_writeback <= 0;
        cache_req.size <= 0;
        cache_req.unsigned_load <= 0;

        cacop_req.code <= 0;
        cacop_req.addr <= 0;
        cacop_req.valid <= 0;

        preld_req.addr <= 0;
        preld_req.hint <= 0;
        preld_req.valid <= 0;

        bar_req.req_index <= 0;
        bar_req.hint <= 0;
        bar_req.valid <= 0;
        bar_req.is_ibar <= 0;
    end
    else if(!store_retire_valid) begin // 没有store要退休，正常发出cache req
        // cache_req info
        cache_req.rob_entry_index <= issued_info.issued_lsu_index;
        cache_req.addr <= final_addr;
        cache_req.data <= store_data;
        cache_req.wstrb <= wstrb;
        cache_req.valid <= normal_ls_signal || cacop_signal || preld_signal;
        cache_req.is_write <= is_write;
        cache_req.is_ll <= is_ll;
        cache_req.is_sc <= is_sc;
        cache_req.is_prefetch <= is_prefetch;
        cache_req.is_qry_inv <= is_qry_inv;
        cache_req.qry_writeback <= qry_writeback;
        cache_req.size <= size;
        cache_req.unsigned_load <= unsigned_load;

        // cacop info
        cacop_req.code <= hint;
        cacop_req.addr <= final_addr;
        cacop_req.valid <= cacop_signal;
        
        // preld info
        preld_req.addr <= final_addr;
        preld_req.hint <= hint;
        preld_req.valid <= preld_signal;

        // bar info 
        bar_req.rob_entry_index <= issued_info.issued_lsu_index;
        bar_req.hint <= hint;
        bar_req.valid <= bar_signal;
        bar_req.is_ibar <= is_ibar;
    end
end

// receive response from cache module
// we need have some var to store the response values
cache_resp_t cache_resp_reg;
cacop_resp_t cacop_resp_reg;
bar_resp_t bar_resp_reg;

always_ff @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        cache_resp_reg <= 0;
        cacop_resp_reg <= 0;
        bar_resp_reg <= 0;
    end else begin
        cache_resp_reg <= cache_resp;
        cacop_resp_reg <= cacop_resp;
        bar_resp_reg <= bar_resp;
    end
end

// split the resp to different signals
// according to the valid signals
always_comb begin
    // load_data = 0;
    complete_rob_entry_index = 0;
    complete_valid = cache_resp_reg.ready || cacop_resp_reg.ready || bar_resp_reg.ready;

    write_back_index_to_IQ = 0;
    write_back_index_valid = 0;
    write_prf_valid = 0;
    write_prf_data = 32'b0;
    write_back_preg_rd_index_out = 0;
    
    if(cache_resp_reg.ready) begin
        // load_data = cache_resp_reg.data;
        complete_rob_entry_index = cache_resp_reg.rob_entry_index;
        
        // only when the completion signal valid is 1 we need to write back the data to PRF
        write_back_index_to_IQ = cache_resp_reg.rob_entry_index;
        write_back_index_valid = 1; 
        write_prf_valid = !issued_info.issued_store_or_load; // 只有load指令需要写回寄存器
        write_prf_data = cache_resp_reg.data;
        write_back_preg_rd_index_out = cache_resp_reg.preg_rd_index;
        // 还有其他的需要写回吗？

        // 目前我先不考虑cacop了，后面再写这个csr
    end else if (cacop_resp_reg.ready) begin
        // load_data = 0;
        // cacop目前的逻辑是不完整的，后面再来补充
    end else if (bar_resp_reg.ready) begin
        // load_data = 0;
        complete_rob_entry_index = bar_resp_reg.rob_entry_index;
    end
end
// write back interface

endmodule