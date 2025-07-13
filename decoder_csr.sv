`include "defs.sv"

module decoder_csr (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [3:0] csr_op_type
);

// Internal signal for instruction field extraction
logic [18:0] instr_extraction_24_23_21_to_17_14_13_9_to_0;

always_comb begin
    // Default to invalid operation
    csr_op_type = `INVALID_OP_4B;

    // Extract instruction fields for easier comparison
    instr_extraction_24_23_21_to_17_14_13_9_to_0 = {instr[24:23], instr[21:17], instr[14:13], instr[9:0]};

    // Check if this is a CSR instruction (opcode = 6'b0000_01)
    if (instr[31:26] == 6'b0000_01) begin
        // Check basic CSR operations (instr[25] == 0)
        if (instr[25] == 1'b0) begin
            if (instr[24] == 1'b0) begin
                // Check CSR read/write operations
                if (instr[9:5] == 5'b00000) begin
                    csr_op_type = `CSR_CSRRD;    // CSR read
                end else if (instr[9:5] == 5'b00001) begin
                    csr_op_type = `CSR_CSRWR;    // CSR write
                end else begin
                    csr_op_type = `CSR_CSRXCHG;  // CSR exchange
                end
            end
        end
        // Check advanced CSR operations (instr[25] == 1)
        else begin
            // Check TLB and cache operations
            if (instr_extraction_24_23_21_to_17_14_13_9_to_0 == 19'b00_00100_01_0000_0000_00) begin
                // Check cache operation (instr[22] == 0)
                if (instr[22] == 1'b0) begin
                    csr_op_type = `CSR_CACOP;    // Cache operation
                end
                // Check TLB operations (instr[22] == 1)
                else begin
                    case (instr[16:15])
                        2'b00: begin
                            // Check specific TLB operations
                            case (instr[12:10])
                                3'b010: csr_op_type = `CSR_TLBSRCH;  // TLB search
                                3'b011: csr_op_type = `CSR_TLBRD;    // TLB read
                                3'b100: csr_op_type = `CSR_TLBWR;    // TLB write
                                3'b101: csr_op_type = `CSR_TLBFILL;  // TLB fill
                                3'b110: csr_op_type = `CSR_ERTN;     // Exception return
                                3'b111: csr_op_type = `CSR_IDLE;     // Idle
                                default: csr_op_type = `INVALID_OP_4B;
                            endcase
                        end
                        2'b01: csr_op_type = `CSR_ERTN;     // Exception return
                        2'b11: csr_op_type = `CSR_INVTLB;   // Invalidate TLB
                        default: csr_op_type = `INVALID_OP_4B;
                    endcase
                end
            end
        end
    end
end

endmodule