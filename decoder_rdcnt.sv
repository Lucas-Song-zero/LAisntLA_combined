`include "defs.sv"

module decoder_rdcnt (
    // inputs
    input logic [31:0] instr,
    // outputs
    output logic [1:0] rdcnt_op_type
);

always_comb begin
    // Default to invalid operation
    rdcnt_op_type = `INVALID_OP_2B;
    
    // Check if this is a RDCNT instruction
    // Format: 000_0000_0000_00000_00000_00000_000000
    if (instr[31:11] == 21'b000_0000_0000_00000_00000_00000_000000) begin
        // Check RDCNTVH instruction (instr[10] == 1)
        if (instr[10] == 1'b1) begin
            rdcnt_op_type = `RDCNT_RDCNTVH;
        end
        // Check RDCNTID instruction (rd == 0, instr[4:0] == 5'b00000)
        else if (instr[4:0] == 5'b00000) begin
            rdcnt_op_type = `RDCNT_RDCNTID;
        end
        // Default to RDCNTVL instruction
        else begin
            rdcnt_op_type = `RDCNT_RDCNTVL;
        end
    end
end

endmodule