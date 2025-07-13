`include "defs.sv"

module decoder (
    // Input instruction
    input logic [31:0] instr,    // 32-bit instruction to decode
    // input logic [31:0] pc,       // 当前指令的PC

    // Decoded instruction outputs
    output logic [3:0] gen_op_type,   // General operation type (3R, 2R12I, BJ, CSR, etc.)
    output logic [4:0] spec_op_type,  // Specific operation type within general category
    output logic [25:0] imm,          // 26-bit immediate value (sign/zero extended as needed)
    output logic imm_enable,          // Immediate enable signal (1=use immediate)
    output logic imm_sign_extend,     // 是否需要符号拓展，1 -- 需要，0 -- 不需要
    output logic [4:0] reg_rd,        // Destination register index
    output logic [4:0] reg_rj,        // Source register 1 index
    output logic [4:0] reg_rk,        // Source register 2 index
    output logic reg_rd_exist,        // Destination register exists (1=valid)
    output logic reg_rj_exist,        // Source register 1 exists (1=valid)
    output logic reg_rk_exist,        // Source register 2 exists (1=valid)
    output logic valid,               // Valid instruction signal (1=instruction is valid)
    output logic store_or_load,       // Store or load signal (1=store, 0=load)
    output logic [1:0] bar_type,      // 屏障类型，主要是DBAR和IBAR
    output logic [`ROB_MAINBODY_ENTRY_exception_width-1:0] exception_state, // 异常状态，主要是CSR指令或者不存在的指令
    output logic completion_bit,      // 完成位，表示指令是否可以直接完成(一些指令不需要等待，发射之后直接标记完成即可)
    output logic [1:0] issue_distr_direction, // 指令发射方向，分成00 -- simple_IQ, 01 -- complex_IQ, 10 -- LSU_IQ
    output logic B_exist, // 表示这一条指令是B指令，即直接跳转指令
    output logic [31:0] B_jump_offset, // 表示B指令的跳转偏移量
    output logic branch_exist // 表示这一条指令是分支指令
);

// Internal signals for individual decoders
// These signals connect to the outputs of individual decoder modules
logic [4:0] _3r_op_type;         // 3-register operation type (ADD, SUB, etc.)
logic [1:0] rdcnt_op_type;       // Read counter operation type (RDCNTID, RDCNTVL, RDCNTVH)
logic [1:0] bar_op_type;         // Barrier operation type (DBAR, IBAR)
logic [1:0] atomic_op_type;      // Atomic operation type (LL, SC)
logic [1:0] sysc_brk_op_type;    // System call/break operation type (SYSCALL, BREAK)
logic [3:0] _2r12i_op_type;      // 2-register 12-bit immediate operation type (ADDI, LD, ST, etc.)
logic [3:0] csr_op_type;         // CSR operation type (CSRRD, CSRWR, etc.)
logic [1:0] _1r20i_op_type;      // 1-register 20-bit immediate operation type (LU12I, PCADDU12I)
logic [1:0] _2r5i_op_type;       // 2-register 5-bit immediate operation type (SLLI, SRLI, SRAI)
logic [3:0] bj_op_type;          // Branch/jump operation type (B, BL, BEQ, etc.)
logic [9:0] instruction_type_onehot;

// Instantiate individual decoders
// Each decoder module handles a specific instruction format and outputs its operation type
decoder_3r decoder_3r_inst (
    .instr(instr),
    ._3r_op_type(_3r_op_type)
);

decoder_rdcnt decoder_rdcnt_inst (
    .instr(instr),
    .rdcnt_op_type(rdcnt_op_type)
);

decoder_bar decoder_bar_inst (
    .instr(instr),
    .bar_op_type(bar_op_type)
);

decoder_atomic decoder_atomic_inst (
    .instr(instr),
    .atomic_op_type(atomic_op_type)
);

decoder_sysc_brk decoder_sysc_brk_inst (
    .instr(instr),
    .sysc_brk_op_type(sysc_brk_op_type)
);

decoder_2r12i decoder_2r12i_inst (
    .instr(instr),
    ._2r12i_op_type(_2r12i_op_type)
);

decoder_csr decoder_csr_inst (
    .instr(instr),
    .csr_op_type(csr_op_type)
);

decoder_1r20i decoder_1r20i_inst (
    .instr(instr),
    ._1r20i_op_type(_1r20i_op_type)
);

decoder_2r5i decoder_2r5i_inst (
    .instr(instr),
    ._2r5i_op_type(_2r5i_op_type)
);

decoder_bj decoder_bj_inst (
    .instr(instr),
    .bj_op_type(bj_op_type)
);

// Combinational decoder logic
// This block combines results from individual decoders and generates unified outputs
always_comb begin
    // Pre-assign register indices to reduce power and delay
    // Extract register fields from instruction (always do this)
    reg_rd = instr[4:0];    // rd is always at bits [4:0]
    reg_rj = instr[9:5];    // rj is always at bits [9:5] 
    reg_rk = instr[14:10];  // rk is always at bits [14:10]
    
    store_or_load = 1'b0; // set default value to avoid latch
    bar_type = 2'b00; // default value
    completion_bit = 1'b0; // default value
    exception_state = '0; // default no exception
    issue_distr_direction = 2'b00; // 00 -- simple_IQ, 01 -- complex_IQ, 10 -- LSU_IQ
    // 11 -- 直接到ROB，最后直接按顺序退休
    imm_sign_extend = 1'b0; // 默认零拓展
    B_exist = 1'b0; // 默认不是B指令
    B_jump_offset = 26'b0; // 默认B指令的跳转偏移量为0
    branch_exist = 1'b0; // 默认不是分支指令

    // Create one-hot instruction type vector for parallel decoding
    // Each bit represents a valid instruction type (only one can be 1)
    instruction_type_onehot = {
        (_3r_op_type != `INVALID_OP_5B) ? 1'b1 : 1'b0,
        (_2r12i_op_type != `INVALID_OP_4B) ? 1'b1 : 1'b0,
        (bj_op_type != `INVALID_OP_4B) ? 1'b1 : 1'b0,
        (csr_op_type != `INVALID_OP_4B) ? 1'b1 : 1'b0,
        (rdcnt_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0,
        (bar_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0,
        (atomic_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0,
        (sysc_brk_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0,
        (_2r5i_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0,
        (_1r20i_op_type != `INVALID_OP_2B) ? 1'b1 : 1'b0
    };
    
    // Use case statement on one-hot vector for final result
    // Only modify existence flags and other outputs, not register indices
    case (instruction_type_onehot)
        10'b1000000000: begin  // 3R instruction
            gen_op_type = `GENERAL_OPTYPE_3R;
            spec_op_type = _3r_op_type;
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b1;
            imm = 26'b0;
            imm_enable = 1'b0;  // 3R instructions don't use immediate
            valid = 1'b1;
            case(_3r_op_type)
                `_3R_DIV: issue_distr_direction = 2'b01;
                `_3R_DIVU: issue_distr_direction = 2'b01;
                `_3R_MOD: issue_distr_direction = 2'b01;
                `_3R_MODU: issue_distr_direction = 2'b01;
                default: issue_distr_direction = 2'b00;
            endcase
        end
        
        10'b0100000000: begin  // 2R12I instruction (ADDI, LD, ST, etc.)
            gen_op_type = `GENERAL_OPTYPE_2R12I;
            spec_op_type = {1'b0, _2r12i_op_type};  // Zero-extend 4-bit to 5-bit
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm_enable = 1'b1;  // 2R12I instructions use immediate

            // Extract and sign/zero extend 12-bit immediate based on instruction type
            case(_2r12i_op_type)
                // Arithmetic and logic instructions using 12-bit sign-extended immediate
                // 因为这里imm也只是26位，并非最终的结果，所以这里imm不需要考虑是否做符号还是零拓展
                // 但是需要传递imm_sign_extend信号，免得后面FU再判断指令类型了
                `_2R12I_SLTI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1; 
                end
                `_2R12I_SLTUI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1; 
                end
                `_2R12I_ADDI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1; 
                end
                // Logic instructions use zero-extended immediate
                `_2R12I_ANDI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b0; 
                end
                `_2R12I_ORI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b0; 
                end
                `_2R12I_XORI: begin
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b0; 
                end
                // Load/Store instructions use sign-extended immediate for address offset
                `_2R12I_LD_B: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b0; // 0 means load
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_LD_H: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b0; // 0 means load
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_LD_W: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b0; // 0 means load
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_ST_B: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b1; // 1 means store
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_ST_H: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b1; // 1 means store
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_ST_W: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b1; // 1 means store
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_LD_BU: begin  // LD_BU是指load出来的Byte认为无符号
                // 但是立即数是符号拓展的，所以这里需要传递imm_sign_extend信号 = 1
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b0; // 0 means load
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_LD_HU: begin 
                    imm = {{14{1'b0}}, instr[21:10]};
                    imm_sign_extend = 1'b1;
                    store_or_load = 1'b0; // 0 means load
                    issue_distr_direction = 2'b10; // LSU_IQ
                end
                `_2R12I_PRELD: begin 
                    reg_rd_exist = 1'b0;
                    imm = {instr[4:0], {9{1'b0}}, instr[21:10]};
                    // 加载hint到26位imm的高位上
                    imm_sign_extend = 1'b1;
                    // because preld use 2 imm: sign-extended offset and hint code (5btis)
                    // so we need to combine them together into the 26-bit immediate to pass donw
                    store_or_load = 1'b0; // 0 means load
                    completion_bit = 1'b1; // preld指令不需要等待，直接标记完成
                end
                default: imm = 26'b0;
            endcase
            valid = 1'b1;
        end
        
        10'b0010000000: begin  // BJ instruction (B, BL, BEQ, etc.)
            gen_op_type = `GENERAL_OPTYPE_BJ;
            spec_op_type = {1'b0, bj_op_type};  // Zero-extend 4-bit to 5-bit
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm_enable = 1'b1;  // BJ instructions use immediate
            branch_exist = 1'b1;
            // Extract immediate and adjust register existence based on BJ instruction type
            case(bj_op_type)
                // B and BL instructions: no registers, 26-bit immediate
                `BJ_B: begin
                    // imm = {instr[9:0],instr[25:10]};  // 26-bit branch offset
                    // imm_sign_extend = 1'b1;
                    // 实际上B和BL指令的立即数还有两bit的偏移，但是这里在后面FU里面判断的时候再说了
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    B_exist = 1'b1;
                    B_jump_offset = {14{instr[9]}, instr[9:0], instr[25:10], 2'b00};
                end
                `BJ_BL: begin
                    imm = {instr[9:0],instr[25:10]};  // 26-bit branch offset
                    imm_sign_extend = 1'b1;
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                end
                // Other BJ instructions: use rj register, 16-bit immediate
                default: begin
                    imm = {{10{1'b0}}, instr[25:10]};  // Sign-extend 16-bit to 26-bit
                    imm_sign_extend = 1'b1;
                end
            endcase
            valid = 1'b1;
        end
        
        10'b0001000000: begin  // CSR instruction (CSRRD, CSRWR, etc.)
            gen_op_type = `GENERAL_OPTYPE_CSR;
            spec_op_type = {1'b0, csr_op_type};  // Zero-extend 4-bit to 5-bit
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm_enable = 1'b1;  // Most CSR instructions use immediate
            completion_bit = 1'b1;
            issue_distr_direction = 2'b11; // CSR要求顺序的执行
            // Extract immediate and adjust register existence based on CSR instruction type
            case(csr_op_type)
                // Basic CSR operations: 14-bit CSR address
                `CSR_CSRRD: begin
                    imm = {{12{1'b0}},instr[23:10]};
                    imm_sign_extend = 1'b0;
                    exception_state = `EXCEPTION_CSR_INSTR;
                end
                `CSR_CSRWR: begin
                    imm = {{12{1'b0}},instr[23:10]};
                    imm_sign_extend = 1'b0;
                    exception_state = `EXCEPTION_CSR_INSTR;
                end
                `CSR_CSRXCHG: begin
                    imm = {{12{1'b0}},instr[23:10]};
                    imm_sign_extend = 1'b0;
                    exception_state = `EXCEPTION_CSR_INSTR;
                end
                // Cache operation: combines code and offset
                `CSR_CACOP: begin
                    reg_rd_exist = 1'b0;
                    // Combines 2 immediates: code[4:0] and offset[21:10]
                    // Code at MSB, offset at LSB for later splitting
                    imm = {instr[4:0],{9'b0}, instr[21:10]};
                    issue_distr_direction = 2'b10; // 这里CACOP如果是hit类的，或许可以提前执行？
                    // 把hint加载到了26bit imm的高位上
                    if(instr[4:3] != 2) begin
                        exception_state = `EXCEPTION_CSR_INSTR;
                    end else begin
                        issue_distr_direction = 2'b11; // CSR要求顺序的执行
                    end
                end
                // TLB operations: no registers, no immediate
                `CSR_TLBSRCH: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm_enable = 1'b0;
                end
                `CSR_TLBRD: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm_enable = 1'b0;
                end
                `CSR_TLBWR: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm_enable = 1'b0;
                end
                `CSR_TLBFILL: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm_enable = 1'b0;
                end
                `CSR_ERTN: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm_enable = 1'b0;
                end
                // IDLE instruction: 15-bit immediate
                `CSR_IDLE: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b0;
                    reg_rk_exist = 1'b0;
                    imm = {{11{1'b0}},instr[14:0]};
                end
                // INVTLB instruction: uses rj and rk, 5-bit immediate
                `CSR_INVTLB: begin
                    reg_rd_exist = 1'b0;
                    reg_rj_exist = 1'b1;
                    reg_rk_exist = 1'b1;
                    imm = {{21{1'b0}},instr[4:0]};
                end
                default: imm = 26'b0;
            endcase
            valid = 1'b1;
        end
        
        10'b0000100000: begin  // RDCNT instruction (RDCNTID, RDCNTVL, RDCNTVH)
            gen_op_type = `GENERAL_OPTYPE_RDCNT;
            spec_op_type = {3'b000, rdcnt_op_type};
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm = 26'b0;
            imm_enable = 1'b0;  // RDCNT instructions don't use immediate
            issue_distr_direction = 2'b00; // simple_IQ (或者应该在最后的时候按顺序再读取？如果提前读取会变快x)
            // RDCNT如果需要保证顺序的执行，就需要把issue_distr_direction设置为2'b11
            valid = 1'b1;
        end
        
        10'b0000010000: begin  // BAR instruction (DBAR, IBAR)
            gen_op_type = `GENERAL_OPTYPE_BAR;
            spec_op_type = {3'b000, bar_op_type};
            reg_rd_exist = 1'b0;
            reg_rj_exist = 1'b0;
            reg_rk_exist = 1'b0;
            imm = {11'b0, instr[14:0]}; // keep the bar's hint field
            imm_enable = 1'b1;
            issue_distr_direction = 2'b10; // LSU_IQ
            case (bar_op_type)
                `BAR_DBAR: begin
                    bar_type = 2'b01; // dbar
                end
                `BAR_IBAR: begin
                    bar_type = 2'b10; // ibar
                end
                default: bar_type = 2'b00;
            endcase
            valid = 1'b1;
        end
        
        10'b0000001000: begin  // ATOMIC instruction (LL, SC)
            gen_op_type = `GENERAL_OPTYPE_ATOMIC;
            spec_op_type = {3'b000, atomic_op_type};
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm = {{12{instr[23]}}, instr[23:10]};  // 14-bit sign-extended offset
            imm_enable = 1'b1;
            issue_distr_direction = 2'b10; // LSU_IQ
            case (atomic_op_type)
                `ATOMIC_LL: begin
                    store_or_load = 1'b0; // 0 means load
                end
                `ATOMIC_SC: begin
                    store_or_load = 1'b1; // 1 means store
                end
            endcase
            valid = 1'b1;
        end
        
        10'b0000000100: begin  // SYSC_BRK instruction (SYSCALL, BREAK)
            gen_op_type = `GENERAL_OPTYPE_SYSC_BRK;
            spec_op_type = {3'b000, sysc_brk_op_type};
            reg_rd_exist = 1'b0;
            reg_rj_exist = 1'b0;
            reg_rk_exist = 1'b0;
            imm = {{11{1'b0}},instr[14:0]};  // 15-bit immediate
            exception_state = `EXCEPTION_SYSCALL_BREAK;
            issue_distr_direction = 2'b11; // 这种指令要求顺序的执行
            imm_enable = 1'b1;
            valid = 1'b1;
        end
        
        10'b0000000010: begin  // 2R5I instruction (SLLI, SRLI, SRAI)
            gen_op_type = `GENERAL_OPTYPE_2R5I;
            spec_op_type = {3'b000, _2r5i_op_type};
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b1;
            reg_rk_exist = 1'b0;
            imm = {{21{1'b0}}, instr[14:10]};  // Zero-extended 5-bit shift amount
            imm_sign_extend = 1'b0;
            imm_enable = 1'b1;  // 2R5I instructions use immediate
            valid = 1'b1;
        end
        
        10'b0000000001: begin  // 1R20I instruction (LU12I, PCADDU12I)
            gen_op_type = `GENERAL_OPTYPE_1R20I;
            spec_op_type = {3'b000, _1r20i_op_type};
            reg_rd_exist = 1'b1;
            reg_rj_exist = 1'b0;
            reg_rk_exist = 1'b0;
            imm = {{6{instr[24]}}, instr[24:5]};  // Sign-extended 20-bit immediate
            imm_enable = 1'b1;  // 1R20I instructions use immediate
            valid = 1'b1;
        end
        
        default: begin  // Invalid instruction or multiple matches
            valid = 1'b0;
            gen_op_type = 4'b0000;
            spec_op_type = 5'b00000;
            reg_rd_exist = 1'b0;
            reg_rj_exist = 1'b0;
            reg_rk_exist = 1'b0;
            imm = 26'b0;
            imm_enable = 1'b0;
            imm_sign_extend = 1'b0;
            store_or_load = 1'b0;
            bar_type = 2'b00;
            completion_bit = 1'b0;
            exception_state = '0;
            issue_distr_direction = 2'b00;
            B_exist = 1'b0;
            B_jump_offset = 26'b0;
            branch_exist = 1'b0;
        end
    endcase
end

endmodule