$include "./arith_and_logic_spec.sail"
$include "./decoding_and_spec_utils.sail"

/*! Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( E, G )
<h3>Op/En = MR: [OP R/M, REG] or [OP E, G]</h3> 
 
 <p>where @ ( ' E ' ) is the destination operand and @ ( ' G ' ) is the source 
 operand. Note that @ ( ' E ' ) stands for a general-purpose register or 
 memory operand specified by the @ ( ' ModRM.r/m ' ) field, and @ ( ' G ' ) 
 stands for a general-purpose register specified by the 
 @ ( ' ModRM.reg ' ) field.</p> 
 
 [OP R/M, REG] Flags Affected<br/> 
 00, 01: ADD c p a z s o<br/> 
 08, 09: OR p z s ( o and c cleared, a undefined ) <br/> 
 10, 11: ADC c p a z s o<br/> 
 18, 19: SBB c p a z s o<br/> 
 20, 21: AND p z s ( o and c cleared, a undefined ) <br/> 
 28, 29: SUB c p a z s o<br/> 
 30, 31: XOR p z s ( o and c cleared, a undefined ) <br/> 
 38, 39: CMP c p a z s o<br/> 
 84, 85: TEST p z s ( o and c cleared, a undefined ) <br/> */
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g : (range(0, 8), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = 0b0 == truncate(logand(0x01, opcode), 1);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let g : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (e, increment_rip_by, e_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(operand_size, operation, truncate(e, 64), g, input_rflags);
    if operation == 8 | operation == 7 then () else {
        x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(result, 128), seg_reg, e_addr, prefixes, rex_byte, r_m, mod_var, sib)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, temp_rip)
}

/*! Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP: Addressing Mode = ( G, E )
<h3>Op/En = RM: [OP REG, R/M] or [OP G, E]</h3> 
 
 <p>where @ ( ' G ' ) is the destination operand and @ ( ' E ' ) is the source 
 operand. Note that @ ( ' G ' ) stands for a general-purpose register 
 specified by the @ ( ' ModRM.reg ' ) field, and @ ( ' E ' ) stands for a 
 general-purpose register or memory operand specified by the 
 @ ( ' ModRM.r/m ' ) field.</p> 
 
 [OP REG, R/M] Flags Affected<br/> 
 02, 03: ADD c p a z s o<br/> 
 0A, 0B: OR p z s ( o and c cleared, a undefined ) <br/> 
 12, 13: ADC c p a z s o<br/> 
 1A, 1B: SBB c p a z s o<br/> 
 22, 23: AND p z s ( o and c cleared, a undefined ) <br/> 
 2A, 2B: SUB c p a z s o<br/> 
 32, 33: XOR p z s ( o and c cleared, a undefined ) <br/> 
 3A, 3B: CMP c p a z s o <br/> */
val x86_add_adc_sub_sbb_or_and_xor_cmp_g_e : (range(0, 8), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_g_e (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = 0b0 == truncate(logand(0x01, opcode), 1);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let g : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (e, increment_rip_by, e_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(operand_size, operation, g, truncate(e, 64), input_rflags);
    if operation == 8 then () else {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), result, rex_byte)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, temp_rip)
}

/*! Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( E, I )
<h3>Op/En = MI: [OP R/M, IMM] or [OP E, I]</h3> 
 
 <p>where @ ( ' E ' ) is the destination operand and @ ( ' I ' ) is the source 
 operand. Note that @ ( ' E ' ) stands for a general-purpose register or 
 memory operand specified by the @ ( ' ModRM.r/m ' ) field, and @ ( ' I ' ) 
 stands for immediate data. All opcodes except those of TEST fall 
 under Group 1, and have opcode extensions ( ModR/M.reg field ) , as 
 per Table A-6 of the Intel Manuals, Vol. 2. The opcodes for TEST 
 fall under Unary Group 3, and also have opcode extensions.</p> 
 
 [OP R/M, IMM] Flags Affected<br/> 
 80-83 ( 000 ) : ADD c p a z s o<br/> 
 80-83 ( 001 ) : OR p z s ( o and c cleared, a undefined ) <br/> 
 80-83 ( 010 ) : ADC c p a z s o<br/> 
 80-83 ( 011 ) : SBB c p a z s o<br/> 
 80-83 ( 100 ) : AND p z s ( o and c cleared, a undefined ) <br/> 
 80-83 ( 101 ) : SUB c p a z s o<br/> 
 80-83 ( 110 ) : XOR p z s ( o and c cleared, a undefined ) <br/> 
 80-83 ( 111 ) : CMP c p a z s o<br/> 
 F6-F7 ( 000 ) : TEST p z s ( o and c cleared, a undefined ) <br/> */
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i : (range(0, 8), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let e_byte_operand? : bool = opcode == 0x80 | opcode == 0x82 | opcode == 0xf6;
    let e_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false);
    let imm_byte_operand? : bool = opcode == 0x80 | opcode == 0x82 | opcode == 0x83 | opcode == 0xf6;
    let imm_size : {|1, 2, 4|} = check_range(1, 4, select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false));
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (e, increment_rip_by, e_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, e_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3));
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let imm : bits(32) = truncate(rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), imm_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false), 32);
    let imm : int = loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), unsigned(imm)));
    let temp_rip : sbits(49) = sail_sign_extend(add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48)), 49);
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(e_size, operation, truncate(e, 64), bits_of_int(imm, 64), input_rflags);
    if operation == 8 | operation == 7 then () else {
        x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, sail_zero_extend(result, 128), seg_reg, e_addr, prefixes, rex_byte, r_m, mod_var, sib)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

/*! Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( rAX, I )
<h3>Op/En = I: [OP rAX, IMM] or [OP rAX, I]</h3> 
 
 <p>where @ ( ' rAX ' ) is the destination operand and @ ( ' I ' ) is the 
 source operand. Note that @ ( ' rAX ' ) stands for AL/AX/EAX/RAX, 
 depending on the operand size, and @ ( ' I ' ) stands for immediate 
 data.</p> 
 
 [OP rAX, IMM] Flags Affected<br/> 
 04, 05: ADD c p a z s o<br/> 
 0C, 0D: OR p z s ( o and c cleared, a undefined ) <br/> 
 14, 15: ADC c p a z s o<br/> 
 1C, 1D: SBB c p a z s o<br/> 
 24, 25: AND p z s ( o and c cleared, a undefined ) <br/> 
 2C, 2D: SUB c p a z s o<br/> 
 34, 35: XOR p z s ( o and c cleared, a undefined ) <br/> 
 3C, 3D: CMP c p a z s o<br/> 
 A8, A9: TEST p z s ( o and c cleared, a undefined ) <br/> */
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (range(0, 8), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I";
    let byte_operand? : bool = 0x00 == logand(0x01, opcode);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false);
    let rax_size : {|1, 2, 4, 8|} = {
        if logbitp(3, rex_byte) then 8 else {
            operand_size
        }
    };
    let rax_var : bits(64) = rgfi_size(bits_of_int(rax_size, 4), 0x0, rex_byte);
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let imm : bits(64) = {
        if not_bool(byte_operand?) & rax_size == 8 then {
            sail_sign_extend(truncate(imm, 32), 64)
        } else {
            sail_zero_extend(truncate(imm, 32), 64)
        }
    };
    let temp_rip : sbits(49) = sail_sign_extend(add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48)), 49);
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(rax_size, operation, rax_var, imm, input_rflags);
    if operation == 8 | operation == 7 then () else {
        write_rgfi_size(bits_of_int(rax_size, 4), 0x0, result, rex_byte)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

val x86_inc_dec_fe_ff : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-INC/DEC-FE-FF";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = 0x00 == logand(0x01, opcode);
    let r_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (r_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let old_cf : bits(1) = input_rflags[cf];
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(r_mem_size, if reg == 0b000 then 0 else 4, truncate(r_mem, 64), 0x0000000000000001, input_rflags);
    let output_rflags : rflagsbits = [output_rflags with cf = old_cf];
    write_user_rflags(output_rflags, undefined_flags);
    x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, sail_zero_extend(result, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

val x86_inc_dec_4x : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-INC/DEC-4X";
    let operand_size : {|2, 4|} = check_range(2, 4, select_operand_size(proc_mode, false, 0x00, false, prefixes, false, false, false));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let reg : bits(3) = truncate(logand(0x07, opcode), 3);
    let operand : bits(64) = rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), 0x00);
    let input_rflags : rflagsbits = rflags;
    let old_cf : bits(1) = input_rflags[cf];
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(operand_size, if logbitp(3, opcode) then 4 else 0, operand, 0x0000000000000001, input_rflags);
    let output_rflags : rflagsbits = [output_rflags with cf = old_cf];
    write_user_rflags(output_rflags, undefined_flags);
    write_rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), result, 0x00);
    write_iptr(proc_mode, temp_rip)
}

val x86_not_neg_f6_f7 : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-NOT/NEG-F6-F7";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = 0x00 == logand(0x01, opcode);
    let r_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (r_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = {
        match reg {
          3 => gpr_arith_logic_spec(r_mem_size, 4, 0x0000000000000000, truncate(r_mem, 64), input_rflags),
          _ => (sail_mask(64, trunc(r_mem_size, lognot(r_mem))), Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
        }
    };
    if reg == 0b011 then {
        let cf : bits(1) = {
            if 0x00000000000000000000000000000000 == r_mem then 0b0 else 0b1
        };
        let output_rflags : rflagsbits = [output_rflags with cf];
        write_user_rflags(output_rflags, undefined_flags)
    };
    x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, sail_zero_extend(result, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}
