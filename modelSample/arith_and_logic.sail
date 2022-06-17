$include <prelude.sail>
$include <string.sail>
$include "arith_and_logic_spec.sail"
$include "decoding_and_spec_utils.sail"
/*Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( E, G )*/
/*<h3>Op/En = MR: [OP R/M, REG] or [OP E, G]</h3> 
 
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
 84, 85: TEST p z s ( o and c cleared, a undefined ) <br/>*/
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((0b0) == (truncate(logand(0x01, opcode), 1))) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let g : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match operand_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(operation, bits_of_int(e, 8), truncate(g, 8), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(operation, bits_of_int(e, 16), truncate(g, 16), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(operation, bits_of_int(e, 32), truncate(g, 32), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(operation, bits_of_int(e, 64), g, input_rflags)
}) in
let flg1 : option(string) = (if ((operation) == (8)) | ((operation) == (7))
then None() : option(string)
else x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP: Addressing Mode = ( G, E )*/
/*<h3>Op/En = RM: [OP REG, R/M] or [OP G, E]</h3> 
 
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
 3A, 3B: CMP c p a z s o <br/>*/
val x86_add_adc_sub_sbb_or_and_xor_cmp_g_e : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, undef, wreg}
function x86_add_adc_sub_sbb_or_and_xor_cmp_g_e (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((0b0) == (truncate(logand(0x01, opcode), 1))) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let g : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match operand_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(operation, truncate(g, 8), bits_of_int(e, 8), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(operation, truncate(g, 16), bits_of_int(e, 16), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(operation, truncate(g, 32), bits_of_int(e, 32), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(operation, g, bits_of_int(e, 64), input_rflags)
}) in
{ if (operation) == (8)
then ()
else { write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(result), rex_byte)
};
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( E, I )*/
/*<h3>Op/En = MI: [OP R/M, IMM] or [OP E, I]</h3> 
 
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
 F6-F7 ( 000 ) : TEST p z s ( o and c cleared, a undefined ) <br/>*/
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let e_byte_operand? : bool = (((opcode) == (0x80)) | (((opcode) == (0x82)) | ((opcode) == (0xf6)))) in
let e_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let imm_byte_operand? : bool = (((opcode) == (0x80)) | (((opcode) == (0x82)) | (((opcode) == (0x83)) | ((opcode) == (0xf6))))) in
let imm_size : {|1, 2, 4|} = (check_range(1, 4, select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, e_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3))) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg1, imm) : (option(string), int) = (rme_size(proc_mode, imm_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
let imm : bits(32) = (bits_of_int(imm, 32)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
let imm : int = (loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), unsigned(imm)))) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match e_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(operation, bits_of_int(e, 8), bits_of_int(imm, 8), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(operation, bits_of_int(e, 16), bits_of_int(imm, 16), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(operation, bits_of_int(e, 32), bits_of_int(imm, 32), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(operation, bits_of_int(e, 64), bits_of_int(imm, 64), input_rflags)
}) in
let flg1 : option(string) = (if ((operation) == (8)) | ((operation) == (7))
then None() : option(string)
else x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}
}
}

/*Operand Fetch and Execute for ADD, ADC, SUB, SBB, OR, AND, 
 XOR, CMP, TEST: Addressing Mode = ( rAX, I )*/
/*<h3>Op/En = I: [OP rAX, IMM] or [OP rAX, I]</h3> 
 
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
 A8, A9: TEST p z s ( o and c cleared, a undefined ) <br/>*/
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, undef, wreg}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I") in
let byte_operand? : bool = ((0x00) == (logand(0x01, opcode))) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false)) in
let rax_size : {|1, 2, 4, 8|} = (if logbitp(3, rex_byte)
then 8
else operand_size) in
let rax_var : bits(64) = (rgfi_size(bits_of_int(rax_size, 4), 0x0, rex_byte)) in
let (flg, imm) : (option(string), int) = (rme_size(proc_mode, operand_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
let imm : bits(64) = (if (not_bool(byte_operand?)) & ((rax_size) == (8))
then n64(sail_zero_extend(n32_to_i32(bits_of_int(imm, 32)), 64))
else sail_zero_extend(bits_of_int(imm, 32), 64)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match rax_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(operation, truncate(rax_var, 8), truncate(imm, 8), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(operation, truncate(rax_var, 16), truncate(imm, 16), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(operation, truncate(rax_var, 32), truncate(imm, 32), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(operation, rax_var, imm, input_rflags)
}) in
{ if ((operation) == (8)) | ((operation) == (7))
then ()
else { write_rgfi_size(bits_of_int(rax_size, 4), 0x0, unsigned(result), rex_byte)
};
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_inc_dec_fe_ff : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-INC/DEC-FE-FF") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let select_byte_operand : bool = ((0x00) == (logand(0x01, opcode))) in
let r_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, r_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let old_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match r_mem_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(if (reg) == (0b000)
then 0
else 4, bits_of_int(r_mem, 8), 0x01, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(if (reg) == (0b000)
then 0
else 4, bits_of_int(r_mem, 16), 0x0001, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(if (reg) == (0b000)
then 0
else 4, bits_of_int(r_mem, 32), 0x0000_0001, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(if (reg) == (0b000)
then 0
else 4, bits_of_int(r_mem, 64), 0x0000_0000_0000_0001, input_rflags)
}) in
let output_rflags : bits(32) = (set_rflagsbits_get_cf(old_cf, output_rflags)) in
{ write_user_rflags(output_rflags, undefined_flags);
let flg1 : option(string) = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, temp_rip)
}
}
}
}
}

val x86_inc_dec_4x : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, undef, wreg}
function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-INC/DEC-4X") in
let operand_size : {|2, 4|} = (check_range(2, 4, select_operand_size(proc_mode, false, 0x00, false, prefixes, false, false, false))) in
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let reg : bits(3) = (truncate(logand(0x07, opcode), 3)) in
let operand : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), 0x00)) in
let input_rflags : bits(32) = (rflags) in
let old_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match operand_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(if logbitp(3, opcode)
then 4
else 0, truncate(operand, 8), 0x01, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(if logbitp(3, opcode)
then 4
else 0, truncate(operand, 16), 0x0001, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(if logbitp(3, opcode)
then 4
else 0, truncate(operand, 32), 0x0000_0001, input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(if logbitp(3, opcode)
then 4
else 0, operand, 0x0000_0000_0000_0001, input_rflags)
}) in
let output_rflags : bits(32) = (set_rflagsbits_get_cf(old_cf, output_rflags)) in
{ write_user_rflags(output_rflags, undefined_flags);
write_rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), unsigned(result), 0x00);
write_iptr(proc_mode, temp_rip)
}
}

val x86_not_neg_f6_f7 : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-NOT/NEG-F6-F7") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let select_byte_operand : bool = ((0x00) == (logand(0x01, opcode))) in
let r_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, r_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (match reg {
3 => (match r_mem_size {
1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = (gpr_arith_logic_spec_1(4, 0x00, bits_of_int(r_mem, 8), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) = (gpr_arith_logic_spec_2(4, 0x0000, bits_of_int(r_mem, 16), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) = (gpr_arith_logic_spec_4(4, 0x0000_0000, bits_of_int(r_mem, 32), input_rflags)) in
(sail_zero_extend(elem0, 64), elem1, elem2),
_ => gpr_arith_logic_spec_8(4, 0x0000_0000_0000_0000, bits_of_int(r_mem, 64), input_rflags)
}) : (bits(64), bits(32), bits(32)),
_ => (sail_mask(64, trunc(r_mem_size, bits_of_int(lognot(r_mem), 64))), 0x0000_0000, 0x0000_0000)
}) in
{ if (reg) == (0b011)
then { let cf : bits(1) = (bits_of_int(if (0) == (r_mem)
then 0
else 1, 1)) in
let output_rflags : bits(32) = (set_rflagsbits_get_cf(cf, output_rflags)) in
write_user_rflags(output_rflags, undefined_flags)
}
else ();
let flg1 : option(string) = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, temp_rip)
}
}
}
}
}

