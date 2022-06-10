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
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((get_slice_int(8, 0, 0)) == (the_bits(8, the_bits(1, logand(get_slice_int(8, 1, 0), opcode))))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let g = (rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)) : bits(64) in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let e_addr = (the_sbits(64, e_addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(the_range(0, 8, operation), get_slice_int(8, e, 0), the_bits(8, g), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(the_range(0, 8, operation), get_slice_int(16, e, 0), the_bits(16, g), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(the_range(0, 8, operation), get_slice_int(32, e, 0), the_bits(32, g), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(the_range(0, 8, operation), get_slice_int(64, e, 0), g, input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let flg1 = (if ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 8, 0))) | ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 7, 0)))
then None() : option(string)
else x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else { write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, temp_rip)
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
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((get_slice_int(8, 0, 0)) == (the_bits(8, the_bits(1, logand(get_slice_int(8, 1, 0), opcode))))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let g = (rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)) : bits(64) in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let e_addr = (the_sbits(64, e_addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(the_range(0, 8, operation), the_bits(8, g), get_slice_int(8, e, 0), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(the_range(0, 8, operation), the_bits(16, g), get_slice_int(16, e, 0), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(the_range(0, 8, operation), the_bits(32, g), get_slice_int(32, e, 0), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(the_range(0, 8, operation), g, get_slice_int(64, e, 0), input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
{ if (get_slice_int(8, operation, 0)) == (get_slice_int(8, 8, 0))
then ()
else { write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), unsigned(result), rex_byte)
};
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, temp_rip)
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
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let e_byte_operand? = (((opcode) == (get_slice_int(8, 128, 0))) | (((opcode) == (get_slice_int(8, 130, 0))) | ((opcode) == (get_slice_int(8, 246, 0))))) : bool in
let e_size = (the_range(1, 8, select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let imm_byte_operand? = (((opcode) == (get_slice_int(8, 128, 0))) | (((opcode) == (get_slice_int(8, 130, 0))) | (((opcode) == (get_slice_int(8, 131, 0))) | ((opcode) == (get_slice_int(8, 246, 0)))))) : bool in
let imm_size = (the_range(1, 4, select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false))) : {|1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), e_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, imm_size, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let e_addr = (the_sbits(64, e_addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, imm) = (rme_size(proc_mode, imm_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), int) in
let imm = (the_bits(32, imm)) : bits(32) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let imm = (loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), unsigned(imm)))) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, imm_size, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(49, temp_rip)) : sbits(49) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match e_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(the_range(0, 8, operation), get_slice_int(8, e, 0), get_slice_int(8, imm, 0), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(the_range(0, 8, operation), get_slice_int(16, e, 0), get_slice_int(16, imm, 0), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(the_range(0, 8, operation), get_slice_int(32, e, 0), get_slice_int(32, imm, 0), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(the_range(0, 8, operation), get_slice_int(64, e, 0), get_slice_int(64, imm, 0), input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let flg1 = (if ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 8, 0))) | ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 7, 0)))
then None() : option(string)
else x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else { write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, the_sbits(48, temp_rip))
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
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I") : string in
let byte_operand? = ((get_slice_int(8, 0, 0)) == (logand(get_slice_int(8, 1, 0), opcode))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let rax_size = (if logbitp(3, rex_byte)
then 8
else operand_size) : {|8, 1, 2, 4|} in
let rax_var = (rgfi_size(get_slice_int(4, rax_size, 0), get_slice_int(4, 0, 0), rex_byte)) : bits(64) in
let (flg, imm) = (rme_size(proc_mode, operand_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let imm = (if (not_bool(byte_operand?)) & ((get_slice_int(8, rax_size, 0)) == (get_slice_int(8, 8, 0)))
then n64(the_bits(64, n32_to_i32(the_bits(32, imm))))
else the_bits(64, the_bits(32, imm))) : bits(64) in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, operand_size, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(49, temp_rip)) : sbits(49) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match rax_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(the_range(0, 8, operation), the_bits(8, rax_var), the_bits(8, imm), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(the_range(0, 8, operation), the_bits(16, rax_var), the_bits(16, imm), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(the_range(0, 8, operation), the_bits(32, rax_var), the_bits(32, imm), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(the_range(0, 8, operation), rax_var, imm, input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
{ if ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 8, 0))) | ((get_slice_int(8, operation, 0)) == (get_slice_int(8, 7, 0)))
then ()
else { write_rgfi_size(get_slice_int(4, rax_size, 0), get_slice_int(4, 0, 0), unsigned(result), rex_byte)
};
write_user_rflags(output_rflags, undefined_flags);
write_iptr(proc_mode, the_sbits(48, temp_rip))
}

val x86_inc_dec_fe_ff : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-INC/DEC-FE-FF") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((get_slice_int(8, 0, 0)) == (logand(get_slice_int(8, 1, 0), opcode))) : bool in
let r_mem_size = (the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, r_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let (result, output_rflags, undefined_flags) = ((match r_mem_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(if (the_bits(8, reg)) == (get_slice_int(8, 0, 0))
then 0
else 4, get_slice_int(8, r_mem, 0), get_slice_int(8, 1, 0), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(if (the_bits(8, reg)) == (get_slice_int(8, 0, 0))
then 0
else 4, get_slice_int(16, r_mem, 0), get_slice_int(16, 1, 0), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(if (the_bits(8, reg)) == (get_slice_int(8, 0, 0))
then 0
else 4, get_slice_int(32, r_mem, 0), get_slice_int(32, 1, 0), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(if (the_bits(8, reg)) == (get_slice_int(8, 0, 0))
then 0
else 4, get_slice_int(64, r_mem, 0), get_slice_int(64, 1, 0), input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (set_rflagsbits_get_cf(old_cf, output_rflags)) : bits(32) in
{ write_user_rflags(output_rflags, undefined_flags);
let flg1 = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, temp_rip)
}

val x86_inc_dec_4x : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, undef, wreg}
function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-INC/DEC-4X") : string in
let operand_size = (the_range(2, 4, select_operand_size(proc_mode, false, get_slice_int(8, 0, 0), false, prefixes, false, false, false))) : {|2, 4|} in
let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let reg = (the_bits(3, logand(get_slice_int(8, 7, 0), opcode))) : bits(3) in
let operand = (rgfi_size(get_slice_int(4, operand_size, 0), the_bits(4, reg), get_slice_int(8, 0, 0))) : bits(64) in
let input_rflags = (rflags) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(if logbitp(3, opcode)
then 4
else 0, the_bits(8, operand), get_slice_int(8, 1, 0), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(if logbitp(3, opcode)
then 4
else 0, the_bits(16, operand), get_slice_int(16, 1, 0), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(if logbitp(3, opcode)
then 4
else 0, the_bits(32, operand), get_slice_int(32, 1, 0), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(if logbitp(3, opcode)
then 4
else 0, operand, get_slice_int(64, 1, 0), input_rflags)
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (set_rflagsbits_get_cf(old_cf, output_rflags)) : bits(32) in
{ write_user_rflags(output_rflags, undefined_flags);
write_rgfi_size(get_slice_int(4, operand_size, 0), the_bits(4, reg), unsigned(result), get_slice_int(8, 0, 0));
write_iptr(proc_mode, temp_rip)
}

val x86_not_neg_f6_f7 : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-NOT/NEG-F6-F7") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((get_slice_int(8, 0, 0)) == (logand(get_slice_int(8, 1, 0), opcode))) : bool in
let r_mem_size = (the_range(0, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, r_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match reg {
3 => (match r_mem_size {
1 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_1(4, get_slice_int(8, 0, 0), get_slice_int(8, r_mem, 0), input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_2(4, get_slice_int(16, 0, 0), get_slice_int(16, r_mem, 0), input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (gpr_arith_logic_spec_4(4, get_slice_int(32, 0, 0), get_slice_int(32, r_mem, 0), input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
_ => gpr_arith_logic_spec_8(4, get_slice_int(64, 0, 0), get_slice_int(64, r_mem, 0), input_rflags)
}) : (bits(64), bits(32), bits(32)),
_ => (sail_mask(64, trunc(r_mem_size, get_slice_int(64, lognot(r_mem), 0))), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
{ if (the_bits(8, reg)) == (get_slice_int(8, 3, 0))
then { let cf = (the_bits(1, if (get_slice_int(8, 0, 0)) == (get_slice_int(8, r_mem, 0))
then 0
else 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, output_rflags)) : bits(32) in
write_user_rflags(output_rflags, undefined_flags)
}
else ();
let flg1 = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, temp_rip)
}

