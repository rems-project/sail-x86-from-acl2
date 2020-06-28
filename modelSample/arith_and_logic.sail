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
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wmv, wreg, rmem, eamem, escape}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((0) == (binary_logand(1, opcode))) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let g = (rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => gpr_arith_logic_spec_1(operation, e, g, input_rflags),
2 => gpr_arith_logic_spec_2(operation, e, g, input_rflags),
4 => gpr_arith_logic_spec_4(operation, e, g, input_rflags),
_ => gpr_arith_logic_spec_8(operation, e, g, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let (flg1, x86) = (if ((operation) == (8)) | ((operation) == (7))
then (None() : option(string), 0 : int)
else x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, result, seg_reg, e_addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

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
val x86_add_adc_sub_sbb_or_and_xor_cmp_g_e : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_add_adc_sub_sbb_or_and_xor_cmp_g_e (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((0) == (binary_logand(1, opcode))) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let g = (rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => gpr_arith_logic_spec_1(operation, g, e, input_rflags),
2 => gpr_arith_logic_spec_2(operation, g, e, input_rflags),
4 => gpr_arith_logic_spec_4(operation, g, e, input_rflags),
_ => gpr_arith_logic_spec_8(operation, g, e, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let x86 = (if (operation) == (8)
then 0
else write_rgfi_size(operand_size, reg_index(reg, rex_byte, 2), result, rex_byte, 0)) : int in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

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
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wmv, wreg, rmem, eamem, escape}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let e_byte_operand? = (((opcode) == (128)) | (((opcode) == (130)) | ((opcode) == (246)))) : bool in
let e_size = (select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let imm_byte_operand? = (((opcode) == (128)) | (((opcode) == (130)) | (((opcode) == (131)) | ((opcode) == (246))))) : bool in
let imm_size = (select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, e, increment_rip_by, e_addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, e_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, imm_size, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, imm, x86) = (rme_size(proc_mode, imm_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let imm = (loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), imm))) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, imm_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let (result, output_rflags, undefined_flags) = ((match e_size {
1 => gpr_arith_logic_spec_1(operation, e, imm, input_rflags),
2 => gpr_arith_logic_spec_2(operation, e, imm, input_rflags),
4 => gpr_arith_logic_spec_4(operation, e, imm, input_rflags),
_ => gpr_arith_logic_spec_8(operation, e, imm, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let (flg1, x86) = (if ((operation) == (8)) | ((operation) == (7))
then (None() : option(string), 0 : int)
else x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, result, seg_reg, e_addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

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
val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I") : string in
let byte_operand? = ((0) == (binary_logand(1, opcode))) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false, 0)) : int in
let rax_size = (if logbitp(3, rex_byte)
then 8
else operand_size) : int in
let rax = (rgfi_size(rax_size, 0, rex_byte, 0)) : int in
let (flg, imm, x86) = (rme_size(proc_mode, operand_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let imm = (if (not_bool(byte_operand?)) & ((rax_size) == (8))
then n64(n32_to_i32(imm))
else imm) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, operand_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let (result, output_rflags, undefined_flags) = ((match rax_size {
1 => gpr_arith_logic_spec_1(operation, rax, imm, input_rflags),
2 => gpr_arith_logic_spec_2(operation, rax, imm, input_rflags),
4 => gpr_arith_logic_spec_4(operation, rax, imm, input_rflags),
_ => gpr_arith_logic_spec_8(operation, rax, imm, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let x86 = (if ((operation) == (8)) | ((operation) == (7))
then 0
else write_rgfi_size(rax_size, 0, result, rex_byte, 0)) : int in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_inc_dec_fe_ff : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wmv, wreg, rmem, eamem, escape}
function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-INC/DEC-FE-FF") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((0) == (binary_logand(1, opcode))) : bool in
let r_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, r_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let (result, output_rflags, undefined_flags) = ((match r_mem_size {
1 => gpr_arith_logic_spec_1(if (reg) == (0)
then 0
else 4, r_mem, 1, input_rflags),
2 => gpr_arith_logic_spec_2(if (reg) == (0)
then 0
else 4, r_mem, 1, input_rflags),
4 => gpr_arith_logic_spec_4(if (reg) == (0)
then 0
else 4, r_mem, 1, input_rflags),
_ => gpr_arith_logic_spec_8(if (reg) == (0)
then 0
else 4, r_mem, 1, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let output_rflags = (set_rflagsbits_get_cf(old_cf, output_rflags)) : int in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let (flg1, x86) = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, result, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_inc_dec_4x : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, escape, wreg}
function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-INC/DEC-4X") : string in
let operand_size = (select_operand_size(proc_mode, false, 0, false, prefixes, false, false, false, 0)) : int in
let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let reg = (binary_logand(7, opcode)) : int in
let operand = (rgfi_size(operand_size, reg, 0, 0)) : int in
let input_rflags = (r_rflags(0)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let (result, output_rflags, undefined_flags) = ((match operand_size {
1 => gpr_arith_logic_spec_1(if logbitp(3, opcode)
then 4
else 0, operand, 1, input_rflags),
2 => gpr_arith_logic_spec_2(if logbitp(3, opcode)
then 4
else 0, operand, 1, input_rflags),
4 => gpr_arith_logic_spec_4(if logbitp(3, opcode)
then 4
else 0, operand, 1, input_rflags),
_ => gpr_arith_logic_spec_8(if logbitp(3, opcode)
then 4
else 0, operand, 1, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let output_rflags = (set_rflagsbits_get_cf(old_cf, output_rflags)) : int in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let x86 = (write_rgfi_size(operand_size, reg, result, 0, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_not_neg_f6_f7 : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wmv, wreg, rmem, eamem, escape}
function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-NOT/NEG-F6-F7") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((0) == (binary_logand(1, opcode))) : bool in
let r_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, r_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (r_rflags(0)) : int in
let (result, output_rflags, undefined_flags) = ((match reg {
3 => (match r_mem_size {
1 => gpr_arith_logic_spec_1(4, 0, r_mem, input_rflags),
2 => gpr_arith_logic_spec_2(4, 0, r_mem, input_rflags),
4 => gpr_arith_logic_spec_4(4, 0, r_mem, input_rflags),
_ => gpr_arith_logic_spec_8(4, 0, r_mem, input_rflags)
}) : (int, int, int),
_ => (trunc(r_mem_size, lognot(r_mem)), 0 : int, 0 : int)
}) : (int, int, int)) : (int, int, int) in
let x86 = (if (reg) == (3)
then let cf = (if (0) == (r_mem)
then 0
else 1) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, output_rflags)) : int in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
0
else 0) : int in
let (flg1, x86) = (x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, result, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

