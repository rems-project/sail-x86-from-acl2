$include <prelude.sail>
$include <string.sail>


val x86_xchg : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, eamem, escape, rmem, wmv}
function x86_xchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-XCHG") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((opcode) == (134)) : bool in
let reg_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, val1, increment_rip_by, addr, x86) = (if (ash(opcode, -4)) == (9)
then (None() : option(string), rgfi_size(reg_mem_size, 0, rex_byte, 0), 0 : int, 0 : int, 0 : int)
else x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let rw_rd = (binary_logand(7, opcode)) : int in
let val2 = (if (ash(opcode, -4)) == (9)
then rgfi_size(reg_mem_size, reg_index(rw_rd, rex_byte, 0), rex_byte, 0)
else rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let (flg2, x86) = (if (ash(opcode, -4)) == (9)
then let x86 = (write_rgfi_size(reg_mem_size, 0, val2, rex_byte, 0)) : int in
(None() : option(string), 0 : int)
else x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, val2, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM-ERROR"))
else let x86 = (if (ash(opcode, -4)) == (9)
then write_rgfi_size(reg_mem_size, reg_index(rw_rd, rex_byte, 0), val1, rex_byte, 0)
else write_rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), val1, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_cmpxchg : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, eamem, wreg, escape, rmem, wmv}
function x86_cmpxchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-CMPXCHG") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((opcode) == (176)) : bool in
let reg_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let rax = (rgfi_size(reg_mem_size, 0, rex_byte, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
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
let (result, output_rflags, undefined_flags) = ((match reg_mem_size {
1 => gpr_arith_logic_spec_1(8, reg_mem, rax, input_rflags),
2 => gpr_arith_logic_spec_2(8, reg_mem, rax, input_rflags),
4 => gpr_arith_logic_spec_4(8, reg_mem, rax, input_rflags),
_ => gpr_arith_logic_spec_8(8, reg_mem, rax, input_rflags)
}) : (int, int, int)) : (int, int, int) in
let x86 = (write_user_rflags(output_rflags, undefined_flags, 0)) : int in
let (flg1, x86) = (if (result) == (0)
then let register_var = (rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, register_var, seg_reg, addr, rex_byte, r_m, mod_var, 0)
else let x86 = (write_rgfi_size(reg_mem_size, 0, reg_mem, rex_byte, 0)) : int in
(None() : option(string), 0 : int)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM-ERROR"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_two_byte_nop : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, escape, rmem, wreg}
function x86_two_byte_nop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-TWO-BYTE-NOP") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :NEXT-RIP-INVALID"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

