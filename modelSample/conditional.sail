$include <prelude.sail>
$include <string.sail>

val jcc_cmovcc_setcc_spec : (int, int) -> bool effect {escape, rreg}
function jcc_cmovcc_setcc_spec (opcode, x86) =
let low_nibble = (binary_logand(opcode, 15)) : int in
(match low_nibble {
0 => (1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var)),
1 => (0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var)),
2 => (1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_cf(rflags_var)),
3 => (0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_cf(rflags_var)),
4 => (1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var)),
5 => (0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var)),
6 => ((1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_cf(rflags_var))) | ((1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var))),
7 => ((0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_cf(rflags_var))) & ((0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var))),
8 => (1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)),
9 => (0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)),
10 => (1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_pf(rflags_var)),
11 => (0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_pf(rflags_var)),
12 => not_bool((let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var))),
13 => (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var)),
14 => ((1) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var))) | (not_bool((let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var)))),
15 => ((0) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_zf(rflags_var))) & ((let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_sf(rflags_var)) == (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_of(rflags_var))),
_ => false
}) : bool

val x86_one_byte_jcc : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_one_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-ONE-BYTE-JCC") : string in
let badlength? = (check_instruction_length(start_rip, temp_rip, 1)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let branch_cond = (jcc_cmovcc_setcc_spec(opcode, 0)) : bool in
if branch_cond
then let (flg, rel8, x86) = (rime_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, (1) + (rel8), 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0

val x86_two_byte_jcc : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_two_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-TWO-BYTE-JCC") : string in
let offset_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true, 0)) : int in
let badlength? = (check_instruction_length(start_rip, temp_rip, offset_size)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let branch_cond = (jcc_cmovcc_setcc_spec(opcode, 0)) : bool in
if branch_cond
then let (flg, offset, x86) = (rime_size(proc_mode, offset_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, (offset_size) + (offset), 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, offset_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0

val x86_jrcxz : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_jrcxz (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-JRCXZ") : string in
let badlength? = (check_instruction_length(start_rip, temp_rip, 1)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let register_size = (select_address_size(proc_mode, p4?, 0)) : int in
let branch_cond = ((rgfi_size(register_size, 1, rex_byte, 0)) == (0)) : bool in
if branch_cond
then let (flg, rel8, x86) = (rime_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, (1) + (rel8), 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let x86 = (write_iptr(proc_mode, next_rip, 0)) : int in
0

val x86_cmovcc : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_cmovcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-CMOVCC") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let operand_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let branch_cond = (jcc_cmovcc_setcc_spec(opcode, 0)) : bool in
let x86 = (if branch_cond
then write_rgfi_size(operand_size, reg_index(reg, rex_byte, 2), reg_mem, rex_byte, 0)
else 0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_setcc : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_setcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-SETCC") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let branch_cond = (jcc_cmovcc_setcc_spec(opcode, 0)) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let sailval = (if branch_cond
then 1
else 0) : int in
let (flg2, x86) = (x86_operand_to_reg_mem(proc_mode, 1, inst_ac?, false, sailval, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

