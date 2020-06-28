$include <prelude.sail>
$include <string.sail>

val x86_bt_0f_a3 : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wreg, rmem, escape}
function x86_bt_0f_a3 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-BT-0F-A3") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let operand_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let bitoffset = (rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let (flg2, bitoffset, bitbase, x86) = (if (mod_var) == (3)
then (None() : option(string), mod(bitoffset, ash(operand_size, 3)), rgfi_size(operand_size, reg_index(r_m, rex_byte, 0), rex_byte, 0), 0 : int)
else let bitoffset_int = ((match operand_size {
1 => n08_to_i08(bitoffset),
2 => n16_to_i16(bitoffset),
4 => n32_to_i32(bitoffset),
_ => n64_to_i64(bitoffset)
}) : int) : int in
let bitoffset_int_abs = (abs(bitoffset_int)) : int in
let bitnumber = (mod(bitoffset_int_abs, 8)) : int in
let byte_addr = ((addr) + (floor2(bitoffset_int, 8))) : int in
let inst_ac? = (alignment_checking_enabled_p(0)) : bool in
let (flg, byte, x86) = (if signed_byte_p(64, byte_addr)
then rme_size(proc_mode, 1, byte_addr, seg_reg, ":R", inst_ac?, 0, struct {mem_ptr? = false})
else (Some("EFFECTIVE-ADDRESS-ERROR"), 0 : int, 0 : int)) : (option(string), int, int) in
(flg, bitnumber, byte, 0 : int)) : (option(string), int, int, int) in
if is_some(flg2)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let x86 = (let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(logbit(bitoffset, bitbase), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_bt_0f_ba : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wreg, rmem, escape}
function x86_bt_0f_ba (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-BT-0F-BA") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let operand_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, bitbase, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, bitoffset, x86) = (rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let bitoffset = (mod(bitoffset, ash(operand_size, 3))) : int in
let x86 = (let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(logbit(bitoffset, bitbase), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

