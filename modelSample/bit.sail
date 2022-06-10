$include <prelude.sail>
$include <string.sail>

val x86_bt_0f_a3 : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, undef}
function x86_bt_0f_a3 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-BT-0F-A3") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let bitoffset = (rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)) : bits(64) in
let (flg0, addr, increment_rip_by) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let (flg2, bitoffset, bitbase) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then (None() : option(string), mod(unsigned(bitoffset), ash(operand_size, 3)), unsigned(rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), rex_byte)))
else let bitoffset_int = ((match operand_size {
1 => the_sbits(64, n08_to_i08(the_bits(8, bitoffset))),
2 => the_sbits(64, n16_to_i16(the_bits(16, bitoffset))),
4 => the_sbits(64, n32_to_i32(the_bits(32, bitoffset))),
_ => n64_to_i64(bitoffset)
}) : sbits(64)) : sbits(64) in
let bitoffset_int_abs = (abs(signed(bitoffset_int))) : int in
let bitnumber = (mod(bitoffset_int_abs, 8)) : int in
let byte_addr = ((signed(addr)) + (floor2(signed(bitoffset_int), 8))) : int in
let inst_ac? = (alignment_checking_enabled_p()) : bool in
let (flg, byte) = (if signed_byte_p(64, byte_addr)
then rme_size(proc_mode, 1, get_slice_int(64, byte_addr, 0), seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})
else (Some("EFFECTIVE-ADDRESS-ERROR"), 0 : {|0|})) : (option(string), int) in
(flg, bitnumber, byte)) : (option(string), int, int) in
if is_some(flg2)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else { { let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_cf(logbit(bitoffset, bitbase), rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : bits(32) in
rflags = new_rflags
};
write_iptr(proc_mode, temp_rip)
}

val x86_bt_0f_ba : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, undef}
function x86_bt_0f_ba (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-BT-0F-BA") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, bitbase, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 1, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, bitoffset) = (rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), int) in
let bitoffset = (the_bits(8, bitoffset)) : bits(8) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let bitoffset = (the_range(0, 64, mod(unsigned(bitoffset), the_range(0, 64, ash(operand_size, 3))))) : range(0, 64) in
{ { let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_cf(logbit(bitoffset, bitbase), rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : bits(32) in
rflags = new_rflags
};
write_iptr(proc_mode, temp_rip)
}

