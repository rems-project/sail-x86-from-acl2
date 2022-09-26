$ifndef __X86_BIT
$define __X86_BIT
$include <prelude.sail>
$include <string.sail>

val x86_bt_0f_a3 : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv}
function x86_bt_0f_a3 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-BT-0F-A3") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let bitoffset : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = { (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else let p4? : bool = ((0x67) == ((prefixes)[adr])) in
x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) } in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let (flg2, bitoffset, bitbase) : (option(string), int, bits(128)) = { (if (mod_var) == (0b11)
then (None() : option(string), mod(unsigned(bitoffset), ash(operand_size, 3)), sail_zero_extend(rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), rex_byte), 128))
else let bitoffset_int : sbits(64) = { (match operand_size {
1 => sail_sign_extend(truncate(bitoffset, 8), 64),
2 => sail_sign_extend(truncate(bitoffset, 16), 64),
4 => sail_sign_extend(truncate(bitoffset, 32), 64),
_ => bitoffset
}) } in
let bitoffset_int_abs : int = (abs(signed(bitoffset_int))) in
let bitnumber : int = (mod(bitoffset_int_abs, 8)) in
let byte_addr : int = ((signed(addr)) + (floor2(signed(bitoffset_int), 8))) in
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let (flg, byte) : (option(string), bits(128)) = { (if signed_byte_p(64, byte_addr)
then rme_size(proc_mode, 1, bits_of_int(byte_addr, 64), seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})
else (Some("EFFECTIVE-ADDRESS-ERROR"), 0x0000_0000_0000_0000_0000_0000_0000_0000)) } in
(flg, bitnumber, byte)) } in
{ if is_some(flg2)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
{ let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (logbit(bitoffset, bitbase))]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with pf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with af = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with sf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (sailval)]) in
rflags = (new_rflags).bits
};
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_bt_0f_ba : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv}
function x86_bt_0f_ba (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-BT-0F-BA") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, bitbase, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b001)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg1, bitoffset) : (option(string), bits(128)) = (rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
let bitoffset : bits(8) = (truncate(bitoffset, 8)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let bitoffset : range(0, 64) = (check_range(0, 64, mod(unsigned(bitoffset), ash(operand_size, 3)))) in
{ { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (logbit(bitoffset, bitbase))]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with pf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with af = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with sf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (sailval)]) in
rflags = (new_rflags).bits
};
write_iptr(proc_mode, temp_rip)
}
}
}
}
}
}

$endif
