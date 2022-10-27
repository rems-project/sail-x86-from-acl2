$ifndef __X86_SUBROUTINE
$define __X86_SUBROUTINE
$include <prelude.sail>
$include <string.sail>

val x86_call_e8_op_en_m : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_call_e8_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CALL-E8-OP/EN-M") in
let offset_size : {|1, 2, 4|} = (check_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true))) in
let (flg0, rel16_32) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let rel16_32 : sbits(32) = (truncate(rel16_32, 32)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :RIME-SIZE-OPT-ERROR"))
else ();
let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(next_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let (flg, call_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, truncate(next_rip, 48), sail_sign_extend(rel16_32, 48))) in
let call_rip : sbits(48) = (truncate(call_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :CALL-RIP-INVALID"))
else ();
let rsp_var : sbits(64) = (read_sptr(proc_mode)) in
let addr_size : address_size = (select_address_size(proc_mode, None())) in
let (flg, new_rsp) : (option(string), sbits(64)) = (add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(addr_size), 64))) in
{ if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :SS 0 :CALL FLG)"))
else ();
let flg : option(string) = { (if (proc_mode) == (0)
then wime_size(0, select_address_size(proc_mode, Some(prefixes)), addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_sign_extend(next_rip, 128), alignment_checking_enabled_p(), false)
else wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_zero_extend(if (addr_size) == (2)
then sail_zero_extend(truncate(next_rip, 16), 32)
else truncate(next_rip, 32), 128), alignment_checking_enabled_p(), false)) } in
{ if is_some(flg)
then throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
else ();
write_iptr(proc_mode, call_rip);
write_sptr(proc_mode, new_rsp)
}
}
}
}
}
}

val x86_call_ff_2_op_en_m : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_call_ff_2_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CALL-FF/2-OP/EN-M") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let operand_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true))) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, call_rip, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let next_rip : sbits(48) = (truncate(next_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, next_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let call_rip : sbits(130) = { (if (proc_mode) == (0)
then sail_sign_extend(truncate(call_rip, 64), 130)
else sail_zero_extend(call_rip, 130)) } in
{ if not_bool(if (proc_mode) == (0)
then canonical_address_p(signed(call_rip))
else ((0) <= (signed(call_rip))) & ((signed(call_rip)) <= (unsigned(seg_hidden_limits[1]))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :BAD-RETURN-ADDRESS CALL-RIP)"))
else ();
let rsp_var : sbits(64) = (read_sptr(proc_mode)) in
let addr_size : address_size = (select_address_size(proc_mode, None())) in
let (flg, new_rsp) : (option(string), sbits(64)) = (add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(addr_size), 64))) in
{ if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :SS 0 :CALL FLG)"))
else ();
let check_alignment? : bool = (alignment_checking_enabled_p()) in
let flg : option(string) = { (if (operand_size) == (8)
then wime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, truncate(call_rip, 128), check_alignment?, false)
else wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, truncate(call_rip, 128), check_alignment?, false)) } in
{ if is_some(flg)
then throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
else ();
write_iptr(proc_mode, truncate(call_rip, 48));
write_sptr(proc_mode, new_rsp)
}
}
}
}
}
}

val x86_ret : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_ret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-RET") in
let rsp_var : sbits(64) = (read_sptr(proc_mode)) in
let operand_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, Mk_prefixes(0x0_0000_0000_0000), true, true, true))) in
let (flg, new_rsp) : (option(string), sbits(64)) = { (if (opcode) == (0xc3)
then let (flg1, new_rsp) : (option(string), sbits(64)) = (add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64))) in
if is_some(flg1)
then (flg1, 0x0000_0000_0000_0000)
else (None() : option(string), new_rsp)
else let (flg1, imm16) : (option(string), bits(16)) = (rme16(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X", false)) in
if is_some(flg1)
then (flg1, 0x0000_0000_0000_0000)
else let (flg1, new_rsp) : (option(string), sbits(64)) = (add_to_sptr(proc_mode, rsp_var, bits_of_int((operand_size) + (unsigned(imm16)), 64))) in
if is_some(flg1)
then (flg1, 0x0000_0000_0000_0000)
else (None() : option(string), new_rsp)) } in
let new_rsp : sbits(48) = (truncate(new_rsp, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :IMM-RML16-ERROR"))
else ();
let badlength? : bool = (((opcode) == (0xc2)) & (is_some(check_instruction_length(start_rip, temp_rip, 0b010)))) in
{ if badlength?
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let check_alignment? : bool = (alignment_checking_enabled_p()) in
let (flg, tos) : (option(string), sbits(130)) = { (if (operand_size) == (8)
then let (elem0, elem1) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false)) in
(elem0, sail_sign_extend(elem1, 130))
else let (elem0, elem1) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false)) in
(elem0, sail_zero_extend(elem1, 130))) } in
let tos : sbits(64) = (truncate(tos, 64)) in
{ if is_some(flg)
then throw(Emsg("This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`"))
else ();
if not_bool(if (proc_mode) == (0)
then canonical_address_p(signed(tos))
else let cs_limit : bits(32) = (seg_hidden_limits[1]) in
((0) <= (signed(tos))) & ((signed(tos)) <= (unsigned(cs_limit))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :BAD-RETURN-ADDRESS TOS)"))
else ();
write_sptr(proc_mode, sail_sign_extend(new_rsp, 64));
write_rip(truncate(tos, 48))
}
}
}

val x86_leave : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_leave (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LEAVE") in
let operand_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false))) in
let rbp_ebp_bp : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), 0x5, 0x00)) in
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let (flg, sailval) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rbp_ebp_bp, None(), 2, ":R", inst_ac?, false)) in
{ if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :SS 0 :POP-ERROR FLG)"))
else ();
let (flg, new_rsp) : (option(string), sbits(64)) = (add_to_sptr(proc_mode, rbp_ebp_bp, bits_of_int(operand_size, 64))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :INVALID-RSP"))
else ();
write_rgfi_size(bits_of_int(operand_size, 4), 0x5, unsigned(sailval), rex_byte);
write_sptr(proc_mode, new_rsp);
write_iptr(proc_mode, temp_rip)
}
}

$endif
