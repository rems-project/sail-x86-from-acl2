$include <prelude.sail>
$include <string.sail>

val x86_call_e8_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_call_e8_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-CALL-E8-OP/EN-M") : string in
let offset_size = (the_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true))) : {|1, 2, 4|} in
let (flg0, rel16_32) = (rime_size(proc_mode, offset_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
let rel16_32 = (the_sbits(32, rel16_32)) : sbits(32) in
if is_some(flg0)
then throw(Emsg("Model state error: :RIME-SIZE-OPT-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, offset_size, 0))) : (option(string), sbits(52)) in
let next_rip = (the_sbits(49, next_rip)) : sbits(49) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, next_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let (flg, call_rip) = (add_to_iptr(proc_mode, the_sbits(48, next_rip), the_sbits(48, rel16_32))) : (option(string), sbits(52)) in
let call_rip = (the_sbits(48, call_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :CALL-RIP-INVALID"))
else let rsp_var = (read_sptr(proc_mode)) : bits(64) in
let addr_size = (the_range(2, 8, select_address_size(proc_mode, false))) : {|8, 2, 4|} in
let (flg, new_rsp) = (add_to_sptr(proc_mode, rsp_var, get_slice_int(64, negate(addr_size), 0))) : (option(string), sbits(67)) in
if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"))
else let flg = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then wime_size(0, addr_size, the_sbits(64, the_sbits(48, new_rsp)), 2, signed(next_rip), alignment_checking_enabled_p(), struct {mem_ptr? = false})
else wme_size(proc_mode, addr_size, the_sbits(64, the_sbits(48, new_rsp)), 2, unsigned(if (the_range(0, 5, addr_size)) == (2)
then the_bits(32, n16(the_bits(16, next_rip)))
else n32(the_bits(32, next_rip))), alignment_checking_enabled_p(), struct {mem_ptr? = false})) : option(string) in
if is_some(flg)
then throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
else { write_iptr(proc_mode, call_rip);
write_sptr(proc_mode, the_sbits(64, new_rsp))
}

val x86_call_ff_2_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_call_ff_2_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-CALL-FF/2-OP/EN-M") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let operand_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true))) : {|8, 2, 4|} in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, call_rip, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let next_rip = (the_sbits(48, next_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, next_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let call_rip = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then signed(I64(get_slice_int(64, call_rip, 0)))
else call_rip) : int in
if not_bool(if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then canonical_address_p(get_slice_int(48, call_rip, 0))
else ((0) <= (call_rip)) & ((call_rip) <= (unsigned(seg_hidden_limits[1]))))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'CALL-RIP']"))
else let rsp_var = (read_sptr(proc_mode)) : bits(64) in
let addr_size = (the_range(2, 8, select_address_size(proc_mode, false))) : {|8, 2, 4|} in
let (flg, new_rsp) = (add_to_sptr(proc_mode, rsp_var, get_slice_int(64, negate(addr_size), 0))) : (option(string), sbits(67)) in
if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"))
else let check_alignment? = (alignment_checking_enabled_p()) : bool in
let flg = (if (the_range(0, 5, operand_size)) == (the_range(0, 5, 8))
then wime_size(proc_mode, operand_size, rsp_var, 2, call_rip, check_alignment?, struct {mem_ptr? = false})
else wme_size(proc_mode, operand_size, rsp_var, 2, the_nat(call_rip), check_alignment?, struct {mem_ptr? = false})) : option(string) in
if is_some(flg)
then throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
else { write_iptr(proc_mode, get_slice_int(48, call_rip, 0));
write_sptr(proc_mode, the_sbits(64, new_rsp))
}

val x86_ret : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_ret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-RET") : string in
let rsp_var = (read_sptr(proc_mode)) : bits(64) in
let operand_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, get_slice_int(52, 0, 0), true, true, true))) : {|8, 2, 4|} in
let (flg, new_rsp) = (if (opcode) == (get_slice_int(8, 195, 0))
then let (elem0, elem1) = (let (flg1, new_rsp) = (add_to_sptr(proc_mode, rsp_var, get_slice_int(64, operand_size, 0))) : (option(string), sbits(67)) in
if is_some(flg1)
then (flg1, get_slice_int(68, 0, 0))
else (None() : option(string), the_sbits(68, new_rsp))) : (option(string), sbits(68)) in
(elem0, the_sbits(69, elem1))
else let (flg1, imm16) = (rme16(proc_mode, the_sbits(64, temp_rip), 1, ":X", false)) : (option(string), int) in
let imm16 = (the_bits(16, imm16)) : bits(16) in
if is_some(flg1)
then (flg1, get_slice_int(69, 0, 0))
else let (elem0, elem1) = (let (flg1, new_rsp) = (add_to_sptr(proc_mode, rsp_var, get_slice_int(64, (operand_size) + (unsigned(imm16)), 0))) : (option(string), sbits(67)) in
if is_some(flg1)
then (flg1, get_slice_int(68, 0, 0))
else (None() : option(string), the_sbits(68, new_rsp))) : (option(string), sbits(68)) in
(elem0, the_sbits(69, elem1))) : (option(string), sbits(69)) in
let new_rsp = (the_sbits(48, new_rsp)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :IMM-RML16-ERROR"))
else let badlength? = (((opcode) == (get_slice_int(8, 194, 0))) & (is_some(check_instruction_length(start_rip, temp_rip, get_slice_int(3, 2, 0))))) : bool in
if badlength?
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let check_alignment? = (alignment_checking_enabled_p()) : bool in
let (flg, tos) = (if (the_range(0, 5, operand_size)) == (the_range(0, 5, 8))
then let (elem0, elem1) = (rime_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
(elem0, signed(elem1))
else rme_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, struct {mem_ptr? = false})) : (option(string), int) in
let tos = (the_sbits(64, tos)) : sbits(64) in
if is_some(flg)
then throw(Emsg("This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`"))
else if not_bool(if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then canonical_address_p(the_sbits(48, tos))
else let cs_limit = (seg_hidden_limits[1]) : bits(32) in
((0) <= (signed(tos))) & ((signed(tos)) <= (unsigned(cs_limit))))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'TOS']"))
else { write_sptr(proc_mode, the_sbits(64, new_rsp));
rip = the_bits(64, the_sbits(48, tos))
}

val x86_leave : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_leave (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LEAVE") : string in
let operand_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false))) : {|8, 2, 4|} in
let rbp_ebp_bp = (rgfi_size(get_slice_int(4, operand_size, 0), get_slice_int(4, 5, 0), get_slice_int(8, 0, 0))) : bits(64) in
let inst_ac? = (alignment_checking_enabled_p()) : bool in
let (flg, sailval) = (rme_size(proc_mode, operand_size, I64(rbp_ebp_bp), 2, ":R", inst_ac?, struct {mem_ptr? = false})) : (option(string), int) in
if is_some(flg)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':SS', '0', ':POP-ERROR', 'FLG']"))
else let (flg, new_rsp) = (add_to_sptr(proc_mode, I64(rbp_ebp_bp), get_slice_int(64, operand_size, 0))) : (option(string), sbits(67)) in
let new_rsp = (the_sbits(64, new_rsp)) : sbits(64) in
if is_some(flg)
then throw(Emsg("Model state error: :INVALID-RSP"))
else { write_rgfi_size(get_slice_int(4, operand_size, 0), get_slice_int(4, 5, 0), sailval, rex_byte);
write_sptr(proc_mode, new_rsp);
write_iptr(proc_mode, temp_rip)
}

