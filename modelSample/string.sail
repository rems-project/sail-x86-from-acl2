$include <prelude.sail>
$include <string.sail>


val x86_movs : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movs (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVS") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let group_1_prefix : bits(8) = (prefixes_get_rep(prefixes)) in
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let df : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_df(rflags_var), 2), 1)) in
let counter_addr_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let select_byte_operand : bool = ((0xa4) == (opcode)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let counter_addr_size_2_4? : bool = (((counter_addr_size) == (2)) | ((counter_addr_size) == (4))) in
let src_addr : sbits(65) = { (if counter_addr_size_2_4?
then sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x6, rex_byte), 65)
else sail_sign_extend(rgfi(6), 65)) } in
{ if (not_bool(counter_addr_size_2_4?)) & (not_bool(canonical_address_p(signed(src_addr))))
then throw(Emsg("Model state error: :SRC-ADDR-NOT-CANONICAL"))
else ();
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let (flg0, src) : (option(string), bits(128)) = (rme_size(proc_mode, operand_size, truncate(src_addr, 64), seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :SRC-RME-SIZE-ERROR"))
else ();
let dst_addr : sbits(65) = { (if counter_addr_size_2_4?
then sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
else sail_sign_extend(rgfi(7), 65)) } in
{ if (not_bool(counter_addr_size_2_4?)) & (not_bool(canonical_address_p(signed(dst_addr))))
then throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
else ();
let original_dst_addr : sbits(48) = (truncate(dst_addr, 48)) in
let (src_addr, dst_addr) : (int, int) = { (match operand_size {
1 => { if (df) == (0b0)
then ((1) + (signed(truncate(src_addr, 48))), (1) + (signed(truncate(dst_addr, 48))))
else ((-1) + (signed(truncate(src_addr, 48))), (-1) + (signed(truncate(dst_addr, 48)))) },
2 => { if (df) == (0b0)
then ((2) + (signed(truncate(src_addr, 48))), (2) + (signed(truncate(dst_addr, 48))))
else ((-2) + (signed(truncate(src_addr, 48))), (-2) + (signed(truncate(dst_addr, 48)))) },
4 => { if (df) == (0b0)
then ((4) + (signed(truncate(src_addr, 48))), (4) + (signed(truncate(dst_addr, 48))))
else ((-4) + (signed(truncate(src_addr, 48))), (-4) + (signed(truncate(dst_addr, 48)))) },
_ => { if (df) == (0b0)
then ((8) + (signed(truncate(src_addr, 48))), (8) + (signed(truncate(dst_addr, 48))))
else ((-8) + (signed(truncate(src_addr, 48))), (-8) + (signed(truncate(dst_addr, 48)))) }
}) } in
let src_addr : sbits(49) = (bits_of_int(src_addr, 49)) in
let dst_addr : sbits(49) = (bits_of_int(dst_addr, 49)) in
let flg1 : option(string) = (wme_size(proc_mode, operand_size, sail_sign_extend(original_dst_addr, 64), 0, unsigned(src), inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :WME-SIZE-ERROR"))
else ();
(match group_1_prefix {
243 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b0))
then { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
}
else { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
} },
242 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b1))
then { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
else { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
} },
_ => write_iptr(proc_mode, temp_rip)
}) : unit;
(match counter_addr_size {
2 => write_rgfi_size(0x2, 0x6, unsigned(truncate(src_addr, 16)), rex_byte),
4 => write_rgfi_size(0x4, 0x6, unsigned(truncate(src_addr, 32)), rex_byte),
_ => write_rgfi(6, sail_sign_extend(src_addr, 64))
}) : unit;
(match counter_addr_size {
2 => write_rgfi_size(0x2, 0x7, unsigned(truncate(dst_addr, 16)), rex_byte),
4 => write_rgfi_size(0x4, 0x7, unsigned(truncate(dst_addr, 32)), rex_byte),
_ => write_rgfi(7, sail_sign_extend(dst_addr, 64))
}) : unit
}
}
}
}
}

val x86_cmps : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_cmps (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CMPS") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let group_1_prefix : bits(8) = (prefixes_get_rep(prefixes)) in
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let df : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_df(rflags_var), 2), 1)) in
let counter_addr_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let select_byte_operand : bool = ((0xa6) == (opcode)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let counter_addr_size_2_4? : bool = (((counter_addr_size) == (2)) | ((counter_addr_size) == (4))) in
let src_addr : sbits(65) = { (if counter_addr_size_2_4?
then sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x6, rex_byte), 65)
else sail_sign_extend(rgfi(6), 65)) } in
{ if (not_bool(counter_addr_size_2_4?)) & (not_bool(canonical_address_p(signed(src_addr))))
then throw(Emsg("Model state error: :SRC-ADDR-NOT-CANONICAL"))
else ();
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let (flg0, src) : (option(string), bits(128)) = (rme_size(proc_mode, operand_size, truncate(src_addr, 64), seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :SRC-RME-SIZE-ERROR"))
else ();
let dst_addr : sbits(65) = { (if counter_addr_size_2_4?
then sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
else sail_sign_extend(rgfi(7), 65)) } in
{ if (not_bool(counter_addr_size_2_4?)) & (not_bool(canonical_address_p(signed(dst_addr))))
then throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
else ();
let (flg0, dst) : (option(string), bits(128)) = (rme_size(proc_mode, operand_size, truncate(dst_addr, 64), 0, ":R", inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :DST-RME-SIZE-ERROR"))
else ();
let input_rflags : bits(32) = (rflags) in
let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = (gpr_arith_logic_spec(operand_size, 4, truncate(dst, 64), truncate(src, 64), input_rflags)) in
{ write_user_rflags(output_rflags, undefined_flags);
let (src_addr, dst_addr) : (int, int) = { (match operand_size {
1 => { if (df) == (0b0)
then ((signed(truncate(src_addr, 48))) + (1), (signed(truncate(dst_addr, 48))) + (1))
else ((signed(truncate(src_addr, 48))) - (1), (signed(truncate(dst_addr, 48))) - (1)) },
2 => { if (df) == (0b0)
then ((signed(truncate(src_addr, 48))) + (2), (signed(truncate(dst_addr, 48))) + (2))
else ((signed(truncate(src_addr, 48))) - (2), (signed(truncate(dst_addr, 48))) - (2)) },
4 => { if (df) == (0b0)
then ((signed(truncate(src_addr, 48))) + (4), (signed(truncate(dst_addr, 48))) + (4))
else ((signed(truncate(src_addr, 48))) - (4), (signed(truncate(dst_addr, 48))) - (4)) },
_ => { if (df) == (0b0)
then ((signed(truncate(src_addr, 48))) + (8), (signed(truncate(dst_addr, 48))) + (8))
else ((signed(truncate(src_addr, 48))) - (8), (signed(truncate(dst_addr, 48))) - (8)) }
}) } in
let src_addr : sbits(49) = (bits_of_int(src_addr, 49)) in
let dst_addr : sbits(49) = (bits_of_int(dst_addr, 49)) in
{ (match group_1_prefix {
243 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b0))
then { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
}
else { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
} },
242 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b1))
then { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
else { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
} },
_ => write_iptr(proc_mode, temp_rip)
}) : unit;
(match counter_addr_size {
2 => write_rgfi_size(0x2, 0x6, unsigned(truncate(src_addr, 16)), rex_byte),
4 => write_rgfi_size(0x4, 0x6, unsigned(truncate(src_addr, 32)), rex_byte),
_ => write_rgfi(6, sail_sign_extend(src_addr, 64))
}) : unit;
(match counter_addr_size {
2 => write_rgfi_size(0x2, 0x7, unsigned(truncate(dst_addr, 16)), rex_byte),
4 => write_rgfi_size(0x4, 0x7, unsigned(truncate(dst_addr, 32)), rex_byte),
_ => write_rgfi(7, sail_sign_extend(dst_addr, 64))
}) : unit
}
}
}
}
}
}
}

val x86_stos : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_stos (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-STOS") in
let group_1_prefix : bits(8) = (prefixes_get_seg(prefixes)) in
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let df : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_df(rflags_var), 2), 1)) in
let counter_addr_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let counter_addr_size_2_4? : bool = (((counter_addr_size) == (2)) | ((counter_addr_size) == (4))) in
let dst_addr : sbits(65) = { (if counter_addr_size_2_4?
then sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
else sail_sign_extend(rgfi(7), 65)) } in
{ if (not_bool(counter_addr_size_2_4?)) & (not_bool(canonical_address_p(signed(dst_addr))))
then throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
else ();
let select_byte_operand : bool = ((0xaa) == (opcode)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let rax_var : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), 0x0, rex_byte)) in
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let flg0 : option(string) = (wme_size(proc_mode, operand_size, truncate(dst_addr, 64), 0, unsigned(rax_var), inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :WME-SIZE-ERROR"))
else ();
let dst_addr : sbits(49) = (bits_of_int((match operand_size {
1 => { if (df) == (0b0)
then (1) + (signed(truncate(dst_addr, 48)))
else (-1) + (signed(truncate(dst_addr, 48))) },
2 => { if (df) == (0b0)
then (2) + (signed(truncate(dst_addr, 48)))
else (-2) + (signed(truncate(dst_addr, 48))) },
4 => { if (df) == (0b0)
then (4) + (signed(truncate(dst_addr, 48)))
else (-4) + (signed(truncate(dst_addr, 48))) },
_ => { if (df) == (0b0)
then (8) + (signed(truncate(dst_addr, 48)))
else (-8) + (signed(truncate(dst_addr, 48))) }
}) : int, 49)) in
{ (match group_1_prefix {
243 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b0))
then { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
}
else { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
} },
242 => { let counter : bits(64) = (rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_addr_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
if ((counter) == (0x0000_0000_0000_0000)) | ((truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1)) == (0b1))
then { { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
else { write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, unsigned(counter), rex_byte)
} },
_ => write_iptr(proc_mode, temp_rip)
}) : unit;
(match counter_addr_size {
2 => write_rgfi_size(0x2, 0x7, unsigned(truncate(dst_addr, 16)), rex_byte),
4 => write_rgfi_size(0x4, 0x7, unsigned(truncate(dst_addr, 32)), rex_byte),
_ => write_rgfi(7, sail_sign_extend(dst_addr, 64))
}) : unit
}
}
}
}

