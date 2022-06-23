$include <prelude.sail>
$include <string.sail>

val x86_cbw_cwd_cdqe : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, wreg}
function x86_cbw_cwd_cdqe (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CBW/CWD/CDQE") in
let register_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let src_size : range(1, 4) = (check_range(1, 4, ash(register_size, -1))) in
let src : bits(32) = (truncate(rgfi_size(bits_of_int(src_size, 4), 0x0, rex_byte), 32)) in
let dst : bits(64) = (if logbitp(check_range(0, 32, (check_range(0, 32, ash(src_size, 3))) - (1)), src)
then sail_mask(64, trunc(register_size, (match src_size {
1 => sail_sign_extend(truncate(src, 8), 32),
2 => sail_sign_extend(truncate(src, 16), 32),
_ => src
}) : sbits(32)))
else sail_zero_extend(src, 64)) in
{ write_rgfi_size(bits_of_int(register_size, 4), 0x0, unsigned(dst), rex_byte);
write_iptr(proc_mode, temp_rip)
}

val x86_cwd_cdq_cqo : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, wreg}
function x86_cwd_cdq_cqo (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CWD/CDQ/CQO") in
let src_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let src : bits(64) = (rgfi_size(bits_of_int(src_size, 4), 0x0, rex_byte)) in
let rdx_var : bits(64) = (if logbitp((ash(src_size, 3)) - (1), src)
then sail_mask(64, trunc(src_size, 0xffff_ffff_ffff_ffff))
else 0x0000_0000_0000_0000) in
{ write_rgfi_size(bits_of_int(src_size, 4), 0x2, unsigned(rdx_var), rex_byte);
write_iptr(proc_mode, temp_rip)
}

