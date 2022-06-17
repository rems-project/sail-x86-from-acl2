$include <prelude.sail>
$include <string.sail>
$include "arith_and_logic.sail"
$include "bit.sail"
$include "conditional.sail"
$include "divide.sail"
$include "exchange.sail"
$include "jump_and_loop.sail"
$include "move.sail"
$include "multiply.sail"
$include "push_and_pop.sail"
$include "rotate_and_shift.sail"
$include "segmentationinst.sail"
$include "signextend.sail"
$include "string.sail"
$include "subroutine.sail"
$include "topfp.sail"
val x86_hlt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape}
function x86_hlt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-HLT") in
{ write_iptr(proc_mode, temp_rip);
throw(Emsg("Model state error: :LEGAL-HALT"))
}

val x86_cmc_clc_stc_cld_std : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape}
function x86_cmc_clc_stc_cld_std (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CMC/CLC/STC/CLD/STD") in
{ (match opcode {
245 => let cf : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend(rflagsbits_get_cf(rflags_var), 2), 1)) in
let not_cf : {|0, 1|} = (if (cf) == (0b1)
then 0
else 1) in
let rflags_var : bits(32) = (rflags) in
let new_rflags : bits(32) = (set_rflagsbits_get_cf(bits_of_int(not_cf, 1), rflags_var)) in
rflags = new_rflags,
248 => let rflags_var : bits(32) = (rflags) in
let new_rflags : bits(32) = (set_rflagsbits_get_cf(0b0, rflags_var)) in
rflags = new_rflags,
249 => let rflags_var : bits(32) = (rflags) in
let new_rflags : bits(32) = (set_rflagsbits_get_cf(0b1, rflags_var)) in
rflags = new_rflags,
252 => let rflags_var : bits(32) = (rflags) in
let new_rflags : bits(32) = (set_rflagsbits_get_df(0b0, rflags_var)) in
rflags = new_rflags,
_ => let rflags_var : bits(32) = (rflags) in
let new_rflags : bits(32) = (set_rflagsbits_get_df(0b1, rflags_var)) in
rflags = new_rflags
}) : unit;
write_iptr(proc_mode, temp_rip)
}

val x86_sahf : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg}
function x86_sahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SAHF") in
let ax : bits(16) = (rr16(0x0)) in
let ah : bits(8) = (bits_of_int(ash(unsigned(ax), -8), 8)) in
let rflags_var : bits(32) = (rflags) in
let cf : bits(1) = (rflagsbits_get_cf(sail_zero_extend(ah, 32))) in
let pf : bits(1) = (rflagsbits_get_pf(sail_zero_extend(ah, 32))) in
let af : bits(1) = (rflagsbits_get_af(sail_zero_extend(ah, 32))) in
let zf : bits(1) = (rflagsbits_get_zf(sail_zero_extend(ah, 32))) in
let sf : bits(1) = (rflagsbits_get_sf(sail_zero_extend(ah, 32))) in
let new_rflags : bits(32) = (set_rflagsbits_get_cf(cf, set_rflagsbits_get_pf(pf, set_rflagsbits_get_af(af, set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, rflags_var)))))) in
{ rflags = new_rflags;
write_iptr(proc_mode, temp_rip)
}

val x86_lahf : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, wreg}
function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LAHF") in
let rflags_var : bits(32) = (rflags) in
let cf : bits(1) = (rflagsbits_get_cf(rflags_var)) in
let pf : bits(1) = (rflagsbits_get_pf(rflags_var)) in
let af : bits(1) = (rflagsbits_get_af(rflags_var)) in
let zf : bits(1) = (rflagsbits_get_zf(rflags_var)) in
let sf : bits(1) = (rflagsbits_get_sf(rflags_var)) in
let new_ah : bits(8) = (truncate(set_rflagsbits_get_cf(cf, set_rflagsbits_get_res1(0b1, set_rflagsbits_get_pf(pf, set_rflagsbits_get_res2(0b0, set_rflagsbits_get_af(af, set_rflagsbits_get_res3(0b0, set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, 0x0000_0000)))))))), 8)) in
let ax : bits(16) = (rr16(0x0)) in
let new_ax : bits(16) = (bits_of_int(logior(unsigned(logand(0x00ff, ax)), ash(unsigned(new_ah), 8)), 16)) in
{ wr16(0x0, new_ax);
write_iptr(proc_mode, temp_rip)
}

