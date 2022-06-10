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
let ctx = ("X86-HLT") : string in
{ write_iptr(proc_mode, temp_rip);
throw(Emsg("Model state error: :LEGAL-HALT"))
}

val x86_cmc_clc_stc_cld_std : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape}
function x86_cmc_clc_stc_cld_std (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-CMC/CLC/STC/CLD/STD") : string in
{ (match opcode {
245 => let cf = (the_bits(1, let rflags_var = (rflags) : bits(32) in
the_bits(2, rflagsbits_get_cf(rflags_var)))) : bits(1) in
let not_cf = (if (the_bits(8, cf)) == (get_slice_int(8, 1, 0))
then 0
else 1) : {|0, 1|} in
let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_cf(get_slice_int(1, not_cf, 0), rflags_var)) : bits(32) in
rflags = new_rflags,
248 => let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_cf(get_slice_int(1, 0, 0), rflags_var)) : bits(32) in
rflags = new_rflags,
249 => let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_cf(get_slice_int(1, 1, 0), rflags_var)) : bits(32) in
rflags = new_rflags,
252 => let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_df(get_slice_int(1, 0, 0), rflags_var)) : bits(32) in
rflags = new_rflags,
_ => let rflags_var = (rflags) : bits(32) in
let new_rflags = (set_rflagsbits_get_df(get_slice_int(1, 1, 0), rflags_var)) : bits(32) in
rflags = new_rflags
}) : unit;
write_iptr(proc_mode, temp_rip)
}

val x86_sahf : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg}
function x86_sahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-SAHF") : string in
let ax = (rr16(get_slice_int(4, 0, 0))) : bits(16) in
let ah = (the_bits(8, ash(unsigned(ax), -8))) : bits(8) in
let rflags_var = (rflags) : bits(32) in
let cf = (rflagsbits_get_cf(the_bits(32, ah))) : bits(1) in
let pf = (rflagsbits_get_pf(the_bits(32, ah))) : bits(1) in
let af = (rflagsbits_get_af(the_bits(32, ah))) : bits(1) in
let zf = (rflagsbits_get_zf(the_bits(32, ah))) : bits(1) in
let sf = (rflagsbits_get_sf(the_bits(32, ah))) : bits(1) in
let new_rflags = (set_rflagsbits_get_cf(cf, set_rflagsbits_get_pf(pf, set_rflagsbits_get_af(af, set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, rflags_var)))))) : bits(32) in
{ rflags = new_rflags;
write_iptr(proc_mode, temp_rip)
}

val x86_lahf : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, wreg}
function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LAHF") : string in
let rflags_var = (rflags) : bits(32) in
let cf = (rflagsbits_get_cf(rflags_var)) : bits(1) in
let pf = (rflagsbits_get_pf(rflags_var)) : bits(1) in
let af = (rflagsbits_get_af(rflags_var)) : bits(1) in
let zf = (rflagsbits_get_zf(rflags_var)) : bits(1) in
let sf = (rflagsbits_get_sf(rflags_var)) : bits(1) in
let new_ah = (the_bits(8, set_rflagsbits_get_cf(cf, set_rflagsbits_get_res1(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(pf, set_rflagsbits_get_res2(get_slice_int(1, 0, 0), set_rflagsbits_get_af(af, set_rflagsbits_get_res3(get_slice_int(1, 0, 0), set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, get_slice_int(32, 0, 0))))))))))) : bits(8) in
let ax = (rr16(get_slice_int(4, 0, 0))) : bits(16) in
let new_ax = (the_bits(16, logior(unsigned(logand(get_slice_int(16, 255, 0), ax)), ash(unsigned(new_ah), 8)))) : bits(16) in
{ wr16(get_slice_int(4, 0, 0), new_ax);
write_iptr(proc_mode, temp_rip)
}

