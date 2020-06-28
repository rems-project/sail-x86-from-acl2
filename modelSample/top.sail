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
val x86_hlt : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, escape}
function x86_hlt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-HLT") : string in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
throw(Emsg("Model state error: :LEGAL-HALT"))

val x86_cmc_clc_stc_cld_std : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, escape}
function x86_cmc_clc_stc_cld_std (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-CMC/CLC/STC/CLD/STD") : string in
let x86 = ((match opcode {
245 => let cf = (let rflags_var = (r_rflags(0)) : int in
rflagsbits_get_cf(rflags_var)) : int in
let not_cf = (if (cf) == (1)
then 0
else 1) : int in
let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(not_cf, rflags_var)) : int in
write_rflags(new_rflags, 0),
248 => let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(0, rflags_var)) : int in
write_rflags(new_rflags, 0),
249 => let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(1, rflags_var)) : int in
write_rflags(new_rflags, 0),
252 => let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_df(0, rflags_var)) : int in
write_rflags(new_rflags, 0),
_ => let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_df(1, rflags_var)) : int in
write_rflags(new_rflags, 0)
}) : int) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_sahf : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, escape}
function x86_sahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-SAHF") : string in
let ax = (rr16(0, 0)) : int in
let ah = (ash(ax, -8)) : int in
let rflags_var = (r_rflags(0)) : int in
let cf = (rflagsbits_get_cf(ah)) : int in
let pf = (rflagsbits_get_pf(ah)) : int in
let af = (rflagsbits_get_af(ah)) : int in
let zf = (rflagsbits_get_zf(ah)) : int in
let sf = (rflagsbits_get_sf(ah)) : int in
let new_rflags = (set_rflagsbits_get_cf(cf, set_rflagsbits_get_pf(pf, set_rflagsbits_get_af(af, set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, rflags_var)))))) : int in
let x86 = (write_rflags(new_rflags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_lahf : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, escape}
function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-LAHF") : string in
let rflags_var = (r_rflags(0)) : int in
let cf = (rflagsbits_get_cf(rflags_var)) : int in
let pf = (rflagsbits_get_pf(rflags_var)) : int in
let af = (rflagsbits_get_af(rflags_var)) : int in
let zf = (rflagsbits_get_zf(rflags_var)) : int in
let sf = (rflagsbits_get_sf(rflags_var)) : int in
let new_ah = (set_rflagsbits_get_cf(cf, set_rflagsbits_get_res1(1, set_rflagsbits_get_pf(pf, set_rflagsbits_get_res2(0, set_rflagsbits_get_af(af, set_rflagsbits_get_res3(0, set_rflagsbits_get_zf(zf, set_rflagsbits_get_sf(sf, 0))))))))) : int in
let ax = (rr16(0, 0)) : int in
let new_ax = (binary_logior(binary_logand(255, ax), ash(new_ah, 8))) : int in
let x86 = (wr16(0, new_ax, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

