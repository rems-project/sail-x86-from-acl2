$ifndef __X86_TOP
$define __X86_TOP
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
$include "syscall.sail"
$include "subroutine.sail"
$include "topfp.sail"
val x86_hlt : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape}
function x86_hlt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-HLT") in
{ write_iptr(proc_mode, temp_rip);
throw(Emsg("Model state error: :LEGAL-HALT"))
}

val x86_cmc_clc_stc_cld_std : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape}
function x86_cmc_clc_stc_cld_std (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-CMC/CLC/STC/CLD/STD") in
{ (match opcode {
245 => { let cf : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend((Mk_rflagsbits(rflags_var))[cf], 2), 1)) in
let not_cf : {|0, 1|} = { (if (cf) == (0b1)
then 0
else 1) } in
let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (bits_of_int(not_cf, 1))]) in
rflags = (new_rflags).bits },
248 => { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (0b0)]) in
rflags = (new_rflags).bits },
249 => { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (0b1)]) in
rflags = (new_rflags).bits },
252 => { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with df = (0b0)]) in
rflags = (new_rflags).bits },
_ => { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with df = (0b1)]) in
rflags = (new_rflags).bits }
}) : unit;
write_iptr(proc_mode, temp_rip)
}

val x86_sahf : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape, rreg}
function x86_sahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SAHF") in
let ax : bits(16) = (rr16(0x0)) in
let ah : bits(8) = (bits_of_int(ash(unsigned(ax), -8), 8)) in
let rflags_var : bits(32) = (rflags) in
let cf : bits(1) = ((Mk_rflagsbits(sail_zero_extend(ah, 32)))[cf]) in
let pf : bits(1) = ((Mk_rflagsbits(sail_zero_extend(ah, 32)))[pf]) in
let af : bits(1) = ((Mk_rflagsbits(sail_zero_extend(ah, 32)))[af]) in
let zf : bits(1) = ((Mk_rflagsbits(sail_zero_extend(ah, 32)))[zf]) in
let sf : bits(1) = ((Mk_rflagsbits(sail_zero_extend(ah, 32)))[sf]) in
let new_rflags : rflagsbits = ([([([([([(Mk_rflagsbits(rflags_var)) with sf = (sf)]) with zf = (zf)]) with af = (af)]) with pf = (pf)]) with cf = (cf)]) in
{ rflags = (new_rflags).bits;
write_iptr(proc_mode, temp_rip)
}

val x86_lahf : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape, rreg, wreg}
function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LAHF") in
let rflags_var : bits(32) = (rflags) in
let cf : bits(1) = ((Mk_rflagsbits(rflags_var))[cf]) in
let pf : bits(1) = ((Mk_rflagsbits(rflags_var))[pf]) in
let af : bits(1) = ((Mk_rflagsbits(rflags_var))[af]) in
let zf : bits(1) = ((Mk_rflagsbits(rflags_var))[zf]) in
let sf : bits(1) = ((Mk_rflagsbits(rflags_var))[sf]) in
let new_ah : rflagsbits = ([([([([([([([([(Mk_rflagsbits(0x0000_0000)) with sf = (sf)]) with zf = (zf)]) with res3 = (0b0)]) with af = (af)]) with res2 = (0b0)]) with pf = (pf)]) with res1 = (0b1)]) with cf = (cf)]) in
let ax : bits(16) = (rr16(0x0)) in
let new_ax : bits(16) = (bits_of_int(logior(unsigned(logand(0x00ff, ax)), ash(unsigned((new_ah).bits), 8)), 16)) in
{ wr16(0x0, new_ax);
write_iptr(proc_mode, temp_rip)
}

$endif
