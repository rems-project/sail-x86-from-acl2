$include "./arith_and_logic.sail"
$include "./bit.sail"
$include "./conditional.sail"
$include "./divide.sail"
$include "./exchange.sail"
$include "./jump_and_loop.sail"
$include "./move.sail"
$include "./multiply.sail"
$include "./push_and_pop.sail"
$include "./rotate_and_shift.sail"
$include "./segmentationinst.sail"
$include "./signextend.sail"
$include "./string.sail"
$include "./syscall.sail"
$include "./subroutine.sail"
$include "./topfp.sail"

val x86_hlt : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_hlt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-HLT";
    write_iptr(proc_mode, temp_rip);
    throw(Emsg("Model state error: :LEGAL-HALT"))
}

val x86_cmc_clc_stc_cld_std : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cmc_clc_stc_cld_std (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CMC/CLC/STC/CLD/STD";
    (match opcode {
      245 => {
          let cf : bits(1) = truncate(let rflags_var : bits(32) = rflags in
            sail_zero_extend(Mk_rflagsbits(rflags_var)[cf], 2), 1);
          let not_cf : {|0, 1|} = {
              if cf == 0b1 then 0 else 1
          };
          let rflags_var : bits(32) = rflags;
          let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with cf = bits_of_int(not_cf, 1)];
          rflags = new_rflags.bits
      },
      248 => {
          let rflags_var : bits(32) = rflags;
          let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with cf = 0b0];
          rflags = new_rflags.bits
      },
      249 => {
          let rflags_var : bits(32) = rflags;
          let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with cf = 0b1];
          rflags = new_rflags.bits
      },
      252 => {
          let rflags_var : bits(32) = rflags;
          let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with df = 0b0];
          rflags = new_rflags.bits
      },
      _ => {
          let rflags_var : bits(32) = rflags;
          let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with df = 0b1];
          rflags = new_rflags.bits
      }
    }) : unit;
    write_iptr(proc_mode, temp_rip)
}

val x86_sahf : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_sahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SAHF";
    let ax : bits(16) = rr16(0x0);
    let ah : bits(8) = bits_of_int(ash(unsigned(ax), -8), 8);
    let rflags_var : bits(32) = rflags;
    let cf : bits(1) = Mk_rflagsbits(sail_zero_extend(ah, 32))[cf];
    let pf : bits(1) = Mk_rflagsbits(sail_zero_extend(ah, 32))[pf];
    let af : bits(1) = Mk_rflagsbits(sail_zero_extend(ah, 32))[af];
    let zf : bits(1) = Mk_rflagsbits(sail_zero_extend(ah, 32))[zf];
    let sf : bits(1) = Mk_rflagsbits(sail_zero_extend(ah, 32))[sf];
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with sf, zf, af, pf, cf];
    rflags = new_rflags.bits;
    write_iptr(proc_mode, temp_rip)
}

val x86_lahf : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LAHF";
    let rflags_var : bits(32) = rflags;
    let cf : bits(1) = Mk_rflagsbits(rflags_var)[cf];
    let pf : bits(1) = Mk_rflagsbits(rflags_var)[pf];
    let af : bits(1) = Mk_rflagsbits(rflags_var)[af];
    let zf : bits(1) = Mk_rflagsbits(rflags_var)[zf];
    let sf : bits(1) = Mk_rflagsbits(rflags_var)[sf];
    let new_ah : rflagsbits = [Mk_rflagsbits(0x00000000) with sf, zf, res3 = 0b0, af, res2 = 0b0, pf, res1 = 0b1, cf];
    let ax : bits(16) = rr16(0x0);
    let new_ax : bits(16) = bits_of_int(logior(unsigned(logand(0x00ff, ax)), ash(unsigned(new_ah.bits), 8)), 16);
    wr16(0x0, new_ax);
    write_iptr(proc_mode, temp_rip)
}
