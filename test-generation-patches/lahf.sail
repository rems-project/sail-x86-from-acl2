// Use bitwise operators rather than integer ones

val x86_lahf : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lahf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LAHF";
    let rflags_var : rflagsbits = rflags;
    let cf : bits(1) = rflags_var[cf];
    let pf : bits(1) = rflags_var[pf];
    let af : bits(1) = rflags_var[af];
    let zf : bits(1) = rflags_var[zf];
    let sf : bits(1) = rflags_var[sf];
    let new_ah : rflagsbits = [Mk_rflagsbits(0x00000000) with sf, zf, res3 = 0b0, af, res2 = 0b0, pf, res1 = 0b1, cf];
    let ax : bits(16) = rr16(0x0);
    let new_ax : bits(16) = logior(logand(0x00ff, ax), bits_of_int(ash(unsigned(new_ah.bits), 8),16));
    wr16(0x0, new_ax);
    write_iptr(proc_mode, temp_rip)
}
