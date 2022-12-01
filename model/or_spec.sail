
val gpr_or_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_or_spec_1 (dst, src, input_rflags) = {
    let result : bits(8) = logior(dst, src)[7 .. 0];
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec8(result);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec8(result);
    let of : {|0|} = 0;
    let output_rflags : rflagsbits = [input_rflags with cf = bits_of_int(cf, 1), pf, zf, sf, of = bits_of_int(of, 1)];
    let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_or_spec_2 (dst, src, input_rflags) = {
    let result : bits(16) = logior(dst, src)[15 .. 0];
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec16(result);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec16(result);
    let of : {|0|} = 0;
    let output_rflags : rflagsbits = [input_rflags with cf = bits_of_int(cf, 1), pf, zf, sf, of = bits_of_int(of, 1)];
    let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_or_spec_4 (dst, src, input_rflags) = {
    let result : bits(32) = logior(dst, src)[31 .. 0];
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec32(result);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec32(result);
    let of : {|0|} = 0;
    let output_rflags : rflagsbits = [input_rflags with cf = bits_of_int(cf, 1), pf, zf, sf, of = bits_of_int(of, 1)];
    let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_or_spec_8 (dst, src, input_rflags) = {
    let result : bits(64) = logior(dst, src)[63 .. 0];
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec64(result);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec64(result);
    let of : {|0|} = 0;
    let output_rflags : rflagsbits = [input_rflags with cf = bits_of_int(cf, 1), pf, zf, sf, of = bits_of_int(of, 1)];
    let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
    (result, output_rflags, undefined_flags)
}
