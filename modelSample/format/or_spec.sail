
val gpr_or_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_or_spec_1 (dst, src, input_rflags) = {
    let result : bits(8) = slice(logior(dst, src), 0, 8);
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec8(result);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec8(result);
    let of : {|0|} = 0;
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_or_spec_2 (dst, src, input_rflags) = {
    let result : bits(16) = slice(logior(dst, src), 0, 16);
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec16(result);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec16(result);
    let of : {|0|} = 0;
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_or_spec_4 (dst, src, input_rflags) = {
    let result : bits(32) = slice(logior(dst, src), 0, 32);
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec32(result);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec32(result);
    let of : {|0|} = 0;
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_or_spec_8 (dst, src, input_rflags) = {
    let result : bits(64) = slice(logior(dst, src), 0, 64);
    let cf : {|0|} = 0;
    let pf : bits(1) = pf_spec64(result);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec64(result);
    let of : {|0|} = 0;
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000);
    (result, output_rflags, undefined_flags)
}
