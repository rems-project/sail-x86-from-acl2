
val gpr_or_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_or_spec_1 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(8, src) : bits(8);
    let input_rflags = n32(input_rflags) : bits(32);
    let result = slice(logior(dst, src), 0, 8) : bits(8);
    let cf = 0 : {|0|};
    let pf = pf_spec8(result) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec8(result) : bits(1);
    let of = 0 : {|0|};
    let output_rflags = change_rflagsbits(input_rflags, Some(get_slice_int(1, cf, 0)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_or_spec_2 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(16, src) : bits(16);
    let input_rflags = n32(input_rflags) : bits(32);
    let result = slice(logior(dst, src), 0, 16) : bits(16);
    let cf = 0 : {|0|};
    let pf = pf_spec16(result) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec16(result) : bits(1);
    let of = 0 : {|0|};
    let output_rflags = change_rflagsbits(input_rflags, Some(get_slice_int(1, cf, 0)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_or_spec_4 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(32, src) : bits(32);
    let input_rflags = n32(input_rflags) : bits(32);
    let result = slice(logior(dst, src), 0, 32) : bits(32);
    let cf = 0 : {|0|};
    let pf = pf_spec32(result) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec32(result) : bits(1);
    let of = 0 : {|0|};
    let output_rflags = change_rflagsbits(input_rflags, Some(get_slice_int(1, cf, 0)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_or_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_or_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(64, src) : bits(64);
    let input_rflags = n32(input_rflags) : bits(32);
    let result = slice(logior(dst, src), 0, 64) : bits(64);
    let cf = 0 : {|0|};
    let pf = pf_spec64(result) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec64(result) : bits(1);
    let of = 0 : {|0|};
    let output_rflags = change_rflagsbits(input_rflags, Some(get_slice_int(1, cf, 0)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32);
    (result, output_rflags, undefined_flags)
}
