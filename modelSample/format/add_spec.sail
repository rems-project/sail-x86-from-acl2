$include "./rflags_spec.sail"

val gpr_add_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_add_spec_1 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(8, src) : bits(8);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = the_bits(9, unsigned(dst) + unsigned(src)) : bits(9);
    let signed_raw_result = the_sbits(9, signed(n08_to_i08(dst)) + signed(n08_to_i08(src))) : sbits(9);
    let result = n_size(8, raw_result) : bits(8);
    let cf = cf_spec8(raw_result) : bits(1);
    let pf = pf_spec8(result) : bits(1);
    let af = add_af_spec8(dst, src) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec8(result) : bits(1);
    let of = of_spec8(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_add_spec_2 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(16, src) : bits(16);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = the_bits(17, unsigned(dst) + unsigned(src)) : bits(17);
    let signed_raw_result = the_sbits(17, signed(n16_to_i16(dst)) + signed(n16_to_i16(src))) : sbits(17);
    let result = n_size(16, raw_result) : bits(16);
    let cf = cf_spec16(raw_result) : bits(1);
    let pf = pf_spec16(result) : bits(1);
    let af = add_af_spec16(dst, src) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec16(result) : bits(1);
    let of = of_spec16(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_add_spec_4 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(32, src) : bits(32);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = the_bits(33, unsigned(dst) + unsigned(src)) : bits(33);
    let signed_raw_result = the_sbits(33, signed(n32_to_i32(dst)) + signed(n32_to_i32(src))) : sbits(33);
    let result = n_size(32, raw_result) : bits(32);
    let cf = cf_spec32(raw_result) : bits(1);
    let pf = pf_spec32(result) : bits(1);
    let af = add_af_spec32(dst, src) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec32(result) : bits(1);
    let of = of_spec32(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_add_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(64, src) : bits(64);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = the_bits(65, unsigned(dst) + unsigned(src)) : bits(65);
    let signed_raw_result = the_sbits(65, signed(n64_to_i64(dst)) + signed(n64_to_i64(src))) : sbits(65);
    let result = n_size(64, raw_result) : bits(64);
    let cf = cf_spec64(raw_result) : bits(1);
    let pf = pf_spec64(result) : bits(1);
    let af = add_af_spec64(dst, src) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec64(result) : bits(1);
    let of = of_spec64(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_adc_spec_1 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(8, src) : bits(8);
    let input_rflags = n32(input_rflags) : bits(32);
    let input_cf = rflagsbits_get_cf(input_rflags) : bits(1);
    let raw_result = the_bits(9, unsigned(dst) + (unsigned(src) + unsigned(input_cf))) : bits(9);
    let signed_raw_result = the_sbits(9, signed(n08_to_i08(dst)) + (signed(n08_to_i08(src)) + unsigned(input_cf))) : sbits(9);
    let result = n_size(8, raw_result) : bits(8);
    let cf = cf_spec8(raw_result) : bits(1);
    let pf = pf_spec8(result) : bits(1);
    let af = adc_af_spec8(dst, src, input_cf) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec8(result) : bits(1);
    let of = of_spec8(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_adc_spec_2 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(16, src) : bits(16);
    let input_rflags = n32(input_rflags) : bits(32);
    let input_cf = rflagsbits_get_cf(input_rflags) : bits(1);
    let raw_result = the_bits(17, unsigned(dst) + (unsigned(src) + unsigned(input_cf))) : bits(17);
    let signed_raw_result = the_sbits(17, signed(n16_to_i16(dst)) + (signed(n16_to_i16(src)) + unsigned(input_cf))) : sbits(17);
    let result = n_size(16, raw_result) : bits(16);
    let cf = cf_spec16(raw_result) : bits(1);
    let pf = pf_spec16(result) : bits(1);
    let af = adc_af_spec16(dst, src, input_cf) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec16(result) : bits(1);
    let of = of_spec16(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_adc_spec_4 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(32, src) : bits(32);
    let input_rflags = n32(input_rflags) : bits(32);
    let input_cf = rflagsbits_get_cf(input_rflags) : bits(1);
    let raw_result = the_bits(33, unsigned(dst) + (unsigned(src) + unsigned(input_cf))) : bits(33);
    let signed_raw_result = the_sbits(33, signed(n32_to_i32(dst)) + (signed(n32_to_i32(src)) + unsigned(input_cf))) : sbits(33);
    let result = n_size(32, raw_result) : bits(32);
    let cf = cf_spec32(raw_result) : bits(1);
    let pf = pf_spec32(result) : bits(1);
    let af = adc_af_spec32(dst, src, input_cf) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec32(result) : bits(1);
    let of = of_spec32(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_adc_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(64, src) : bits(64);
    let input_rflags = n32(input_rflags) : bits(32);
    let input_cf = rflagsbits_get_cf(input_rflags) : bits(1);
    let raw_result = the_bits(65, unsigned(dst) + (unsigned(src) + unsigned(input_cf))) : bits(65);
    let signed_raw_result = the_sbits(65, signed(n64_to_i64(dst)) + (signed(n64_to_i64(src)) + unsigned(input_cf))) : sbits(65);
    let result = n_size(64, raw_result) : bits(64);
    let cf = cf_spec64(raw_result) : bits(1);
    let pf = pf_spec64(result) : bits(1);
    let af = adc_af_spec64(dst, src, input_cf) : bits(1);
    let zf = the_bits(1, zf_spec(unsigned(result))) : bits(1);
    let sf = sf_spec64(result) : bits(1);
    let of = of_spec64(signed_raw_result) : bits(1);
    let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = get_slice_int(32, 0, 0) : bits(32);
    (result, output_rflags, undefined_flags)
}
