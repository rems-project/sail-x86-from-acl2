$include "./rflags_spec.sail"

val gpr_add_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_add_spec_1 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(8) = 0x08;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : bits(9) = bits_of_int(unsigned(dst) + unsigned(src), 9);
    let signed_raw_result : sbits(9) = bits_of_int(signed(n08_to_i08(dst)) + signed(n08_to_i08(src)), 9);
    let result : bits(8) = 0x08;
    let cf : bits(1) = cf_spec8(raw_result);
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = add_af_spec8(dst, src);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_add_spec_2 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(16) = 0x0010;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : bits(17) = bits_of_int(unsigned(dst) + unsigned(src), 17);
    let signed_raw_result : sbits(17) = bits_of_int(signed(n16_to_i16(dst)) + signed(n16_to_i16(src)), 17);
    let result : bits(16) = 0x0010;
    let cf : bits(1) = cf_spec16(raw_result);
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = add_af_spec16(dst, src);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_add_spec_4 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(32) = 0x00000020;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : bits(33) = bits_of_int(unsigned(dst) + unsigned(src), 33);
    let signed_raw_result : sbits(33) = bits_of_int(signed(n32_to_i32(dst)) + signed(n32_to_i32(src)), 33);
    let result : bits(32) = 0x00000020;
    let cf : bits(1) = cf_spec32(raw_result);
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = add_af_spec32(dst, src);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_add_spec_8 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(64) = 0x0000000000000040;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : bits(65) = bits_of_int(unsigned(dst) + unsigned(src), 65);
    let signed_raw_result : sbits(65) = bits_of_int(signed(n64_to_i64(dst)) + signed(n64_to_i64(src)), 65);
    let result : bits(64) = 0x0000000000000040;
    let cf : bits(1) = cf_spec64(raw_result);
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = add_af_spec64(dst, src);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32))

function gpr_adc_spec_1 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(8) = 0x08;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let raw_result : bits(9) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 9);
    let signed_raw_result : sbits(9) = bits_of_int(signed(n08_to_i08(dst)) + (signed(n08_to_i08(src)) + unsigned(input_cf)), 9);
    let result : bits(8) = 0x08;
    let cf : bits(1) = cf_spec8(raw_result);
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = adc_af_spec8(dst, src, input_cf);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32))

function gpr_adc_spec_2 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(16) = 0x0010;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let raw_result : bits(17) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 17);
    let signed_raw_result : sbits(17) = bits_of_int(signed(n16_to_i16(dst)) + (signed(n16_to_i16(src)) + unsigned(input_cf)), 17);
    let result : bits(16) = 0x0010;
    let cf : bits(1) = cf_spec16(raw_result);
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = adc_af_spec16(dst, src, input_cf);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32))

function gpr_adc_spec_4 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(32) = 0x00000020;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let raw_result : bits(33) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 33);
    let signed_raw_result : sbits(33) = bits_of_int(signed(n32_to_i32(dst)) + (signed(n32_to_i32(src)) + unsigned(input_cf)), 33);
    let result : bits(32) = 0x00000020;
    let cf : bits(1) = cf_spec32(raw_result);
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = adc_af_spec32(dst, src, input_cf);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32))

function gpr_adc_spec_8 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(64) = 0x0000000000000040;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let raw_result : bits(65) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 65);
    let signed_raw_result : sbits(65) = bits_of_int(signed(n64_to_i64(dst)) + (signed(n64_to_i64(src)) + unsigned(input_cf)), 65);
    let result : bits(64) = 0x0000000000000040;
    let cf : bits(1) = cf_spec64(raw_result);
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = adc_af_spec64(dst, src, input_cf);
    let zf : bits(1) = bits_of_int(zf_spec(unsigned(result)), 1);
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(signed_raw_result);
    let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = 0x00000000;
    (result, output_rflags, undefined_flags)
}
