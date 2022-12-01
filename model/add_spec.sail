$include "./rflags_spec.sail"

val gpr_add_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_add_spec_1 (dst, src, input_rflags) = {
    let raw_result : bits(9) = bits_of_int(unsigned(dst) + unsigned(src), 9);
    let signed_raw_result : sbits(9) = bits_of_int(signed(dst) + signed(src), 9);
    let result : bits(8) = truncate(raw_result, 8);
    let cf : bits(1) = cf_spec8(raw_result);
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = add_af_spec8(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_add_spec_2 (dst, src, input_rflags) = {
    let raw_result : bits(17) = bits_of_int(unsigned(dst) + unsigned(src), 17);
    let signed_raw_result : sbits(17) = bits_of_int(signed(dst) + signed(src), 17);
    let result : bits(16) = truncate(raw_result, 16);
    let cf : bits(1) = cf_spec16(raw_result);
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = add_af_spec16(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_add_spec_4 (dst, src, input_rflags) = {
    let raw_result : bits(33) = bits_of_int(unsigned(dst) + unsigned(src), 33);
    let signed_raw_result : sbits(33) = bits_of_int(signed(dst) + signed(src), 33);
    let result : bits(32) = truncate(raw_result, 32);
    let cf : bits(1) = cf_spec32(raw_result);
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = add_af_spec32(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_add_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_add_spec_8 (dst, src, input_rflags) = {
    let raw_result : bits(65) = bits_of_int(unsigned(dst) + unsigned(src), 65);
    let signed_raw_result : sbits(65) = bits_of_int(signed(dst) + signed(src), 65);
    let result : bits(64) = truncate(raw_result, 64);
    let cf : bits(1) = cf_spec64(raw_result);
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = add_af_spec64(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_adc_spec_1 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let raw_result : bits(9) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 9);
    let signed_raw_result : sbits(9) = bits_of_int(signed(dst) + (signed(src) + unsigned(input_cf)), 9);
    let result : bits(8) = truncate(raw_result, 8);
    let cf : bits(1) = cf_spec8(raw_result);
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = adc_af_spec8(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_adc_spec_2 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let raw_result : bits(17) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 17);
    let signed_raw_result : sbits(17) = bits_of_int(signed(dst) + (signed(src) + unsigned(input_cf)), 17);
    let result : bits(16) = truncate(raw_result, 16);
    let cf : bits(1) = cf_spec16(raw_result);
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = adc_af_spec16(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_adc_spec_4 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let raw_result : bits(33) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 33);
    let signed_raw_result : sbits(33) = bits_of_int(signed(dst) + (signed(src) + unsigned(input_cf)), 33);
    let result : bits(32) = truncate(raw_result, 32);
    let cf : bits(1) = cf_spec32(raw_result);
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = adc_af_spec32(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_adc_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_adc_spec_8 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let raw_result : bits(65) = bits_of_int(unsigned(dst) + (unsigned(src) + unsigned(input_cf)), 65);
    let signed_raw_result : sbits(65) = bits_of_int(signed(dst) + (signed(src) + unsigned(input_cf)), 65);
    let result : bits(64) = truncate(raw_result, 64);
    let cf : bits(1) = cf_spec64(raw_result);
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = adc_af_spec64(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}
