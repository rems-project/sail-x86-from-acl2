
val gpr_sub_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_sub_spec_1 (dst, src, input_rflags) = {
    let signed_raw_result : sbits(9) = bits_of_int(signed(dst) - signed(src), 9);
    let result : bits(8) = truncate(signed_raw_result, 8);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(src));
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = sub_af_spec8(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sub_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_sub_spec_2 (dst, src, input_rflags) = {
    let signed_raw_result : sbits(17) = bits_of_int(signed(dst) - signed(src), 17);
    let result : bits(16) = truncate(signed_raw_result, 16);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(src));
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = sub_af_spec16(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sub_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_sub_spec_4 (dst, src, input_rflags) = {
    let signed_raw_result : sbits(33) = bits_of_int(signed(dst) - signed(src), 33);
    let result : bits(32) = truncate(signed_raw_result, 32);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(src));
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = sub_af_spec32(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sub_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_sub_spec_8 (dst, src, input_rflags) = {
    let signed_raw_result : sbits(65) = bits_of_int(signed(dst) - signed(src), 65);
    let result : bits(64) = truncate(signed_raw_result, 64);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(src));
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = sub_af_spec64(dst, src);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(signed_raw_result);
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sbb_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_sbb_spec_1 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let signed_raw_result : sbits(10) = bits_of_int(signed(dst) - signed(bits_of_int(signed(src) + unsigned(input_cf), 9)), 10);
    let result : bits(8) = truncate(signed_raw_result, 8);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(bits_of_int(unsigned(input_cf) + unsigned(src), 9)));
    let pf : bits(1) = pf_spec8(result);
    let af : bits(1) = sbb_af_spec8(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec8(result);
    let of : bits(1) = of_spec8(truncate(signed_raw_result, 9));
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sbb_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_sbb_spec_2 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let signed_raw_result : sbits(18) = bits_of_int(signed(dst) - signed(bits_of_int(signed(src) + unsigned(input_cf), 17)), 18);
    let result : bits(16) = truncate(signed_raw_result, 16);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(bits_of_int(unsigned(input_cf) + unsigned(src), 17)));
    let pf : bits(1) = pf_spec16(result);
    let af : bits(1) = sbb_af_spec16(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec16(result);
    let of : bits(1) = of_spec16(truncate(signed_raw_result, 17));
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sbb_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_sbb_spec_4 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let signed_raw_result : sbits(34) = bits_of_int(signed(dst) - signed(bits_of_int(signed(src) + unsigned(input_cf), 33)), 34);
    let result : bits(32) = truncate(signed_raw_result, 32);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(bits_of_int(unsigned(input_cf) + unsigned(src), 33)));
    let pf : bits(1) = pf_spec32(result);
    let af : bits(1) = sbb_af_spec32(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec32(result);
    let of : bits(1) = of_spec32(truncate(signed_raw_result, 33));
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}

val gpr_sbb_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_sbb_spec_8 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let signed_raw_result : sbits(66) = bits_of_int(signed(dst) - signed(bits_of_int(signed(src) + unsigned(input_cf), 65)), 66);
    let result : bits(64) = truncate(signed_raw_result, 64);
    let cf : bits(1) = bool_to_bit(unsigned(dst) < unsigned(bits_of_int(unsigned(input_cf) + unsigned(src), 65)));
    let pf : bits(1) = pf_spec64(result);
    let af : bits(1) = sbb_af_spec64(dst, src, input_cf);
    let zf : bits(1) = zf_spec(unsigned(result));
    let sf : bits(1) = sf_spec64(result);
    let of : bits(1) = of_spec64(truncate(signed_raw_result, 65));
    let output_rflags : rflagsbits = [input_rflags with cf, pf, af, zf, sf, of];
    let undefined_flags : rflagsbits = Mk_rflagsbits(0x00000000);
    (result, output_rflags, undefined_flags)
}
