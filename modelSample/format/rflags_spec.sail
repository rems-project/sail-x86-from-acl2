$include "./application_level_memory.sail"

val general_cf_spec_fn : (nat, nat) -> bits(1)

function general_cf_spec_fn (result_nbits, raw_result) = {
    bool_to_bit(not_bool(unsigned_byte_p(result_nbits, raw_result)))
}

val cf_spec8 : bits(9) -> bits(1)

function cf_spec8 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(8, unsigned(raw_result))))
}

val cf_spec16 : bits(17) -> bits(1)

function cf_spec16 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(16, unsigned(raw_result))))
}

val cf_spec32 : bits(33) -> bits(1)

function cf_spec32 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(32, unsigned(raw_result))))
}

val cf_spec64 : bits(65) -> bits(1)

function cf_spec64 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(64, unsigned(raw_result))))
}

val general_of_spec_fn : (nat, int) -> bits(1)

function general_of_spec_fn (result_nbits, signed_raw_result) = {
    bool_to_bit(not_bool(signed_byte_p(result_nbits, signed_raw_result)))
}

val of_spec8 : sbits(9) -> bits(1)

function of_spec8 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(8, signed(signed_raw_result))))
}

val of_spec16 : sbits(17) -> bits(1)

function of_spec16 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(16, signed(signed_raw_result))))
}

val of_spec32 : sbits(33) -> bits(1)

function of_spec32 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(32, signed(signed_raw_result))))
}

val of_spec64 : sbits(65) -> bits(1)

function of_spec64 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(64, signed(signed_raw_result))))
}

val zf_spec : nat -> {|0, 1|}

function zf_spec result = {
    if get_slice_int(8, result, 0) == get_slice_int(8, 0, 0) then 1 else 0
}

val bitcount8 : bits(8) -> int

function bitcount8 x = {
    if x == 0 then 0 else {
        unsigned(the_bits(1, loghead(1, unsigned(x)))) + the_range(0, 8, bitcount8(the_bits(8, logtail(1, unsigned(x)))))
    }
}

val pf_spec8 : bits(8) -> bits(1)

function pf_spec8 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(unsigned(result)))))
}

val pf_spec16 : bits(16) -> bits(1)

function pf_spec16 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, unsigned(result))))))
}

val pf_spec32 : bits(32) -> bits(1)

function pf_spec32 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, unsigned(result))))))
}

val pf_spec64 : bits(64) -> bits(1)

function pf_spec64 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, unsigned(result))))))
}

val general_pf_spec_fn : ({|8, 16, 32, 64|}, nat) -> bits(1)

function general_pf_spec_fn (result_nbits, result) = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val sf_spec8 : bits(8) -> bits(1)

function sf_spec8 result = {
    slice(result, 7, 1)
}

val sf_spec16 : bits(16) -> bits(1)

function sf_spec16 result = {
    slice(result, 15, 1)
}

val sf_spec32 : bits(32) -> bits(1)

function sf_spec32 result = {
    slice(result, 31, 1)
}

val sf_spec64 : bits(64) -> bits(1)

function sf_spec64 result = {
    slice(result, 63, 1)
}

val general_sf_spec_fn : ({|8, 16, 32, 64|}, nat) -> bits(1)

function general_sf_spec_fn (result_nbits, result) = {
    get_slice_int(1, result, result_nbits - 1)
}

val add_af_spec8 : (bits(8), bits(8)) -> bits(1)

function add_af_spec8 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let add = the_bits(5, unsigned(dst_3_0) + unsigned(src_3_0)) : bits(5);
    let af = bool_to_bit(15 < unsigned(add)) : bits(1);
    af
}

val add_af_spec16 : (bits(16), bits(16)) -> bits(1)

function add_af_spec16 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let add = the_bits(5, unsigned(dst_3_0) + unsigned(src_3_0)) : bits(5);
    let af = bool_to_bit(15 < unsigned(add)) : bits(1);
    af
}

val add_af_spec32 : (bits(32), bits(32)) -> bits(1)

function add_af_spec32 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let add = the_bits(5, unsigned(dst_3_0) + unsigned(src_3_0)) : bits(5);
    let af = bool_to_bit(15 < unsigned(add)) : bits(1);
    af
}

val add_af_spec64 : (bits(64), bits(64)) -> bits(1)

function add_af_spec64 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let add = the_bits(5, unsigned(dst_3_0) + unsigned(src_3_0)) : bits(5);
    let af = bool_to_bit(15 < unsigned(add)) : bits(1);
    af
}

val sub_af_spec8 : (bits(8), bits(8)) -> bits(1)

function sub_af_spec8 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sub = the_sbits(5, unsigned(dst_3_0) - unsigned(src_3_0)) : sbits(5);
    let af = bool_to_bit(signed(sub) < 0) : bits(1);
    af
}

val sub_af_spec16 : (bits(16), bits(16)) -> bits(1)

function sub_af_spec16 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sub = the_sbits(5, unsigned(dst_3_0) - unsigned(src_3_0)) : sbits(5);
    let af = bool_to_bit(signed(sub) < 0) : bits(1);
    af
}

val sub_af_spec32 : (bits(32), bits(32)) -> bits(1)

function sub_af_spec32 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sub = the_sbits(5, unsigned(dst_3_0) - unsigned(src_3_0)) : sbits(5);
    let af = bool_to_bit(signed(sub) < 0) : bits(1);
    af
}

val sub_af_spec64 : (bits(64), bits(64)) -> bits(1)

function sub_af_spec64 (dst, src) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sub = the_sbits(5, unsigned(dst_3_0) - unsigned(src_3_0)) : sbits(5);
    let af = bool_to_bit(signed(sub) < 0) : bits(1);
    af
}

val adc_af_spec8 : (bits(8), bits(8), bits(1)) -> bits(1)

function adc_af_spec8 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let adc = the_bits(6, unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf))) : bits(6);
    let af = bool_to_bit(15 < unsigned(adc)) : bits(1);
    af
}

val adc_af_spec16 : (bits(16), bits(16), bits(1)) -> bits(1)

function adc_af_spec16 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let adc = the_bits(6, unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf))) : bits(6);
    let af = bool_to_bit(15 < unsigned(adc)) : bits(1);
    af
}

val adc_af_spec32 : (bits(32), bits(32), bits(1)) -> bits(1)

function adc_af_spec32 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let adc = the_bits(6, unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf))) : bits(6);
    let af = bool_to_bit(15 < unsigned(adc)) : bits(1);
    af
}

val adc_af_spec64 : (bits(64), bits(64), bits(1)) -> bits(1)

function adc_af_spec64 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let adc = the_bits(6, unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf))) : bits(6);
    let af = bool_to_bit(15 < unsigned(adc)) : bits(1);
    af
}

val sbb_af_spec8 : (bits(8), bits(8), bits(1)) -> bits(1)

function sbb_af_spec8 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sbb = (unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(the_bits(4, cf)))) : int;
    let af = bool_to_bit(sbb < 0) : bits(1);
    af
}

val sbb_af_spec16 : (bits(16), bits(16), bits(1)) -> bits(1)

function sbb_af_spec16 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sbb = (unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(the_bits(4, cf)))) : int;
    let af = bool_to_bit(sbb < 0) : bits(1);
    af
}

val sbb_af_spec32 : (bits(32), bits(32), bits(1)) -> bits(1)

function sbb_af_spec32 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sbb = (unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(the_bits(4, cf)))) : int;
    let af = bool_to_bit(sbb < 0) : bits(1);
    af
}

val sbb_af_spec64 : (bits(64), bits(64), bits(1)) -> bits(1)

function sbb_af_spec64 (dst, src, cf) = {
    let dst_3_0 = slice(dst, 0, 4) : bits(4);
    let src_3_0 = slice(src, 0, 4) : bits(4);
    let sbb = (unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(the_bits(4, cf)))) : int;
    let af = bool_to_bit(sbb < 0) : bits(1);
    af
}
