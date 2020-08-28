$include "./application_level_memory.sail"

val general_cf_spec_fn : (int, int) -> int effect {escape}

function general_cf_spec_fn (result_nbits, raw_result) = {
    bool_to_bit(not_bool(unsigned_byte_p(result_nbits, raw_result)))
}

val cf_spec8 : int -> int effect {escape}

function cf_spec8 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(8, raw_result)))
}

val cf_spec16 : int -> int effect {escape}

function cf_spec16 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(16, raw_result)))
}

val cf_spec32 : int -> int effect {escape}

function cf_spec32 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(32, raw_result)))
}

val cf_spec64 : int -> int effect {escape}

function cf_spec64 raw_result = {
    bool_to_bit(not_bool(unsigned_byte_p(64, raw_result)))
}

val general_of_spec_fn : (int, int) -> int effect {escape}

function general_of_spec_fn (result_nbits, signed_raw_result) = {
    bool_to_bit(not_bool(signed_byte_p(result_nbits, signed_raw_result)))
}

val of_spec8 : int -> int effect {escape}

function of_spec8 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(8, signed_raw_result)))
}

val of_spec16 : int -> int effect {escape}

function of_spec16 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(16, signed_raw_result)))
}

val of_spec32 : int -> int effect {escape}

function of_spec32 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(32, signed_raw_result)))
}

val of_spec64 : int -> int effect {escape}

function of_spec64 signed_raw_result = {
    bool_to_bit(not_bool(signed_byte_p(64, signed_raw_result)))
}

val zf_spec : int -> int

function zf_spec result = {
    if result == 0 then 1 else 0
}

val bitcount8 : int -> int

function bitcount8 x = {
    if x == 0 then 0 else {
        loghead(1, x) + bitcount8(logtail(1, x))
    }
}

val pf_spec8 : int -> int effect {escape}

function pf_spec8 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(result))))
}

val pf_spec16 : int -> int effect {escape}

function pf_spec16 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val pf_spec32 : int -> int effect {escape}

function pf_spec32 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val pf_spec64 : int -> int effect {escape}

function pf_spec64 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val general_pf_spec_fn : (int, int) -> int effect {escape}

function general_pf_spec_fn (result_nbits, result) = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val sf_spec8 : int -> int

function sf_spec8 result = {
    unsigned(get_slice_int(1, result, 7))
}

val sf_spec16 : int -> int

function sf_spec16 result = {
    unsigned(get_slice_int(1, result, 15))
}

val sf_spec32 : int -> int

function sf_spec32 result = {
    unsigned(get_slice_int(1, result, 31))
}

val sf_spec64 : int -> int

function sf_spec64 result = {
    unsigned(get_slice_int(1, result, 63))
}

val general_sf_spec_fn : (int, int) -> int

function general_sf_spec_fn (result_nbits, result) = {
    unsigned(get_slice_int(1, result, result_nbits - 1))
}

val add_af_spec8 : (int, int) -> int

function add_af_spec8 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let add = (dst_3_0 + src_3_0) : int;
    let af = bool_to_bit(15 < add) : int;
    af
}

val add_af_spec16 : (int, int) -> int

function add_af_spec16 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let add = (dst_3_0 + src_3_0) : int;
    let af = bool_to_bit(15 < add) : int;
    af
}

val add_af_spec32 : (int, int) -> int

function add_af_spec32 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let add = (dst_3_0 + src_3_0) : int;
    let af = bool_to_bit(15 < add) : int;
    af
}

val add_af_spec64 : (int, int) -> int

function add_af_spec64 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let add = (dst_3_0 + src_3_0) : int;
    let af = bool_to_bit(15 < add) : int;
    af
}

val sub_af_spec8 : (int, int) -> int

function sub_af_spec8 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sub = (dst_3_0 - src_3_0) : int;
    let af = bool_to_bit(sub < 0) : int;
    af
}

val sub_af_spec16 : (int, int) -> int

function sub_af_spec16 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sub = (dst_3_0 - src_3_0) : int;
    let af = bool_to_bit(sub < 0) : int;
    af
}

val sub_af_spec32 : (int, int) -> int

function sub_af_spec32 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sub = (dst_3_0 - src_3_0) : int;
    let af = bool_to_bit(sub < 0) : int;
    af
}

val sub_af_spec64 : (int, int) -> int

function sub_af_spec64 (dst, src) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sub = (dst_3_0 - src_3_0) : int;
    let af = bool_to_bit(sub < 0) : int;
    af
}

val adc_af_spec8 : (int, int, int) -> int

function adc_af_spec8 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let adc = (dst_3_0 + (src_3_0 + cf)) : int;
    let af = bool_to_bit(15 < adc) : int;
    af
}

val adc_af_spec16 : (int, int, int) -> int

function adc_af_spec16 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let adc = (dst_3_0 + (src_3_0 + cf)) : int;
    let af = bool_to_bit(15 < adc) : int;
    af
}

val adc_af_spec32 : (int, int, int) -> int

function adc_af_spec32 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let adc = (dst_3_0 + (src_3_0 + cf)) : int;
    let af = bool_to_bit(15 < adc) : int;
    af
}

val adc_af_spec64 : (int, int, int) -> int

function adc_af_spec64 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let adc = (dst_3_0 + (src_3_0 + cf)) : int;
    let af = bool_to_bit(15 < adc) : int;
    af
}

val sbb_af_spec8 : (int, int, int) -> int

function sbb_af_spec8 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sbb = (dst_3_0 - (src_3_0 + cf)) : int;
    let af = bool_to_bit(sbb < 0) : int;
    af
}

val sbb_af_spec16 : (int, int, int) -> int

function sbb_af_spec16 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sbb = (dst_3_0 - (src_3_0 + cf)) : int;
    let af = bool_to_bit(sbb < 0) : int;
    af
}

val sbb_af_spec32 : (int, int, int) -> int

function sbb_af_spec32 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sbb = (dst_3_0 - (src_3_0 + cf)) : int;
    let af = bool_to_bit(sbb < 0) : int;
    af
}

val sbb_af_spec64 : (int, int, int) -> int

function sbb_af_spec64 (dst, src, cf) = {
    let dst_3_0 = unsigned(get_slice_int(4, dst, 0)) : int;
    let src_3_0 = unsigned(get_slice_int(4, src, 0)) : int;
    let sbb = (dst_3_0 - (src_3_0 + cf)) : int;
    let af = bool_to_bit(sbb < 0) : int;
    af
}
