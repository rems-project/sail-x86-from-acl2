$include "./application_level_memory.sail"

/*! <p>General @ ( ' CF ' ) Specification ( Source: Intel Manuals, 
 Vol. 1, Section 3.4.3.1 ) :</p> 
 
 <p><b>Carry flag</b> Set if an arithmetic operation generates a 
 carry or a borrow out of the most-significant bit of the result; 
 cleared otherwise. This flag indicates an overflow condition for 
 unsigned-integer arithmetic. It is also used in multiple-precision 
 arithmetic.</p> */
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

/*! <p>General @ ( ' OF ' ) Specification ( Source: Intel Manuals, 
 Vol. 1, Section 3.4.3.1 ) :</p> 
 
 <p><b>Overflow flag</b> Set if the integer result is too large a 
 positive number or too small a negative number ( excluding the 
 sign-bit ) to fit in the destination operand; cleared otherwise. This 
 flag indicates an overflow condition for signed-integer ( two s 
 complement ) arithmetic.</p> */
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

/*! <p>General @ ( ' ZF ' ) Specification ( Source: Intel Manuals, 
 Vol. 1, Section 3.4.3.1 ) :</p> 
 
 <p><b>Zero flag</b> Set if the result is zero; cleared 
 otherwise.</p> */
val zf_spec : nat -> bits(1)

function zf_spec result = {
    if result == 0 then 0b1 else 0b0
}

val bitcount8 : bits(8) -> nat

function bitcount8 x = {
    if x == 0 then 0 else {
        nat_of_int(unsigned(loghead(1, x)) + check_range(0, 8, bitcount8(bits_of_int(logtail(1, unsigned(x)), 8))))
    }
}

val pf_spec8 : bits(8) -> bits(1)

function pf_spec8 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(unsigned(result)))))
}

val pf_spec16 : bits(16) -> bits(1)

function pf_spec16 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(unsigned(loghead(8, result))))))
}

val pf_spec32 : bits(32) -> bits(1)

function pf_spec32 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(unsigned(loghead(8, result))))))
}

val pf_spec64 : bits(64) -> bits(1)

function pf_spec64 result = {
    bool_to_bit(not_bool(logbitp(0, logcount(unsigned(loghead(8, result))))))
}

/*! <p>General @ ( ' PF ' ) Specification ( Source: Intel Manuals, 
 Vol. 1, Section 3.4.3.1 ) :</p> 
 
 <p><b>Parity flag</b> Set if the least-significant byte of the 
 result contains an even number of 1 bits; cleared otherwise.</p> */
val general_pf_spec_fn : ({|8, 16, 32, 64|}, nat) -> bits(1)

function general_pf_spec_fn (result_nbits, result) = {
    bool_to_bit(not_bool(logbitp(0, logcount(loghead(8, result)))))
}

val sf_spec8 : bits(8) -> bits(1)

function sf_spec8 result = {
    result[7 .. 7]
}

val sf_spec16 : bits(16) -> bits(1)

function sf_spec16 result = {
    result[15 .. 15]
}

val sf_spec32 : bits(32) -> bits(1)

function sf_spec32 result = {
    result[31 .. 31]
}

val sf_spec64 : bits(64) -> bits(1)

function sf_spec64 result = {
    result[63 .. 63]
}

/*! <p>General @ ( ' SF ' ) Specification ( Source: Intel Manuals, 
 Vol. 1, Section 3.4.3.1 ) :</p> 
 
 <p><b>Sign flag</b> Set equal to the most-significant bit of the 
 result, which is the sign bit of a signed integer. ( 0 indicates a 
 positive value and 1 indicates a negative value. ) </p> */
val general_sf_spec_fn : ({|8, 16, 32, 64|}, nat) -> bits(1)

function general_sf_spec_fn (result_nbits, result) = {
    get_slice_int(1, result, result_nbits - 1)
}

val add_af_spec8 : (bits(8), bits(8)) -> bits(1)

function add_af_spec8 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let add : bits(5) = bits_of_int(unsigned(dst_3_0) + unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(15 < unsigned(add));
    af
}

val add_af_spec16 : (bits(16), bits(16)) -> bits(1)

function add_af_spec16 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let add : bits(5) = bits_of_int(unsigned(dst_3_0) + unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(15 < unsigned(add));
    af
}

val add_af_spec32 : (bits(32), bits(32)) -> bits(1)

function add_af_spec32 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let add : bits(5) = bits_of_int(unsigned(dst_3_0) + unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(15 < unsigned(add));
    af
}

val add_af_spec64 : (bits(64), bits(64)) -> bits(1)

function add_af_spec64 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let add : bits(5) = bits_of_int(unsigned(dst_3_0) + unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(15 < unsigned(add));
    af
}

val sub_af_spec8 : (bits(8), bits(8)) -> bits(1)

function sub_af_spec8 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sub : sbits(5) = bits_of_int(unsigned(dst_3_0) - unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(signed(sub) < 0);
    af
}

val sub_af_spec16 : (bits(16), bits(16)) -> bits(1)

function sub_af_spec16 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sub : sbits(5) = bits_of_int(unsigned(dst_3_0) - unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(signed(sub) < 0);
    af
}

val sub_af_spec32 : (bits(32), bits(32)) -> bits(1)

function sub_af_spec32 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sub : sbits(5) = bits_of_int(unsigned(dst_3_0) - unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(signed(sub) < 0);
    af
}

val sub_af_spec64 : (bits(64), bits(64)) -> bits(1)

function sub_af_spec64 (dst, src) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sub : sbits(5) = bits_of_int(unsigned(dst_3_0) - unsigned(src_3_0), 5);
    let af : bits(1) = bool_to_bit(signed(sub) < 0);
    af
}

val adc_af_spec8 : (bits(8), bits(8), bits(1)) -> bits(1)

function adc_af_spec8 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let adc : bits(6) = bits_of_int(unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf)), 6);
    let af : bits(1) = bool_to_bit(15 < unsigned(adc));
    af
}

val adc_af_spec16 : (bits(16), bits(16), bits(1)) -> bits(1)

function adc_af_spec16 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let adc : bits(6) = bits_of_int(unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf)), 6);
    let af : bits(1) = bool_to_bit(15 < unsigned(adc));
    af
}

val adc_af_spec32 : (bits(32), bits(32), bits(1)) -> bits(1)

function adc_af_spec32 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let adc : bits(6) = bits_of_int(unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf)), 6);
    let af : bits(1) = bool_to_bit(15 < unsigned(adc));
    af
}

val adc_af_spec64 : (bits(64), bits(64), bits(1)) -> bits(1)

function adc_af_spec64 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let adc : bits(6) = bits_of_int(unsigned(dst_3_0) + (unsigned(src_3_0) + unsigned(cf)), 6);
    let af : bits(1) = bool_to_bit(15 < unsigned(adc));
    af
}

val sbb_af_spec8 : (bits(8), bits(8), bits(1)) -> bits(1)

function sbb_af_spec8 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sbb : int = unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(sail_zero_extend(cf, 4)));
    let af : bits(1) = bool_to_bit(sbb < 0);
    af
}

val sbb_af_spec16 : (bits(16), bits(16), bits(1)) -> bits(1)

function sbb_af_spec16 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sbb : int = unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(sail_zero_extend(cf, 4)));
    let af : bits(1) = bool_to_bit(sbb < 0);
    af
}

val sbb_af_spec32 : (bits(32), bits(32), bits(1)) -> bits(1)

function sbb_af_spec32 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sbb : int = unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(sail_zero_extend(cf, 4)));
    let af : bits(1) = bool_to_bit(sbb < 0);
    af
}

val sbb_af_spec64 : (bits(64), bits(64), bits(1)) -> bits(1)

function sbb_af_spec64 (dst, src, cf) = {
    let dst_3_0 : bits(4) = dst[3 .. 0];
    let src_3_0 : bits(4) = src[3 .. 0];
    let sbb : int = unsigned(dst_3_0) - (unsigned(src_3_0) + unsigned(sail_zero_extend(cf, 4)));
    let af : bits(1) = bool_to_bit(sbb < 0);
    af
}
