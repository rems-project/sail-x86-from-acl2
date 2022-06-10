
val div_spec_8 : (bits(16), bits(8)) -> (bool, bits(16), bits(8))

function div_spec_8 (dst, src) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(8, src) : bits(8);
    let quotient = the_bits(16, floor2(unsigned(dst), unsigned(src))) : bits(16);
    let remainder = the_bits(8, mod(unsigned(dst), unsigned(src))) : bits(8);
    let overflow? = (255 < unsigned(quotient)) : bool;
    if overflow? then {
        (true, get_slice_int(16, 0, 0), get_slice_int(8, 0, 0))
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_16 : (bits(32), bits(16)) -> (bool, bits(32), bits(16))

function div_spec_16 (dst, src) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(16, src) : bits(16);
    let quotient = the_bits(32, floor2(unsigned(dst), unsigned(src))) : bits(32);
    let remainder = the_bits(16, mod(unsigned(dst), unsigned(src))) : bits(16);
    let overflow? = (65535 < unsigned(quotient)) : bool;
    if overflow? then {
        (true, get_slice_int(32, 0, 0), get_slice_int(16, 0, 0))
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_32 : (bits(64), bits(32)) -> (bool, bits(64), bits(32))

function div_spec_32 (dst, src) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(32, src) : bits(32);
    let quotient = the_bits(64, floor2(unsigned(dst), unsigned(src))) : bits(64);
    let remainder = the_bits(32, mod(unsigned(dst), unsigned(src))) : bits(32);
    let overflow? = (4294967295 < unsigned(quotient)) : bool;
    if overflow? then {
        (true, get_slice_int(64, 0, 0), get_slice_int(32, 0, 0))
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_64 : (bits(128), bits(64)) -> (bool, bits(128), bits(64))

function div_spec_64 (dst, src) = {
    let dst = n_size(128, dst) : bits(128);
    let src = n_size(64, src) : bits(64);
    let quotient = the_bits(128, floor2(unsigned(dst), unsigned(src))) : bits(128);
    let remainder = the_bits(64, mod(unsigned(dst), unsigned(src))) : bits(64);
    let overflow? = (18446744073709551615 < unsigned(quotient)) : bool;
    if overflow? then {
        (true, get_slice_int(128, 0, 0), get_slice_int(64, 0, 0))
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec : ({|1, 2, 4, 8|}, bits(128), bits(64)) -> (bool, bits(128), bits(64))

function div_spec (size, dst, src) = {
    (match size {
      1 => let (elem0, elem1, elem2) = div_spec_8(the_bits(16, dst), the_bits(8, src)) : (bool, bits(16), bits(8)) in
        (elem0, the_bits(128, elem1), the_bits(64, elem2)),
      2 => let (elem0, elem1, elem2) = div_spec_16(the_bits(32, dst), the_bits(16, src)) : (bool, bits(32), bits(16)) in
        (elem0, the_bits(128, elem1), the_bits(64, elem2)),
      4 => let (elem0, elem1, elem2) = div_spec_32(the_bits(64, dst), the_bits(32, src)) : (bool, bits(64), bits(32)) in
        (elem0, the_bits(128, elem1), the_bits(64, elem2)),
      8 => div_spec_64(dst, src),
      _ => (false, get_slice_int(128, 0, 0), get_slice_int(64, 0, 0))
    }) : (bool, bits(128), bits(64))
}

val idiv_spec_8 : (bits(16), bits(8)) -> (bool, bits(8), bits(8))

function idiv_spec_8 (dst, src) = {
    let dst_int = n16_to_i16(dst) : sbits(16);
    let src_int = n08_to_i08(src) : sbits(8);
    let quotient_int = the_sbits(17, tdiv_int(signed(dst_int), signed(src_int))) : sbits(17);
    let remainder_int = the_sbits(8, tmod_int(signed(dst_int), signed(src_int))) : sbits(8);
    let overflow? = (signed(quotient_int) < -127 | 128 < signed(quotient_int)) : bool;
    if overflow? then {
        (true, get_slice_int(8, 0, 0), get_slice_int(8, 0, 0))
    } else {
        let quotient = n_size(8, quotient_int) : bits(8);
        let remainder = n_size(8, remainder_int) : bits(8);
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_16 : (bits(32), bits(16)) -> (bool, bits(16), bits(16))

function idiv_spec_16 (dst, src) = {
    let dst_int = n32_to_i32(dst) : sbits(32);
    let src_int = n16_to_i16(src) : sbits(16);
    let quotient_int = the_sbits(33, tdiv_int(signed(dst_int), signed(src_int))) : sbits(33);
    let remainder_int = the_sbits(16, tmod_int(signed(dst_int), signed(src_int))) : sbits(16);
    let overflow? = (signed(quotient_int) < -32767 | 32768 < signed(quotient_int)) : bool;
    if overflow? then {
        (true, get_slice_int(16, 0, 0), get_slice_int(16, 0, 0))
    } else {
        let quotient = n_size(16, quotient_int) : bits(16);
        let remainder = n_size(16, remainder_int) : bits(16);
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_32 : (bits(64), bits(32)) -> (bool, bits(32), bits(32))

function idiv_spec_32 (dst, src) = {
    let dst_int = n64_to_i64(dst) : sbits(64);
    let src_int = n32_to_i32(src) : sbits(32);
    let quotient_int = the_sbits(65, tdiv_int(signed(dst_int), signed(src_int))) : sbits(65);
    let remainder_int = the_sbits(32, tmod_int(signed(dst_int), signed(src_int))) : sbits(32);
    let overflow? = (signed(quotient_int) < -2147483647 | 2147483648 < signed(quotient_int)) : bool;
    if overflow? then {
        (true, get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    } else {
        let quotient = n_size(32, quotient_int) : bits(32);
        let remainder = n_size(32, remainder_int) : bits(32);
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_64 : (bits(128), bits(64)) -> (bool, bits(64), bits(64))

function idiv_spec_64 (dst, src) = {
    let dst_int = n128_to_i128(dst) : sbits(128);
    let src_int = n64_to_i64(src) : sbits(64);
    let quotient_int = the_sbits(129, tdiv_int(signed(dst_int), signed(src_int))) : sbits(129);
    let remainder_int = the_sbits(64, tmod_int(signed(dst_int), signed(src_int))) : sbits(64);
    let overflow? = (signed(quotient_int) < -9223372036854775807 | 9223372036854775808 < signed(quotient_int)) : bool;
    if overflow? then {
        (true, get_slice_int(64, 0, 0), get_slice_int(64, 0, 0))
    } else {
        let quotient = n_size(64, quotient_int) : bits(64);
        let remainder = n_size(64, remainder_int) : bits(64);
        (overflow?, quotient, remainder)
    }
}

val idiv_spec : ({|1, 2, 4, 8|}, bits(128), bits(64)) -> (bool, bits(64), bits(64))

function idiv_spec (size, dst, src) = {
    (match size {
      1 => let (elem0, elem1, elem2) = idiv_spec_8(the_bits(16, dst), the_bits(8, src)) : (bool, bits(8), bits(8)) in
        (elem0, the_bits(64, elem1), the_bits(64, elem2)),
      2 => let (elem0, elem1, elem2) = idiv_spec_16(the_bits(32, dst), the_bits(16, src)) : (bool, bits(16), bits(16))
      in
        (elem0, the_bits(64, elem1), the_bits(64, elem2)),
      4 => let (elem0, elem1, elem2) = idiv_spec_32(the_bits(64, dst), the_bits(32, src)) : (bool, bits(32), bits(32))
      in
        (elem0, the_bits(64, elem1), the_bits(64, elem2)),
      8 => idiv_spec_64(dst, src),
      _ => (false, get_slice_int(64, 0, 0), get_slice_int(64, 0, 0))
    }) : (bool, bits(64), bits(64))
}
