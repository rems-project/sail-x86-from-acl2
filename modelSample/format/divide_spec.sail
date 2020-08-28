
val div_spec_8 : (int, int) -> (bool, int, int)

function div_spec_8 (dst, src) = {
    let dst = n_size(16, dst) : int;
    let src = n_size(8, src) : int;
    let quotient = floor2(dst, src) : int;
    let remainder = mod(dst, src) : int;
    let overflow? = (255 < quotient) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_16 : (int, int) -> (bool, int, int)

function div_spec_16 (dst, src) = {
    let dst = n_size(32, dst) : int;
    let src = n_size(16, src) : int;
    let quotient = floor2(dst, src) : int;
    let remainder = mod(dst, src) : int;
    let overflow? = (65535 < quotient) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_32 : (int, int) -> (bool, int, int)

function div_spec_32 (dst, src) = {
    let dst = n_size(64, dst) : int;
    let src = n_size(32, src) : int;
    let quotient = floor2(dst, src) : int;
    let remainder = mod(dst, src) : int;
    let overflow? = (4294967295 < quotient) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_64 : (int, int) -> (bool, int, int)

function div_spec_64 (dst, src) = {
    let dst = n_size(128, dst) : int;
    let src = n_size(64, src) : int;
    let quotient = floor2(dst, src) : int;
    let remainder = mod(dst, src) : int;
    let overflow? = (18446744073709551615 < quotient) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec : (int, int, int) -> (bool, int, int)

function div_spec (size, dst, src) = {
    (match size {
      1 => div_spec_8(dst, src),
      2 => div_spec_16(dst, src),
      4 => div_spec_32(dst, src),
      8 => div_spec_64(dst, src),
      _ => (false, 0 : int, 0 : int)
    }) : (bool, int, int)
}

val idiv_spec_8 : (int, int) -> (bool, int, int) effect {escape}

function idiv_spec_8 (dst, src) = {
    let dst_int = n16_to_i16(dst) : int;
    let src_int = n08_to_i08(src) : int;
    let quotient_int = tdiv_int(dst_int, src_int) : int;
    let remainder_int = tmod_int(dst_int, src_int) : int;
    let overflow? = (quotient_int < -127 | 128 < quotient_int) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        let quotient = n_size(8, quotient_int) : int;
        let remainder = n_size(8, remainder_int) : int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_16 : (int, int) -> (bool, int, int) effect {escape}

function idiv_spec_16 (dst, src) = {
    let dst_int = n32_to_i32(dst) : int;
    let src_int = n16_to_i16(src) : int;
    let quotient_int = tdiv_int(dst_int, src_int) : int;
    let remainder_int = tmod_int(dst_int, src_int) : int;
    let overflow? = (quotient_int < -32767 | 32768 < quotient_int) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        let quotient = n_size(16, quotient_int) : int;
        let remainder = n_size(16, remainder_int) : int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_32 : (int, int) -> (bool, int, int) effect {escape}

function idiv_spec_32 (dst, src) = {
    let dst_int = n64_to_i64(dst) : int;
    let src_int = n32_to_i32(src) : int;
    let quotient_int = tdiv_int(dst_int, src_int) : int;
    let remainder_int = tmod_int(dst_int, src_int) : int;
    let overflow? = (quotient_int < -2147483647 | 2147483648 < quotient_int) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        let quotient = n_size(32, quotient_int) : int;
        let remainder = n_size(32, remainder_int) : int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_64 : (int, int) -> (bool, int, int) effect {escape}

function idiv_spec_64 (dst, src) = {
    let dst_int = n128_to_i128(dst) : int;
    let src_int = n64_to_i64(src) : int;
    let quotient_int = tdiv_int(dst_int, src_int) : int;
    let remainder_int = tmod_int(dst_int, src_int) : int;
    let overflow? = (quotient_int < -9223372036854775807 | 9223372036854775808 < quotient_int) : bool;
    if overflow? then {
        (true, 0 : int, 0 : int)
    } else {
        let quotient = n_size(64, quotient_int) : int;
        let remainder = n_size(64, remainder_int) : int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec : (int, int, int) -> (bool, int, int) effect {escape}

function idiv_spec (size, dst, src) = {
    (match size {
      1 => idiv_spec_8(dst, src),
      2 => idiv_spec_16(dst, src),
      4 => idiv_spec_32(dst, src),
      8 => idiv_spec_64(dst, src),
      _ => (false, 0 : int, 0 : int)
    }) : (bool, int, int)
}
