
val div_spec_8 : (bits(16), bits(8)) -> (bool, bits(16), bits(8))

function div_spec_8 (dst, src) = {
    let quotient : bits(16) = bits_of_int(floor2(unsigned(dst), unsigned(src)), 16);
    let remainder : bits(8) = bits_of_int(mod(unsigned(dst), unsigned(src)), 8);
    let overflow? : bool = 255 < unsigned(quotient);
    if overflow? then {
        (true, 0x0000, 0x00)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_16 : (bits(32), bits(16)) -> (bool, bits(32), bits(16))

function div_spec_16 (dst, src) = {
    let quotient : bits(32) = bits_of_int(floor2(unsigned(dst), unsigned(src)), 32);
    let remainder : bits(16) = bits_of_int(mod(unsigned(dst), unsigned(src)), 16);
    let overflow? : bool = 65535 < unsigned(quotient);
    if overflow? then {
        (true, 0x00000000, 0x0000)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_32 : (bits(64), bits(32)) -> (bool, bits(64), bits(32))

function div_spec_32 (dst, src) = {
    let quotient : bits(64) = bits_of_int(floor2(unsigned(dst), unsigned(src)), 64);
    let remainder : bits(32) = bits_of_int(mod(unsigned(dst), unsigned(src)), 32);
    let overflow? : bool = 4294967295 < unsigned(quotient);
    if overflow? then {
        (true, 0x0000000000000000, 0x00000000)
    } else {
        (overflow?, quotient, remainder)
    }
}

val div_spec_64 : (bits(128), bits(64)) -> (bool, bits(128), bits(64))

function div_spec_64 (dst, src) = {
    let quotient : bits(128) = bits_of_int(floor2(unsigned(dst), unsigned(src)), 128);
    let remainder : bits(64) = bits_of_int(mod(unsigned(dst), unsigned(src)), 64);
    let overflow? : bool = 18446744073709551615 < unsigned(quotient);
    if overflow? then {
        (true, 0x00000000000000000000000000000000, 0x0000000000000000)
    } else {
        (overflow?, quotient, remainder)
    }
}

/*! Specification for the @ ( ' DIV ' ) ( unsigned divide ) instruction */
val div_spec : ({|1, 2, 4, 8|}, bits(128), bits(64)) -> (bool, bits(128), bits(64))

function div_spec (size, dst, src) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bool, bits(16), bits(8)) = div_spec_8(truncate(dst, 16), truncate(src, 8));
          (elem0, sail_zero_extend(elem1, 128), sail_zero_extend(elem2, 64))
      },
      2 => {
          let (elem0, elem1, elem2) : (bool, bits(32), bits(16)) = div_spec_16(truncate(dst, 32), truncate(src, 16));
          (elem0, sail_zero_extend(elem1, 128), sail_zero_extend(elem2, 64))
      },
      4 => {
          let (elem0, elem1, elem2) : (bool, bits(64), bits(32)) = div_spec_32(truncate(dst, 64), truncate(src, 32));
          (elem0, sail_zero_extend(elem1, 128), sail_zero_extend(elem2, 64))
      },
      8 => div_spec_64(dst, src),
      _ => (false, 0x00000000000000000000000000000000, 0x0000000000000000)
    }) : (bool, bits(128), bits(64))
}

val idiv_spec_8 : (bits(16), bits(8)) -> (bool, bits(8), bits(8))

function idiv_spec_8 (dst, src) = {
    let dst_int : sbits(16) = dst;
    let src_int : sbits(8) = src;
    let quotient_int : sbits(17) = bits_of_int(tdiv_int(signed(dst_int), signed(src_int)), 17);
    let remainder_int : sbits(8) = bits_of_int(tmod_int(signed(dst_int), signed(src_int)), 8);
    let overflow? : bool = signed(quotient_int) < -128 | 127 < signed(quotient_int);
    if overflow? then {
        (true, 0x00, 0x00)
    } else {
        let quotient : bits(8) = truncate(quotient_int, 8);
        let remainder : bits(8) = remainder_int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_16 : (bits(32), bits(16)) -> (bool, bits(16), bits(16))

function idiv_spec_16 (dst, src) = {
    let dst_int : sbits(32) = dst;
    let src_int : sbits(16) = src;
    let quotient_int : sbits(33) = bits_of_int(tdiv_int(signed(dst_int), signed(src_int)), 33);
    let remainder_int : sbits(16) = bits_of_int(tmod_int(signed(dst_int), signed(src_int)), 16);
    let overflow? : bool = signed(quotient_int) < -32768 | 32767 < signed(quotient_int);
    if overflow? then {
        (true, 0x0000, 0x0000)
    } else {
        let quotient : bits(16) = truncate(quotient_int, 16);
        let remainder : bits(16) = remainder_int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_32 : (bits(64), bits(32)) -> (bool, bits(32), bits(32))

function idiv_spec_32 (dst, src) = {
    let dst_int : sbits(64) = dst;
    let src_int : sbits(32) = src;
    let quotient_int : sbits(65) = bits_of_int(tdiv_int(signed(dst_int), signed(src_int)), 65);
    let remainder_int : sbits(32) = bits_of_int(tmod_int(signed(dst_int), signed(src_int)), 32);
    let overflow? : bool = signed(quotient_int) < -2147483648 | 2147483647 < signed(quotient_int);
    if overflow? then {
        (true, 0x00000000, 0x00000000)
    } else {
        let quotient : bits(32) = truncate(quotient_int, 32);
        let remainder : bits(32) = remainder_int;
        (overflow?, quotient, remainder)
    }
}

val idiv_spec_64 : (bits(128), bits(64)) -> (bool, bits(64), bits(64))

function idiv_spec_64 (dst, src) = {
    let dst_int : sbits(128) = dst;
    let src_int : sbits(64) = src;
    let quotient_int : sbits(129) = bits_of_int(tdiv_int(signed(dst_int), signed(src_int)), 129);
    let remainder_int : sbits(64) = bits_of_int(tmod_int(signed(dst_int), signed(src_int)), 64);
    let overflow? : bool = signed(quotient_int) < -9223372036854775808 | 9223372036854775807 < signed(quotient_int);
    if overflow? then {
        (true, 0x0000000000000000, 0x0000000000000000)
    } else {
        let quotient : bits(64) = truncate(quotient_int, 64);
        let remainder : bits(64) = remainder_int;
        (overflow?, quotient, remainder)
    }
}

/*! Specification for the @ ( ' IDIV ' ) ( unsigned idivide ) instruction */
val idiv_spec : ({|1, 2, 4, 8|}, bits(128), bits(64)) -> (bool, bits(64), bits(64))

function idiv_spec (size, dst, src) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bool, bits(8), bits(8)) = idiv_spec_8(truncate(dst, 16), truncate(src, 8));
          (elem0, sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 64))
      },
      2 => {
          let (elem0, elem1, elem2) : (bool, bits(16), bits(16)) = idiv_spec_16(truncate(dst, 32), truncate(src, 16));
          (elem0, sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 64))
      },
      4 => {
          let (elem0, elem1, elem2) : (bool, bits(32), bits(32)) = idiv_spec_32(truncate(dst, 64), truncate(src, 32));
          (elem0, sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 64))
      },
      8 => idiv_spec_64(dst, src),
      _ => (false, 0x0000000000000000, 0x0000000000000000)
    }) : (bool, bits(64), bits(64))
}
