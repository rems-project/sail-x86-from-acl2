
val mul_spec_8 : (bits(8), bits(8)) -> (bits(8), bits(8), bits(16))

function mul_spec_8 (dst, src) = {
    let product : bits(16) = bits_of_int(unsigned(dst) * unsigned(src), 16);
    let product_high : bits(8) = product[15 .. 8];
    let product_low : bits(8) = product[7 .. 0];
    (product_high, product_low, product)
}

val mul_spec_16 : (bits(16), bits(16)) -> (bits(16), bits(16), bits(32))

function mul_spec_16 (dst, src) = {
    let product : bits(32) = bits_of_int(unsigned(dst) * unsigned(src), 32);
    let product_high : bits(16) = product[31 .. 16];
    let product_low : bits(16) = product[15 .. 0];
    (product_high, product_low, product)
}

val mul_spec_32 : (bits(32), bits(32)) -> (bits(32), bits(32), bits(64))

function mul_spec_32 (dst, src) = {
    let product : bits(64) = bits_of_int(unsigned(dst) * unsigned(src), 64);
    let product_high : bits(32) = product[63 .. 32];
    let product_low : bits(32) = product[31 .. 0];
    (product_high, product_low, product)
}

val mul_spec_64 : (bits(64), bits(64)) -> (bits(64), bits(64), bits(128))

function mul_spec_64 (dst, src) = {
    let product : bits(128) = bits_of_int(unsigned(dst) * unsigned(src), 128);
    let product_high : bits(64) = product[127 .. 64];
    let product_low : bits(64) = product[63 .. 0];
    (product_high, product_low, product)
}

/*! Specification for the @ ( ' MUL ' ) ( unsigned multiply ) instruction */
val mul_spec : ({|1, 2, 4, 8|}, bits(64), bits(64)) -> (bits(64), bits(64), bits(128))

function mul_spec (size, dst, src) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), bits(8), bits(16)) = mul_spec_8(truncate(dst, 8), truncate(src, 8));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128))
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), bits(16), bits(32)) = mul_spec_16(truncate(dst, 16), truncate(src, 16));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128))
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), bits(32), bits(64)) = mul_spec_32(truncate(dst, 32), truncate(src, 32));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128))
      },
      8 => mul_spec_64(dst, src),
      _ => (0x0000000000000000, 0x0000000000000000, 0x00000000000000000000000000000000)
    }) : (bits(64), bits(64), bits(128))
}

val imul_spec_8 : (bits(8), bits(8)) -> (bits(8), bits(8), bits(16), bits(1))

function imul_spec_8 (dst, src) = {
    let dst_int : sbits(8) = dst;
    let src_int : sbits(8) = src;
    let product_int : sbits(16) = bits_of_int(signed(dst_int) * signed(src_int), 16);
    let product : bits(16) = product_int;
    let product_high : bits(8) = product[15 .. 8];
    let product_low : bits(8) = product[7 .. 0];
    let product_low_int : sbits(8) = product_low;
    let cf_and_of : bits(1) = bool_to_bits(not_bool(sail_sign_extend(product_low_int, 16) == product_int));
    (product_high, product_low, product, cf_and_of)
}

val imul_spec_16 : (bits(16), bits(16)) -> (bits(16), bits(16), bits(32), bits(1))

function imul_spec_16 (dst, src) = {
    let dst_int : sbits(16) = dst;
    let src_int : sbits(16) = src;
    let product_int : sbits(32) = bits_of_int(signed(dst_int) * signed(src_int), 32);
    let product : bits(32) = product_int;
    let product_high : bits(16) = product[31 .. 16];
    let product_low : bits(16) = product[15 .. 0];
    let product_low_int : sbits(16) = product_low;
    let cf_and_of : bits(1) = bool_to_bits(not_bool(sail_sign_extend(product_low_int, 32) == product_int));
    (product_high, product_low, product, cf_and_of)
}

val imul_spec_32 : (bits(32), bits(32)) -> (bits(32), bits(32), bits(64), bits(1))

function imul_spec_32 (dst, src) = {
    let dst_int : sbits(32) = dst;
    let src_int : sbits(32) = src;
    let product_int : sbits(64) = bits_of_int(signed(dst_int) * signed(src_int), 64);
    let product : bits(64) = product_int;
    let product_high : bits(32) = product[63 .. 32];
    let product_low : bits(32) = product[31 .. 0];
    let product_low_int : sbits(32) = product_low;
    let cf_and_of : bits(1) = bool_to_bits(not_bool(sail_sign_extend(product_low_int, 64) == product_int));
    (product_high, product_low, product, cf_and_of)
}

val imul_spec_64 : (bits(64), bits(64)) -> (bits(64), bits(64), bits(128), bits(1))

function imul_spec_64 (dst, src) = {
    let dst_int : sbits(64) = dst;
    let src_int : sbits(64) = src;
    let product_int : sbits(128) = bits_of_int(signed(dst_int) * signed(src_int), 128);
    let product : bits(128) = product_int;
    let product_high : bits(64) = product[127 .. 64];
    let product_low : bits(64) = product[63 .. 0];
    let product_low_int : sbits(64) = product_low;
    let cf_and_of : bits(1) = bool_to_bits(not_bool(sail_sign_extend(product_low_int, 128) == product_int));
    (product_high, product_low, product, cf_and_of)
}

/*! Specification for the @ ( ' IMUL ' ) ( unsigned imultiply ) instruction */
val imul_spec : ({|1, 2, 4, 8|}, bits(64), bits(64)) -> (bits(64), bits(64), bits(128), bits(1))

function imul_spec (size, dst, src) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2, elem3) : (bits(8), bits(8), bits(16), bits(1)) = imul_spec_8(truncate(dst, 8), truncate(src, 8));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128), elem3)
      },
      2 => {
          let (elem0, elem1, elem2, elem3) : (bits(16), bits(16), bits(32), bits(1)) = imul_spec_16(truncate(dst, 16), truncate(src, 16));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128), elem3)
      },
      4 => {
          let (elem0, elem1, elem2, elem3) : (bits(32), bits(32), bits(64), bits(1)) = imul_spec_32(truncate(dst, 32), truncate(src, 32));
          (sail_zero_extend(elem0, 64), sail_zero_extend(elem1, 64), sail_zero_extend(elem2, 128), elem3)
      },
      8 => imul_spec_64(dst, src),
      _ => (0x0000000000000000, 0x0000000000000000, 0x00000000000000000000000000000000, 0b0)
    }) : (bits(64), bits(64), bits(128), bits(1))
}
