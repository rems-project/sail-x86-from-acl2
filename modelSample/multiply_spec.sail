$include <prelude.sail>
$include <string.sail>

val mul_spec_8 : (bits(8), bits(8)) -> (bits(8), bits(8), bits(16)) effect {escape}
function mul_spec_8 (dst, src) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(8, src)) : bits(8) in
let product = (the_bits(16, (unsigned(dst)) * (unsigned(src)))) : bits(16) in
let product_high = (slice(product, 8, 8)) : bits(8) in
let product_low = (slice(product, 0, 8)) : bits(8) in
(product_high, product_low, product)

val mul_spec_16 : (bits(16), bits(16)) -> (bits(16), bits(16), bits(32)) effect {escape}
function mul_spec_16 (dst, src) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(16, src)) : bits(16) in
let product = (the_bits(32, (unsigned(dst)) * (unsigned(src)))) : bits(32) in
let product_high = (slice(product, 16, 16)) : bits(16) in
let product_low = (slice(product, 0, 16)) : bits(16) in
(product_high, product_low, product)

val mul_spec_32 : (bits(32), bits(32)) -> (bits(32), bits(32), bits(64)) effect {escape}
function mul_spec_32 (dst, src) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(32, src)) : bits(32) in
let product = (the_bits(64, (unsigned(dst)) * (unsigned(src)))) : bits(64) in
let product_high = (slice(product, 32, 32)) : bits(32) in
let product_low = (slice(product, 0, 32)) : bits(32) in
(product_high, product_low, product)

val mul_spec_64 : (bits(64), bits(64)) -> (bits(64), bits(64), bits(128)) effect {escape}
function mul_spec_64 (dst, src) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(64, src)) : bits(64) in
let product = (the_bits(128, (unsigned(dst)) * (unsigned(src)))) : bits(128) in
let product_high = (slice(product, 64, 64)) : bits(64) in
let product_low = (slice(product, 0, 64)) : bits(64) in
(product_high, product_low, product)

/*Specification for the @ ( ' MUL ' ) ( unsigned multiply ) instruction*/
val mul_spec : ({|1, 2, 4, 8|}, bits(64), bits(64)) -> (bits(64), bits(64), bits(128)) effect {escape}
function mul_spec (size, dst, src) =
(match size {
1 => let (elem0, elem1, elem2) = (mul_spec_8(the_bits(8, dst), the_bits(8, src))) : (bits(8), bits(8), bits(16)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2)),
2 => let (elem0, elem1, elem2) = (mul_spec_16(the_bits(16, dst), the_bits(16, src))) : (bits(16), bits(16), bits(32)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2)),
4 => let (elem0, elem1, elem2) = (mul_spec_32(the_bits(32, dst), the_bits(32, src))) : (bits(32), bits(32), bits(64)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2)),
8 => mul_spec_64(dst, src),
_ => (get_slice_int(64, 0, 0), get_slice_int(64, 0, 0), get_slice_int(128, 0, 0))
}) : (bits(64), bits(64), bits(128))

val imul_spec_8 : (bits(8), bits(8)) -> (bits(8), bits(8), bits(16), bits(1)) effect {escape}
function imul_spec_8 (dst, src) =
let dst_int = (n08_to_i08(dst)) : sbits(8) in
let src_int = (n08_to_i08(src)) : sbits(8) in
let product_int = (the_sbits(16, (signed(dst_int)) * (signed(src_int)))) : sbits(16) in
let product = (n_size(16, product_int)) : bits(16) in
let product_high = (slice(product, 8, 8)) : bits(8) in
let product_low = (slice(product, 0, 8)) : bits(8) in
let product_low_int = (n08_to_i08(product_low)) : sbits(8) in
let cf_and_of = (bool_to_bit(not_bool((product_low_int) == (the_bits(8, product_int))))) : bits(1) in
(product_high, product_low, product, cf_and_of)

val imul_spec_16 : (bits(16), bits(16)) -> (bits(16), bits(16), bits(32), bits(1)) effect {escape}
function imul_spec_16 (dst, src) =
let dst_int = (n16_to_i16(dst)) : sbits(16) in
let src_int = (n16_to_i16(src)) : sbits(16) in
let product_int = (the_sbits(32, (signed(dst_int)) * (signed(src_int)))) : sbits(32) in
let product = (n_size(32, product_int)) : bits(32) in
let product_high = (slice(product, 16, 16)) : bits(16) in
let product_low = (slice(product, 0, 16)) : bits(16) in
let product_low_int = (n16_to_i16(product_low)) : sbits(16) in
let cf_and_of = (bool_to_bit(not_bool((the_bits(8, product_low_int)) == (the_bits(8, product_int))))) : bits(1) in
(product_high, product_low, product, cf_and_of)

val imul_spec_32 : (bits(32), bits(32)) -> (bits(32), bits(32), bits(64), bits(1)) effect {escape}
function imul_spec_32 (dst, src) =
let dst_int = (n32_to_i32(dst)) : sbits(32) in
let src_int = (n32_to_i32(src)) : sbits(32) in
let product_int = (the_sbits(64, (signed(dst_int)) * (signed(src_int)))) : sbits(64) in
let product = (n_size(64, product_int)) : bits(64) in
let product_high = (slice(product, 32, 32)) : bits(32) in
let product_low = (slice(product, 0, 32)) : bits(32) in
let product_low_int = (n32_to_i32(product_low)) : sbits(32) in
let cf_and_of = (bool_to_bit(not_bool((the_bits(8, product_low_int)) == (the_bits(8, product_int))))) : bits(1) in
(product_high, product_low, product, cf_and_of)

val imul_spec_64 : (bits(64), bits(64)) -> (bits(64), bits(64), bits(128), bits(1)) effect {escape}
function imul_spec_64 (dst, src) =
let dst_int = (n64_to_i64(dst)) : sbits(64) in
let src_int = (n64_to_i64(src)) : sbits(64) in
let product_int = (the_sbits(128, (signed(dst_int)) * (signed(src_int)))) : sbits(128) in
let product = (n_size(128, product_int)) : bits(128) in
let product_high = (slice(product, 64, 64)) : bits(64) in
let product_low = (slice(product, 0, 64)) : bits(64) in
let product_low_int = (n64_to_i64(product_low)) : sbits(64) in
let cf_and_of = (bool_to_bit(not_bool((the_bits(8, product_low_int)) == (the_bits(8, product_int))))) : bits(1) in
(product_high, product_low, product, cf_and_of)

/*Specification for the @ ( ' IMUL ' ) ( unsigned imultiply ) instruction*/
val imul_spec : ({|1, 2, 4, 8|}, bits(64), bits(64)) -> (bits(64), bits(64), bits(128), bits(1)) effect {escape}
function imul_spec (size, dst, src) =
(match size {
1 => let (elem0, elem1, elem2, elem3) = (imul_spec_8(the_bits(8, dst), the_bits(8, src))) : (bits(8), bits(8), bits(16), bits(1)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2), elem3),
2 => let (elem0, elem1, elem2, elem3) = (imul_spec_16(the_bits(16, dst), the_bits(16, src))) : (bits(16), bits(16), bits(32), bits(1)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2), elem3),
4 => let (elem0, elem1, elem2, elem3) = (imul_spec_32(the_bits(32, dst), the_bits(32, src))) : (bits(32), bits(32), bits(64), bits(1)) in
(the_bits(64, elem0), the_bits(64, elem1), the_bits(128, elem2), elem3),
8 => imul_spec_64(dst, src),
_ => (get_slice_int(64, 0, 0), get_slice_int(64, 0, 0), get_slice_int(128, 0, 0), get_slice_int(1, 0, 0))
}) : (bits(64), bits(64), bits(128), bits(1))

