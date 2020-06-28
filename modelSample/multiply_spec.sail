$include <prelude.sail>
$include <string.sail>

val mul_spec_8 : (int, int) -> (int, int, int)
function mul_spec_8 (dst, src) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(8, src)) : int in
let product = ((dst) * (src)) : int in
let product_high = (unsigned(get_slice_int(8, product, 8))) : int in
let product_low = (unsigned(get_slice_int(8, product, 0))) : int in
(product_high, product_low, product)

val mul_spec_16 : (int, int) -> (int, int, int)
function mul_spec_16 (dst, src) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(16, src)) : int in
let product = ((dst) * (src)) : int in
let product_high = (unsigned(get_slice_int(16, product, 16))) : int in
let product_low = (unsigned(get_slice_int(16, product, 0))) : int in
(product_high, product_low, product)

val mul_spec_32 : (int, int) -> (int, int, int)
function mul_spec_32 (dst, src) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(32, src)) : int in
let product = ((dst) * (src)) : int in
let product_high = (unsigned(get_slice_int(32, product, 32))) : int in
let product_low = (unsigned(get_slice_int(32, product, 0))) : int in
(product_high, product_low, product)

val mul_spec_64 : (int, int) -> (int, int, int)
function mul_spec_64 (dst, src) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(64, src)) : int in
let product = ((dst) * (src)) : int in
let product_high = (unsigned(get_slice_int(64, product, 64))) : int in
let product_low = (unsigned(get_slice_int(64, product, 0))) : int in
(product_high, product_low, product)

/*Specification for the @ ( ' MUL ' ) ( unsigned multiply ) instruction*/
val mul_spec : (int, int, int) -> (int, int, int)
function mul_spec (size, dst, src) =
(match size {
1 => mul_spec_8(dst, src),
2 => mul_spec_16(dst, src),
4 => mul_spec_32(dst, src),
8 => mul_spec_64(dst, src),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val imul_spec_8 : (int, int) -> (int, int, int, int) effect {escape}
function imul_spec_8 (dst, src) =
let dst_int = (n08_to_i08(dst)) : int in
let src_int = (n08_to_i08(src)) : int in
let product_int = ((dst_int) * (src_int)) : int in
let product = (n_size(16, product_int)) : int in
let product_high = (unsigned(get_slice_int(8, product, 8))) : int in
let product_low = (unsigned(get_slice_int(8, product, 0))) : int in
let product_low_int = (n08_to_i08(product_low)) : int in
let cf_and_of = (bool_to_bit(not_bool((product_low_int) == (product_int)))) : int in
(product_high, product_low, product, cf_and_of)

val imul_spec_16 : (int, int) -> (int, int, int, int) effect {escape}
function imul_spec_16 (dst, src) =
let dst_int = (n16_to_i16(dst)) : int in
let src_int = (n16_to_i16(src)) : int in
let product_int = ((dst_int) * (src_int)) : int in
let product = (n_size(32, product_int)) : int in
let product_high = (unsigned(get_slice_int(16, product, 16))) : int in
let product_low = (unsigned(get_slice_int(16, product, 0))) : int in
let product_low_int = (n16_to_i16(product_low)) : int in
let cf_and_of = (bool_to_bit(not_bool((product_low_int) == (product_int)))) : int in
(product_high, product_low, product, cf_and_of)

val imul_spec_32 : (int, int) -> (int, int, int, int) effect {escape}
function imul_spec_32 (dst, src) =
let dst_int = (n32_to_i32(dst)) : int in
let src_int = (n32_to_i32(src)) : int in
let product_int = ((dst_int) * (src_int)) : int in
let product = (n_size(64, product_int)) : int in
let product_high = (unsigned(get_slice_int(32, product, 32))) : int in
let product_low = (unsigned(get_slice_int(32, product, 0))) : int in
let product_low_int = (n32_to_i32(product_low)) : int in
let cf_and_of = (bool_to_bit(not_bool((product_low_int) == (product_int)))) : int in
(product_high, product_low, product, cf_and_of)

val imul_spec_64 : (int, int) -> (int, int, int, int) effect {escape}
function imul_spec_64 (dst, src) =
let dst_int = (n64_to_i64(dst)) : int in
let src_int = (n64_to_i64(src)) : int in
let product_int = ((dst_int) * (src_int)) : int in
let product = (n_size(128, product_int)) : int in
let product_high = (unsigned(get_slice_int(64, product, 64))) : int in
let product_low = (unsigned(get_slice_int(64, product, 0))) : int in
let product_low_int = (n64_to_i64(product_low)) : int in
let cf_and_of = (bool_to_bit(not_bool((product_low_int) == (product_int)))) : int in
(product_high, product_low, product, cf_and_of)

/*Specification for the @ ( ' IMUL ' ) ( unsigned imultiply ) instruction*/
val imul_spec : (int, int, int) -> (int, int, int, int) effect {escape}
function imul_spec (size, dst, src) =
(match size {
1 => imul_spec_8(dst, src),
2 => imul_spec_16(dst, src),
4 => imul_spec_32(dst, src),
8 => imul_spec_64(dst, src),
_ => (0 : int, 0 : int, 0 : int, 0 : int)
}) : (int, int, int, int)

