$include <prelude.sail>
$include <string.sail>

val rcl_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function rcl_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(9, dst), 8, 1, old_cf)) : bits(9) in
let raw_result = (the_bits(9, rotate_left(unsigned(new_dst), 9, unsigned(src)))) : bits(9) in
let result = (n_size(8, raw_result)) : bits(8) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(8, raw_result)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(7, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(8, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rcl_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function rcl_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(17, dst), 16, 1, old_cf)) : bits(17) in
let raw_result = (the_bits(17, rotate_left(unsigned(new_dst), 17, unsigned(src)))) : bits(17) in
let result = (n_size(16, raw_result)) : bits(16) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(16, raw_result)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(15, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(16, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rcl_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function rcl_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(33, dst), 32, 1, old_cf)) : bits(33) in
let raw_result = (the_bits(33, rotate_left(unsigned(new_dst), 33, unsigned(src)))) : bits(33) in
let result = (n_size(32, raw_result)) : bits(32) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(32, raw_result)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(31, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(32, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rcl_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rcl_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let old_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(65, dst), 64, 1, old_cf)) : bits(65) in
let raw_result = (the_bits(65, rotate_left(unsigned(new_dst), 65, unsigned(src)))) : bits(65) in
let result = (n_size(64, raw_result)) : bits(64) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(64, raw_result)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(63, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(64, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' RCL ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set 
 Reference \ ( N-Z\ ) .</p> 
 
 <p>The RCL instruction shifts the CF flag into the least-significant 
 bit and shifts the most-significant bit into the CF flag. ... The OF 
 flag is defined only for the 1-bit rotates; it is undefined in all 
 other cases \ ( except RCL and RCR instructions only: a zero-bit rotate 
 does nothing, that is affects no flags\ ) . For left rotates, the OF 
 flag is set to the exclusive OR of the CF bit \ ( after the rotate\ ) and 
 the most-significant bit of the result.</p>*/
val rcl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rcl_spec (size, dst, src, input_rflags) =
(match size {
1 => let (elem0, elem1, elem2) = (rcl_spec_8(the_bits(8, dst), src, input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (rcl_spec_16(the_bits(16, dst), src, input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (rcl_spec_32(the_bits(32, dst), src, input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
8 => rcl_spec_64(dst, src, input_rflags),
_ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))

val rol_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function rol_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(8, the_bits(9, rotate_left(unsigned(dst), 8, unsigned(src))))) : bits(8) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (slice(result, 0, 1)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(7, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rol_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function rol_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(16, the_bits(17, rotate_left(unsigned(dst), 16, unsigned(src))))) : bits(16) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (slice(result, 0, 1)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(15, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rol_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function rol_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(32, the_bits(33, rotate_left(unsigned(dst), 32, unsigned(src))))) : bits(32) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (slice(result, 0, 1)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(31, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val rol_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rol_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(64, the_bits(65, rotate_left(unsigned(dst), 64, unsigned(src))))) : bits(64) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (slice(result, 0, 1)) : bits(1) in
let of = (b_xor(unsigned(cf), unsigned(logbit(63, result)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' ROL ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set 
 Reference \ ( N-Z\ ) .</p> 
 
 <p>For the ROL and ROR instructions, the original value of the CF flag 
 is not a part of the result, but the CF flag receives a copy of the 
 bit that was shifted from one end to the other. ... The OF flag is 
 defined only for the 1-bit rotates; it is undefined in all other cases 
 \ ( except ROL and RCR instructions only: a zero-bit rotate does 
 nothing, that is affects no flags\ ) . For left rotates, the OF flag is 
 set to the exclusive OR of the CF bit \ ( after the rotate\ ) and the 
 most-significant bit of the result.</p>*/
val rol_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rol_spec (size, dst, src, input_rflags) =
(match size {
1 => let (elem0, elem1, elem2) = (rol_spec_8(the_bits(8, dst), src, input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (rol_spec_16(the_bits(16, dst), src, input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (rol_spec_32(the_bits(32, dst), src, input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
8 => rol_spec_64(dst, src, input_rflags),
_ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))

val rcr_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function rcr_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(9, dst), 8, 1, input_cf)) : bits(9) in
let raw_result = (the_bits(9, rotate_right(unsigned(new_dst), 9, unsigned(src)))) : bits(9) in
let result = (n_size(8, raw_result)) : bits(8) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(8, raw_result)) : bits(1) in
let of = (b_xor(unsigned(logbit(7, result)), unsigned(slice(result, 6, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(8, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
(result, output_rflags, undefined_flags)

val rcr_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function rcr_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(17, dst), 16, 1, input_cf)) : bits(17) in
let raw_result = (the_bits(17, rotate_right(unsigned(new_dst), 17, unsigned(src)))) : bits(17) in
let result = (n_size(16, raw_result)) : bits(16) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(16, raw_result)) : bits(1) in
let of = (b_xor(unsigned(logbit(15, result)), unsigned(slice(result, 14, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(16, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
(result, output_rflags, undefined_flags)

val rcr_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function rcr_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(33, dst), 32, 1, input_cf)) : bits(33) in
let raw_result = (the_bits(33, rotate_right(unsigned(new_dst), 33, unsigned(src)))) : bits(33) in
let result = (n_size(32, raw_result)) : bits(32) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(32, raw_result)) : bits(1) in
let of = (b_xor(unsigned(logbit(31, result)), unsigned(slice(result, 30, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(32, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
(result, output_rflags, undefined_flags)

val rcr_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rcr_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let new_dst = (changeSlice(the_bits(65, dst), 64, 1, input_cf)) : bits(65) in
let raw_result = (the_bits(65, rotate_right(unsigned(new_dst), 65, unsigned(src)))) : bits(65) in
let result = (n_size(64, raw_result)) : bits(64) in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, get_slice_int(32, 0, 0)),
1 => let (elem0, elem1) = (let cf = (logbit(64, raw_result)) : bits(1) in
let of = (b_xor(unsigned(logbit(63, result)), unsigned(slice(result, 62, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (logbit(64, raw_result)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' RCR ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set 
 Reference \ ( N-Z\ ) .</p> 
 
 <p>The RCR instruction shifts the CF flag into the most-significant 
 bit and shifts the least-significant bit into the CF flag. ... The OF 
 flag is defined only for the 1-bit rotates; it is undefined in all 
 other cases \ ( except RCL and RCR instructions only: a zero-bit rotate 
 does nothing, that is affects no flags\ ) . For right rotates, the OF 
 flag is set to the exclusive OR of the two most-significant bits of 
 the result.</p>*/
val rcr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function rcr_spec (size, dst, src, input_rflags) =
(match size {
1 => let (elem0, elem1, elem2) = (rcr_spec_8(the_bits(8, dst), src, input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (rcr_spec_16(the_bits(16, dst), src, input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (rcr_spec_32(the_bits(32, dst), src, input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
8 => rcr_spec_64(dst, src, input_rflags),
_ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))

val ror_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function ror_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(8, the_bits(8, rotate_right(unsigned(dst), 8, unsigned(src))))) : bits(8) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (logbit(7, result)) : bits(1) in
let of = (b_xor(unsigned(logbit(7, result)), unsigned(slice(result, 6, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val ror_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function ror_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(16, the_bits(16, rotate_right(unsigned(dst), 16, unsigned(src))))) : bits(16) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (logbit(15, result)) : bits(1) in
let of = (b_xor(unsigned(logbit(15, result)), unsigned(slice(result, 14, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val ror_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function ror_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(32, the_bits(32, rotate_right(unsigned(dst), 32, unsigned(src))))) : bits(32) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (logbit(31, result)) : bits(1) in
let of = (b_xor(unsigned(logbit(31, result)), unsigned(slice(result, 30, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

val ror_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function ror_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(6, src)) : bits(6) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let result = (n_size(64, the_bits(64, rotate_right(unsigned(dst), 64, unsigned(src))))) : bits(64) in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(input_rflags, undefined_flags),
1 => let (elem0, elem1) = (let cf = (logbit(63, result)) : bits(1) in
let of = (b_xor(unsigned(logbit(63, result)), unsigned(slice(result, 62, 1)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
(output_rflags, 0 : {|0|})) : (bits(32), {|0|}) in
(elem0, get_slice_int(32, elem1, 0)),
_ => let cf = (slice(result, 0, 1)) : bits(1) in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : bits(32) in
let undefined_flags = (set_rflagsbits_get_of(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0))) : bits(32) in
(output_rflags, undefined_flags)
}) : (bits(32), bits(32))) : (bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' ROR ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set 
 Reference \ ( N-Z\ ) .</p> 
 
 <p>For the ROR and ROR instructions, the orig- inal value of the CF 
 flag is not a part of the result, but the CF flag receives a copy of 
 the bit that was shifted from one end to the other. ... The OF flag is 
 defined only for the 1-bit rotates; it is undefined in all other cases 
 \ ( except RCL and RCR instructions only: a zero-bit rotate does 
 nothing, that is affects no flags\ ) . For right rotates, the OF flag 
 is set to the exclusive OR of the two most-significant bits of the 
 result.</p>*/
val ror_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function ror_spec (size, dst, src, input_rflags) =
(match size {
1 => let (elem0, elem1, elem2) = (ror_spec_8(the_bits(8, dst), src, input_rflags)) : (bits(8), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
2 => let (elem0, elem1, elem2) = (ror_spec_16(the_bits(16, dst), src, input_rflags)) : (bits(16), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
4 => let (elem0, elem1, elem2) = (ror_spec_32(the_bits(32, dst), src, input_rflags)) : (bits(32), bits(32), bits(32)) in
(the_bits(64, elem0), elem1, elem2),
8 => ror_spec_64(dst, src, input_rflags),
_ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))

