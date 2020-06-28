$include <prelude.sail>
$include <string.sail>

val rcl_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function rcl_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 8, 1, old_cf)) : int in
let raw_result = (rotate_left(new_dst, 9, src)) : int in
let result = (n_size(8, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(8, raw_result)) : int in
let of = (b_xor(cf, logbit(7, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(8, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rcl_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function rcl_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 16, 1, old_cf)) : int in
let raw_result = (rotate_left(new_dst, 17, src)) : int in
let result = (n_size(16, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(16, raw_result)) : int in
let of = (b_xor(cf, logbit(15, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(16, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rcl_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function rcl_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 32, 1, old_cf)) : int in
let raw_result = (rotate_left(new_dst, 33, src)) : int in
let result = (n_size(32, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(32, raw_result)) : int in
let of = (b_xor(cf, logbit(31, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(32, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rcl_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function rcl_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let old_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 64, 1, old_cf)) : int in
let raw_result = (rotate_left(new_dst, 65, src)) : int in
let result = (n_size(64, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(64, raw_result)) : int in
let of = (b_xor(cf, logbit(63, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(64, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
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
val rcl_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function rcl_spec (size, dst, src, input_rflags) =
(match size {
1 => rcl_spec_8(dst, src, input_rflags),
2 => rcl_spec_16(dst, src, input_rflags),
4 => rcl_spec_32(dst, src, input_rflags),
8 => rcl_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val rol_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function rol_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(8, rotate_left(dst, 8, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let of = (b_xor(cf, logbit(7, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rol_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function rol_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(16, rotate_left(dst, 16, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let of = (b_xor(cf, logbit(15, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rol_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function rol_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(32, rotate_left(dst, 32, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let of = (b_xor(cf, logbit(31, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val rol_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function rol_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(64, rotate_left(dst, 64, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let of = (b_xor(cf, logbit(63, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
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
val rol_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function rol_spec (size, dst, src, input_rflags) =
(match size {
1 => rol_spec_8(dst, src, input_rflags),
2 => rol_spec_16(dst, src, input_rflags),
4 => rol_spec_32(dst, src, input_rflags),
8 => rol_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val rcr_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function rcr_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let input_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 8, 1, input_cf)) : int in
let raw_result = (rotate_right(new_dst, 9, src)) : int in
let result = (n_size(8, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(8, raw_result)) : int in
let of = (b_xor(logbit(7, result), unsigned(get_slice_int(1, result, 6)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(8, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
(result, output_rflags, undefined_flags)

val rcr_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function rcr_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let input_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 16, 1, input_cf)) : int in
let raw_result = (rotate_right(new_dst, 17, src)) : int in
let result = (n_size(16, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(16, raw_result)) : int in
let of = (b_xor(logbit(15, result), unsigned(get_slice_int(1, result, 14)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(16, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
(result, output_rflags, undefined_flags)

val rcr_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function rcr_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let input_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 32, 1, input_cf)) : int in
let raw_result = (rotate_right(new_dst, 33, src)) : int in
let result = (n_size(32, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(32, raw_result)) : int in
let of = (b_xor(logbit(31, result), unsigned(get_slice_int(1, result, 30)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(32, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
(result, output_rflags, undefined_flags)

val rcr_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function rcr_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let input_cf = (rflagsbits_get_cf(input_rflags)) : int in
let new_dst = (changeBits(dst, 64, 1, input_cf)) : int in
let raw_result = (rotate_right(new_dst, 65, src)) : int in
let result = (n_size(64, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (logbit(64, raw_result)) : int in
let of = (b_xor(logbit(63, result), unsigned(get_slice_int(1, result, 62)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (logbit(64, raw_result)) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
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
val rcr_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function rcr_spec (size, dst, src, input_rflags) =
(match size {
1 => rcr_spec_8(dst, src, input_rflags),
2 => rcr_spec_16(dst, src, input_rflags),
4 => rcr_spec_32(dst, src, input_rflags),
8 => rcr_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val ror_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function ror_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(8, rotate_right(dst, 8, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (logbit(7, result)) : int in
let of = (b_xor(logbit(7, result), unsigned(get_slice_int(1, result, 6)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val ror_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function ror_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(16, rotate_right(dst, 16, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (logbit(15, result)) : int in
let of = (b_xor(logbit(15, result), unsigned(get_slice_int(1, result, 14)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val ror_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function ror_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(32, rotate_right(dst, 32, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (logbit(31, result)) : int in
let of = (b_xor(logbit(31, result), unsigned(get_slice_int(1, result, 30)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
(result, output_rflags, undefined_flags)

val ror_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function ror_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (n_size(64, rotate_right(dst, 64, src))) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(input_rflags, undefined_flags),
1 => let cf = (logbit(63, result)) : int in
let of = (b_xor(logbit(63, result), unsigned(get_slice_int(1, result, 62)))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, 0 : int),
_ => let cf = (unsigned(get_slice_int(1, result, 0))) : int in
let output_rflags = (set_rflagsbits_get_cf(cf, input_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_of(1, 0)) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
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
val ror_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function ror_spec (size, dst, src, input_rflags) =
(match size {
1 => ror_spec_8(dst, src, input_rflags),
2 => ror_spec_16(dst, src, input_rflags),
4 => ror_spec_32(dst, src, input_rflags),
8 => ror_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

