$include <prelude.sail>
$include <string.sail>

val sal_shl_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function sal_shl_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let raw_result = (ash(dst, src)) : int in
let result = (n_size(8, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 7))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let of = (b_xor(cf, logbit(7, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (8) <= (src)
then let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (8) - (src)))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sal_shl_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function sal_shl_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let raw_result = (ash(dst, src)) : int in
let result = (n_size(16, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 15))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let of = (b_xor(cf, logbit(15, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (16) <= (src)
then let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (16) - (src)))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sal_shl_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function sal_shl_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let raw_result = (ash(dst, src)) : int in
let result = (n_size(32, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 31))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let of = (b_xor(cf, logbit(31, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (32) <= (src)
then let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (32) - (src)))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sal_shl_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function sal_shl_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let raw_result = (ash(dst, src)) : int in
let result = (n_size(64, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 63))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let of = (b_xor(cf, logbit(63, result))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (64) <= (src)
then let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (64) - (src)))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' SAL/SHL ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set Reference 
 \ ( N-Z\ ) .</p> 
 
 <p>The shift arithmetic left \ ( SAL\ ) and shift logical left \ ( SHL\ ) 
 instructions perform the same operation; they shift the bits in the 
 destination operand to the left \ ( toward more significant bit 
 locations\ ) . For each shift count, the most significant bit of the 
 destination operand is shifted into the CF flag, and the least 
 significant bit is cleared. The OF flag is affected only on 1-bit 
 shifts. For left shifts, the OF flag is set to 0 if the 
 most-significant bit of the result is the same as the CF flag ( that 
 is, the top two bits of the original operand were the same ) ; 
 otherwise, it is set to 1.</p>*/
val sal_shl_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function sal_shl_spec (size, dst, src, input_rflags) =
(match size {
1 => sal_shl_spec_8(dst, src, input_rflags),
2 => sal_shl_spec_16(dst, src, input_rflags),
4 => sal_shl_spec_32(dst, src, input_rflags),
8 => sal_shl_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val shr_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function shr_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result = (ash(dst, neg_src)) : int in
let result = (n_size(8, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let of = (unsigned(get_slice_int(1, dst, 7))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (8) <= (src)
then let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val shr_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function shr_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result = (ash(dst, neg_src)) : int in
let result = (n_size(16, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let of = (unsigned(get_slice_int(1, dst, 15))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (16) <= (src)
then let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val shr_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function shr_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result = (ash(dst, neg_src)) : int in
let result = (n_size(32, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let of = (unsigned(get_slice_int(1, dst, 31))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (32) <= (src)
then let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val shr_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function shr_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result = (ash(dst, neg_src)) : int in
let result = (n_size(64, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let of = (unsigned(get_slice_int(1, dst, 63))) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (64) <= (src)
then let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' SHR ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set Reference \ ( N-Z\ ) .</p> 
 
 <p>The shift arithmetic right \ ( SAR\ ) and shift logical right \ ( SHR\ ) 
 instructions shift the bits of the destination operand to the right 
 \ ( toward less significant bit locations\ ) . For each shift count, the 
 least significant bit of the destination operand is shifted into the 
 CF flag, and the most significant bit is either set or cleared 
 depending on the instruction type. The SHR instruction clears the most 
 significant bit ( see Figure 7-8 in the Intel 64 and IA-32 
 Architectures Software Developer s Manual, Volume 1 ) ... The OF flag is 
 affected only on 1-bit shifts. For the SAR instruction, the OF flag is 
 cleared for all 1-bit shifts. For the SHR instruction, the OF flag is 
 set to the most-significant bit of the original operand.</p>*/
val shr_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function shr_spec (size, dst, src, input_rflags) =
(match size {
1 => shr_spec_8(dst, src, input_rflags),
2 => shr_spec_16(dst, src, input_rflags),
4 => shr_spec_32(dst, src, input_rflags),
8 => shr_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val sar_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function sar_spec_8 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result_not_sign_extended = (ash(dst, neg_src)) : int in
let raw_result = (if (logbit(7, dst)) == (1)
then loghead(8, ash(binary_logext(8, dst), neg_src))
else raw_result_not_sign_extended) : int in
let result = (n_size(8, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (8) <= (src)
then let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sar_spec_16 : (int, int, int) -> (int, int, int) effect {escape}
function sar_spec_16 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result_not_sign_extended = (ash(dst, neg_src)) : int in
let raw_result = (if (logbit(15, dst)) == (1)
then loghead(16, ash(binary_logext(16, dst), neg_src))
else raw_result_not_sign_extended) : int in
let result = (n_size(16, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (16) <= (src)
then let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sar_spec_32 : (int, int, int) -> (int, int, int) effect {escape}
function sar_spec_32 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result_not_sign_extended = (ash(dst, neg_src)) : int in
let raw_result = (if (logbit(31, dst)) == (1)
then loghead(32, ash(binary_logext(32, dst), neg_src))
else raw_result_not_sign_extended) : int in
let result = (n_size(32, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (32) <= (src)
then let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

val sar_spec_64 : (int, int, int) -> (int, int, int) effect {escape}
function sar_spec_64 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(6, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let neg_src = (negate(src)) : int in
let raw_result_not_sign_extended = (ash(dst, neg_src)) : int in
let raw_result = (if (logbit(63, dst)) == (1)
then loghead(64, ash(binary_logext(64, dst), neg_src))
else raw_result_not_sign_extended) : int in
let result = (n_size(64, raw_result)) : int in
let (output_rflags, undefined_flags) = ((match src {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (64) <= (src)
then let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, Some(1), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let cf = (unsigned(get_slice_int(1, dst, (src) - (1)))) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(result, output_rflags, undefined_flags)

/*Specification for the @ ( ' SAR ' ) instruction*/
/*<p>Source: Intel Manual, Volume 2B, Instruction Set Reference \ ( N-Z\ ) .</p> 
 
 <p>The shift arithmetic right \ ( SAR\ ) and shift logical right \ ( SHR\ ) 
 instructions shift the bits of the destination operand to the right 
 \ ( toward less significant bit locations\ ) . For each shift count, the 
 least significant bit of the destination operand is shifted into the 
 CF flag, and the most significant bit is either set or cleared 
 depending on the instruction type. The SAR instruction sets or clears 
 the most significant bit to correspond to the sign \ ( most significant 
 bit\ ) of the original value in the destination operand. In effect, the 
 SAR instruction fills the empty bit position s shifted value with the 
 sign of the unshifted value. ... The OF flag is affected only on 1-bit 
 shifts. For the SAR instruction, the OF flag is cleared for all 1-bit 
 shifts. For the SHR instruction, the OF flag is set to the 
 most-significant bit of the original operand.</p>*/
val sar_spec : (int, int, int, int) -> (int, int, int) effect {escape}
function sar_spec (size, dst, src, input_rflags) =
(match size {
1 => sar_spec_8(dst, src, input_rflags),
2 => sar_spec_16(dst, src, input_rflags),
4 => sar_spec_32(dst, src, input_rflags),
8 => sar_spec_64(dst, src, input_rflags),
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val shld_spec_16 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shld_spec_16 (dst, src, cnt, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(16, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let dst_src = (binary_logior(ash(dst, 16), src)) : int in
let output_dst = (n_size(16, ash(dst_src, cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 15))) : int in
let of = (if (logbit(15, dst)) == (logbit(15, output_dst))
then 0
else 1) : int in
let pf = (pf_spec16(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec16(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (16)
then let cf = (unsigned(get_slice_int(1, dst, (16) - (cnt)))) : int in
let pf = (pf_spec16(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec16(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (16), output_rflags, undefined_flags)

val shld_spec_32 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shld_spec_32 (dst, src, cnt, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(32, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let dst_src = (binary_logior(ash(dst, 32), src)) : int in
let output_dst = (n_size(32, ash(dst_src, cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 31))) : int in
let of = (if (logbit(31, dst)) == (logbit(31, output_dst))
then 0
else 1) : int in
let pf = (pf_spec32(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec32(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (32)
then let cf = (unsigned(get_slice_int(1, dst, (32) - (cnt)))) : int in
let pf = (pf_spec32(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec32(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (32), output_rflags, undefined_flags)

val shld_spec_64 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shld_spec_64 (dst, src, cnt, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(64, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let dst_src = (binary_logior(ash(dst, 64), src)) : int in
let output_dst = (n_size(64, ash(dst_src, cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 63))) : int in
let of = (if (logbit(63, dst)) == (logbit(63, output_dst))
then 0
else 1) : int in
let pf = (pf_spec64(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec64(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (64)
then let cf = (unsigned(get_slice_int(1, dst, (64) - (cnt)))) : int in
let pf = (pf_spec64(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec64(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (64), output_rflags, undefined_flags)

/*Specification for the @ ( ' SHLD ' ) instruction.*/
val shld_spec : (int, int, int, int, int) -> (int, bool, int, int) effect {escape}
function shld_spec (size, dst, src, cnt, input_rflags) =
(match size {
2 => shld_spec_16(dst, src, cnt, input_rflags),
4 => shld_spec_32(dst, src, cnt, input_rflags),
8 => shld_spec_64(dst, src, cnt, input_rflags),
_ => (0 : int, false, 0 : int, 0 : int)
}) : (int, bool, int, int)

val shrd_spec_16 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shrd_spec_16 (dst, src, cnt, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(16, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let src_dst = (binary_logior(ash(src, 16), dst)) : int in
let neg_cnt = (negate(cnt)) : int in
let output_dst = (n_size(16, ash(src_dst, neg_cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let of = (if (logbit(15, dst)) == (logbit(15, output_dst))
then 0
else 1) : int in
let pf = (pf_spec16(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec16(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (16)
then let cf = (unsigned(get_slice_int(1, dst, (cnt) - (1)))) : int in
let pf = (pf_spec16(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec16(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (16), output_rflags, undefined_flags)

val shrd_spec_32 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shrd_spec_32 (dst, src, cnt, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(32, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let src_dst = (binary_logior(ash(src, 32), dst)) : int in
let neg_cnt = (negate(cnt)) : int in
let output_dst = (n_size(32, ash(src_dst, neg_cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let of = (if (logbit(31, dst)) == (logbit(31, output_dst))
then 0
else 1) : int in
let pf = (pf_spec32(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec32(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (32)
then let cf = (unsigned(get_slice_int(1, dst, (cnt) - (1)))) : int in
let pf = (pf_spec32(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec32(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (32), output_rflags, undefined_flags)

val shrd_spec_64 : (int, int, int, int) -> (int, bool, int, int) effect {escape}
function shrd_spec_64 (dst, src, cnt, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(64, src)) : int in
let cnt = (n_size(6, cnt)) : int in
let input_rflags = (n32(input_rflags)) : int in
let src_dst = (binary_logior(ash(src, 64), dst)) : int in
let neg_cnt = (negate(cnt)) : int in
let output_dst = (n_size(64, ash(src_dst, neg_cnt))) : int in
let (output_rflags, undefined_flags) = ((match cnt {
0 => (input_rflags, 0 : int),
1 => let cf = (unsigned(get_slice_int(1, dst, 0))) : int in
let of = (if (logbit(63, dst)) == (logbit(63, output_dst))
then 0
else 1) : int in
let pf = (pf_spec64(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec64(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(output_rflags, undefined_flags),
_ => if (cnt) <= (64)
then let cf = (unsigned(get_slice_int(1, dst, (cnt) - (1)))) : int in
let pf = (pf_spec64(output_dst)) : int in
let zf = (zf_spec(output_dst)) : int in
let sf = (sf_spec64(output_dst)) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let undefined_flags = (change_rflagsbits(0, None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), Some(1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
(output_rflags, undefined_flags)
else let output_rflags = (input_rflags) : int in
let undefined_flags = (set_rflagsbits_get_cf(1, set_rflagsbits_get_of(1, set_rflagsbits_get_pf(1, set_rflagsbits_get_zf(1, set_rflagsbits_get_sf(1, set_rflagsbits_get_af(1, input_rflags))))))) : int in
(output_rflags, undefined_flags)
}) : (int, int)) : (int, int) in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (n32(undefined_flags)) : int in
(output_dst, (cnt) > (64), output_rflags, undefined_flags)

/*Specification for the @ ( ' SHRD ' ) instruction.*/
val shrd_spec : (int, int, int, int, int) -> (int, bool, int, int) effect {escape}
function shrd_spec (size, dst, src, cnt, input_rflags) =
(match size {
2 => shrd_spec_16(dst, src, cnt, input_rflags),
4 => shrd_spec_32(dst, src, cnt, input_rflags),
8 => shrd_spec_64(dst, src, cnt, input_rflags),
_ => (0 : int, false, 0 : int, 0 : int)
}) : (int, bool, int, int)

