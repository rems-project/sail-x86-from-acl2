$include <prelude.sail>
$include <string.sail>

val gpr_sub_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_1 (dst, src, input_rflags) =
let dst : bits(8) = (0x08) in
let src : bits(8) = (0x08) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let signed_raw_result : sbits(9) = (bits_of_int((signed(n08_to_i08(dst))) - (signed(n08_to_i08(src))), 9)) in
let result : bits(8) = (0x08) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) in
let pf : bits(1) = (pf_spec8(result)) in
let af : bits(1) = (sub_af_spec8(dst, src)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec8(result)) in
let of : bits(1) = (of_spec8(signed_raw_result)) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_2 (dst, src, input_rflags) =
let dst : bits(16) = (0x0010) in
let src : bits(16) = (0x0010) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let signed_raw_result : sbits(17) = (bits_of_int((signed(n16_to_i16(dst))) - (signed(n16_to_i16(src))), 17)) in
let result : bits(16) = (0x0010) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) in
let pf : bits(1) = (pf_spec16(result)) in
let af : bits(1) = (sub_af_spec16(dst, src)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec16(result)) in
let of : bits(1) = (of_spec16(signed_raw_result)) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_4 (dst, src, input_rflags) =
let dst : bits(32) = (0x0000_0020) in
let src : bits(32) = (0x0000_0020) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let signed_raw_result : sbits(33) = (bits_of_int((signed(n32_to_i32(dst))) - (signed(n32_to_i32(src))), 33)) in
let result : bits(32) = (0x0000_0020) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) in
let pf : bits(1) = (pf_spec32(result)) in
let af : bits(1) = (sub_af_spec32(dst, src)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec32(result)) in
let of : bits(1) = (of_spec32(signed_raw_result)) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_8 (dst, src, input_rflags) =
let dst : bits(64) = (0x0000_0000_0000_0040) in
let src : bits(64) = (0x0000_0000_0000_0040) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let signed_raw_result : sbits(65) = (bits_of_int((signed(n64_to_i64(dst))) - (signed(n64_to_i64(src))), 65)) in
let result : bits(64) = (0x0000_0000_0000_0040) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) in
let pf : bits(1) = (pf_spec64(result)) in
let af : bits(1) = (sub_af_spec64(dst, src)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec64(result)) in
let of : bits(1) = (of_spec64(signed_raw_result)) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_1 (dst, src, input_rflags) =
let dst : bits(8) = (0x08) in
let src : bits(8) = (0x08) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let input_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let signed_raw_result : sbits(10) = (bits_of_int((signed(n08_to_i08(dst))) - (signed(bits_of_int((signed(n08_to_i08(src))) + (unsigned(input_cf)), 9))), 10)) in
let result : bits(8) = (0x08) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(bits_of_int((unsigned(input_cf)) + (unsigned(src)), 9))))) in
let pf : bits(1) = (pf_spec8(result)) in
let af : bits(1) = (sbb_af_spec8(dst, src, input_cf)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec8(result)) in
let of : bits(1) = (of_spec8(truncate(signed_raw_result, 9))) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_2 (dst, src, input_rflags) =
let dst : bits(16) = (0x0010) in
let src : bits(16) = (0x0010) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let input_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let signed_raw_result : sbits(18) = (bits_of_int((signed(n16_to_i16(dst))) - (signed(bits_of_int((signed(n16_to_i16(src))) + (unsigned(input_cf)), 17))), 18)) in
let result : bits(16) = (0x0010) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(bits_of_int((unsigned(input_cf)) + (unsigned(src)), 17))))) in
let pf : bits(1) = (pf_spec16(result)) in
let af : bits(1) = (sbb_af_spec16(dst, src, input_cf)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec16(result)) in
let of : bits(1) = (of_spec16(truncate(signed_raw_result, 17))) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_4 (dst, src, input_rflags) =
let dst : bits(32) = (0x0000_0020) in
let src : bits(32) = (0x0000_0020) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let input_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let signed_raw_result : sbits(34) = (bits_of_int((signed(n32_to_i32(dst))) - (signed(bits_of_int((signed(n32_to_i32(src))) + (unsigned(input_cf)), 33))), 34)) in
let result : bits(32) = (0x0000_0020) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(bits_of_int((unsigned(input_cf)) + (unsigned(src)), 33))))) in
let pf : bits(1) = (pf_spec32(result)) in
let af : bits(1) = (sbb_af_spec32(dst, src, input_cf)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec32(result)) in
let of : bits(1) = (of_spec32(truncate(signed_raw_result, 33))) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_8 (dst, src, input_rflags) =
let dst : bits(64) = (0x0000_0000_0000_0040) in
let src : bits(64) = (0x0000_0000_0000_0040) in
let input_rflags : bits(32) = (n32(input_rflags)) in
let input_cf : bits(1) = (rflagsbits_get_cf(input_rflags)) in
let signed_raw_result : sbits(66) = (bits_of_int((signed(n64_to_i64(dst))) - (signed(bits_of_int((signed(n64_to_i64(src))) + (unsigned(input_cf)), 65))), 66)) in
let result : bits(64) = (0x0000_0000_0000_0040) in
let cf : bits(1) = (bool_to_bit((unsigned(dst)) < (unsigned(bits_of_int((unsigned(input_cf)) + (unsigned(src)), 65))))) in
let pf : bits(1) = (pf_spec64(result)) in
let af : bits(1) = (sbb_af_spec64(dst, src, input_cf)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec64(result)) in
let of : bits(1) = (of_spec64(truncate(signed_raw_result, 65))) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (n32(output_rflags)) in
let undefined_flags : bits(32) = (0x0000_0000) in
(result, output_rflags, undefined_flags)

