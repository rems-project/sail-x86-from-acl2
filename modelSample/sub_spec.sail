$include <prelude.sail>
$include <string.sail>

val gpr_sub_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_1 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(8, src)) : bits(8) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let signed_raw_result = (the_sbits(9, (signed(n08_to_i08(dst))) - (signed(n08_to_i08(src))))) : sbits(9) in
let result = (n_size(8, signed_raw_result)) : bits(8) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) : bits(1) in
let pf = (pf_spec8(result)) : bits(1) in
let af = (sub_af_spec8(dst, src)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec8(result)) : bits(1) in
let of = (of_spec8(signed_raw_result)) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_2 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(16, src)) : bits(16) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let signed_raw_result = (the_sbits(17, (signed(n16_to_i16(dst))) - (signed(n16_to_i16(src))))) : sbits(17) in
let result = (n_size(16, signed_raw_result)) : bits(16) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) : bits(1) in
let pf = (pf_spec16(result)) : bits(1) in
let af = (sub_af_spec16(dst, src)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec16(result)) : bits(1) in
let of = (of_spec16(signed_raw_result)) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_4 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(32, src)) : bits(32) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let signed_raw_result = (the_sbits(33, (signed(n32_to_i32(dst))) - (signed(n32_to_i32(src))))) : sbits(33) in
let result = (n_size(32, signed_raw_result)) : bits(32) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) : bits(1) in
let pf = (pf_spec32(result)) : bits(1) in
let af = (sub_af_spec32(dst, src)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec32(result)) : bits(1) in
let of = (of_spec32(signed_raw_result)) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sub_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_sub_spec_8 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(64, src)) : bits(64) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let signed_raw_result = (the_sbits(65, (signed(n64_to_i64(dst))) - (signed(n64_to_i64(src))))) : sbits(65) in
let result = (n_size(64, signed_raw_result)) : bits(64) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(src)))) : bits(1) in
let pf = (pf_spec64(result)) : bits(1) in
let af = (sub_af_spec64(dst, src)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec64(result)) : bits(1) in
let of = (of_spec64(signed_raw_result)) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_1 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : bits(8) in
let src = (n_size(8, src)) : bits(8) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let signed_raw_result = (the_sbits(10, (signed(n08_to_i08(dst))) - (signed(the_sbits(9, (signed(n08_to_i08(src))) + (unsigned(input_cf))))))) : sbits(10) in
let result = (n_size(8, signed_raw_result)) : bits(8) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(the_bits(9, (unsigned(input_cf)) + (unsigned(src))))))) : bits(1) in
let pf = (pf_spec8(result)) : bits(1) in
let af = (sbb_af_spec8(dst, src, input_cf)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec8(result)) : bits(1) in
let of = (of_spec8(the_sbits(9, signed_raw_result))) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_2 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : bits(16) in
let src = (n_size(16, src)) : bits(16) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let signed_raw_result = (the_sbits(18, (signed(n16_to_i16(dst))) - (signed(the_sbits(17, (signed(n16_to_i16(src))) + (unsigned(input_cf))))))) : sbits(18) in
let result = (n_size(16, signed_raw_result)) : bits(16) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(the_bits(17, (unsigned(input_cf)) + (unsigned(src))))))) : bits(1) in
let pf = (pf_spec16(result)) : bits(1) in
let af = (sbb_af_spec16(dst, src, input_cf)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec16(result)) : bits(1) in
let of = (of_spec16(the_sbits(17, signed_raw_result))) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_4 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : bits(32) in
let src = (n_size(32, src)) : bits(32) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let signed_raw_result = (the_sbits(34, (signed(n32_to_i32(dst))) - (signed(the_sbits(33, (signed(n32_to_i32(src))) + (unsigned(input_cf))))))) : sbits(34) in
let result = (n_size(32, signed_raw_result)) : bits(32) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(the_bits(33, (unsigned(input_cf)) + (unsigned(src))))))) : bits(1) in
let pf = (pf_spec32(result)) : bits(1) in
let af = (sbb_af_spec32(dst, src, input_cf)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec32(result)) : bits(1) in
let of = (of_spec32(the_sbits(33, signed_raw_result))) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

val gpr_sbb_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_sbb_spec_8 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : bits(64) in
let src = (n_size(64, src)) : bits(64) in
let input_rflags = (n32(input_rflags)) : bits(32) in
let input_cf = (rflagsbits_get_cf(input_rflags)) : bits(1) in
let signed_raw_result = (the_sbits(66, (signed(n64_to_i64(dst))) - (signed(the_sbits(65, (signed(n64_to_i64(src))) + (unsigned(input_cf))))))) : sbits(66) in
let result = (n_size(64, signed_raw_result)) : bits(64) in
let cf = (bool_to_bit((unsigned(dst)) < (unsigned(the_bits(65, (unsigned(input_cf)) + (unsigned(src))))))) : bits(1) in
let pf = (pf_spec64(result)) : bits(1) in
let af = (sbb_af_spec64(dst, src, input_cf)) : bits(1) in
let zf = (the_bits(1, zf_spec(unsigned(result)))) : bits(1) in
let sf = (sf_spec64(result)) : bits(1) in
let of = (of_spec64(the_sbits(65, signed_raw_result))) : bits(1) in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), Some(af), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : bits(32) in
let output_rflags = (n32(output_rflags)) : bits(32) in
let undefined_flags = (get_slice_int(32, 0, 0)) : bits(32) in
(result, output_rflags, undefined_flags)

