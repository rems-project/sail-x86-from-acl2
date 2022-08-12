$include <prelude.sail>
$include <string.sail>

val gpr_and_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_and_spec_1 (dst, src, input_rflags) =
let result : bits(8) = ((logand(dst, src))[7 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec8(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec8(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_and_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_and_spec_2 (dst, src, input_rflags) =
let result : bits(16) = ((logand(dst, src))[15 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec16(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec16(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_and_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_and_spec_4 (dst, src, input_rflags) =
let result : bits(32) = ((logand(dst, src))[31 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec32(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec32(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_and_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_and_spec_8 (dst, src, input_rflags) =
let result : bits(64) = ((logand(dst, src))[63 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec64(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec64(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

