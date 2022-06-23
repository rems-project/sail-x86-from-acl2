$include <prelude.sail>
$include <string.sail>

val gpr_or_spec_1 : (bits(8), bits(8), bits(32)) -> (bits(8), bits(32), bits(32)) effect {escape}
function gpr_or_spec_1 (dst, src, input_rflags) =
let dst : bits(8) = (dst) in
let src : bits(8) = (src) in
let input_rflags : bits(32) = (input_rflags) in
let result : bits(8) = (slice(logior(dst, src), 0, 8)) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec8(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec8(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (output_rflags) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_or_spec_2 : (bits(16), bits(16), bits(32)) -> (bits(16), bits(32), bits(32)) effect {escape}
function gpr_or_spec_2 (dst, src, input_rflags) =
let dst : bits(16) = (dst) in
let src : bits(16) = (src) in
let input_rflags : bits(32) = (input_rflags) in
let result : bits(16) = (slice(logior(dst, src), 0, 16)) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec16(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec16(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (output_rflags) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_or_spec_4 : (bits(32), bits(32), bits(32)) -> (bits(32), bits(32), bits(32)) effect {escape}
function gpr_or_spec_4 (dst, src, input_rflags) =
let dst : bits(32) = (dst) in
let src : bits(32) = (src) in
let input_rflags : bits(32) = (input_rflags) in
let result : bits(32) = (slice(logior(dst, src), 0, 32)) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec32(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec32(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (output_rflags) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

val gpr_or_spec_8 : (bits(64), bits(64), bits(32)) -> (bits(64), bits(32), bits(32)) effect {escape}
function gpr_or_spec_8 (dst, src, input_rflags) =
let dst : bits(64) = (dst) in
let src : bits(64) = (src) in
let input_rflags : bits(32) = (input_rflags) in
let result : bits(64) = (slice(logior(dst, src), 0, 64)) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec64(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec64(result)) in
let of : {|0|} = (0) in
let output_rflags : bits(32) = (change_rflagsbits(input_rflags, Some(bits_of_int(cf, 1)), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) in
let output_rflags : bits(32) = (output_rflags) in
let undefined_flags : bits(32) = (set_rflagsbits_get_af(0b1, 0x0000_0000)) in
(result, output_rflags, undefined_flags)

