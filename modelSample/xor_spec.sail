$ifndef __X86_XOR_SPEC
$define __X86_XOR_SPEC
$include <prelude.sail>
$include <string.sail>

val gpr_xor_spec_1 : (bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits) effect {escape}
function gpr_xor_spec_1 (dst, src, input_rflags) =
let result : bits(8) = ((logxor(dst, src))[7 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec8(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec8(result)) in
let of : {|0|} = (0) in
let output_rflags : rflagsbits = ([([([([([(input_rflags) with cf = (bits_of_int(cf, 1))]) with pf = (pf)]) with zf = (zf)]) with sf = (sf)]) with of = (bits_of_int(of, 1))]) in
let undefined_flags : rflagsbits = ([(Mk_rflagsbits(0x0000_0000)) with af = (0b1)]) in
(result, output_rflags, undefined_flags)

val gpr_xor_spec_2 : (bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits) effect {escape}
function gpr_xor_spec_2 (dst, src, input_rflags) =
let result : bits(16) = ((logxor(dst, src))[15 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec16(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec16(result)) in
let of : {|0|} = (0) in
let output_rflags : rflagsbits = ([([([([([(input_rflags) with cf = (bits_of_int(cf, 1))]) with pf = (pf)]) with zf = (zf)]) with sf = (sf)]) with of = (bits_of_int(of, 1))]) in
let undefined_flags : rflagsbits = ([(Mk_rflagsbits(0x0000_0000)) with af = (0b1)]) in
(result, output_rflags, undefined_flags)

val gpr_xor_spec_4 : (bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits) effect {escape}
function gpr_xor_spec_4 (dst, src, input_rflags) =
let result : bits(32) = ((logxor(dst, src))[31 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec32(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec32(result)) in
let of : {|0|} = (0) in
let output_rflags : rflagsbits = ([([([([([(input_rflags) with cf = (bits_of_int(cf, 1))]) with pf = (pf)]) with zf = (zf)]) with sf = (sf)]) with of = (bits_of_int(of, 1))]) in
let undefined_flags : rflagsbits = ([(Mk_rflagsbits(0x0000_0000)) with af = (0b1)]) in
(result, output_rflags, undefined_flags)

val gpr_xor_spec_8 : (bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits) effect {escape}
function gpr_xor_spec_8 (dst, src, input_rflags) =
let result : bits(64) = ((logxor(dst, src))[63 .. 0]) in
let cf : {|0|} = (0) in
let pf : bits(1) = (pf_spec64(result)) in
let zf : bits(1) = (bits_of_int(zf_spec(unsigned(result)), 1)) in
let sf : bits(1) = (sf_spec64(result)) in
let of : {|0|} = (0) in
let output_rflags : rflagsbits = ([([([([([(input_rflags) with cf = (bits_of_int(cf, 1))]) with pf = (pf)]) with zf = (zf)]) with sf = (sf)]) with of = (bits_of_int(of, 1))]) in
let undefined_flags : rflagsbits = ([(Mk_rflagsbits(0x0000_0000)) with af = (0b1)]) in
(result, output_rflags, undefined_flags)

$endif
