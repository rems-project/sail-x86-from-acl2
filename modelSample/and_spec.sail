$include <prelude.sail>
$include <string.sail>

val gpr_and_spec_1 : (int, int, int) -> (int, int, int) effect {escape}
function gpr_and_spec_1 (dst, src, input_rflags) =
let dst = (n_size(8, dst)) : int in
let src = (n_size(8, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (unsigned(get_slice_int(8, binary_logand(dst, src), 0))) : int in
let cf = (0) : int in
let pf = (pf_spec8(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec8(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(result, output_rflags, undefined_flags)

val gpr_and_spec_2 : (int, int, int) -> (int, int, int) effect {escape}
function gpr_and_spec_2 (dst, src, input_rflags) =
let dst = (n_size(16, dst)) : int in
let src = (n_size(16, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (unsigned(get_slice_int(16, binary_logand(dst, src), 0))) : int in
let cf = (0) : int in
let pf = (pf_spec16(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec16(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(result, output_rflags, undefined_flags)

val gpr_and_spec_4 : (int, int, int) -> (int, int, int) effect {escape}
function gpr_and_spec_4 (dst, src, input_rflags) =
let dst = (n_size(32, dst)) : int in
let src = (n_size(32, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (unsigned(get_slice_int(32, binary_logand(dst, src), 0))) : int in
let cf = (0) : int in
let pf = (pf_spec32(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec32(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(result, output_rflags, undefined_flags)

val gpr_and_spec_8 : (int, int, int) -> (int, int, int) effect {escape}
function gpr_and_spec_8 (dst, src, input_rflags) =
let dst = (n_size(64, dst)) : int in
let src = (n_size(64, src)) : int in
let input_rflags = (n32(input_rflags)) : int in
let result = (unsigned(get_slice_int(64, binary_logand(dst, src), 0))) : int in
let cf = (0) : int in
let pf = (pf_spec64(result)) : int in
let zf = (zf_spec(result)) : int in
let sf = (sf_spec64(result)) : int in
let of = (0) : int in
let output_rflags = (change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(zf), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())) : int in
let output_rflags = (n32(output_rflags)) : int in
let undefined_flags = (set_rflagsbits_get_af(1, 0)) : int in
(result, output_rflags, undefined_flags)

