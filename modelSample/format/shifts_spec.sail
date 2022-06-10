
val sal_shl_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function sal_shl_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = ash(unsigned(dst), unsigned(src)) : int;
    let result = n_size(8, raw_result) : bits(8);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 7, 1) : bits(1) in
      let pf = pf_spec8(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec8(result) : bits(1) in
      let of = b_xor(unsigned(cf), unsigned(logbit(7, result))) : int in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, 8 - unsigned(src), 1) : bits(1);
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function sal_shl_spec_16 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = ash(unsigned(dst), unsigned(src)) : int;
    let result = n_size(16, raw_result) : bits(16);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 15, 1) : bits(1) in
      let pf = pf_spec16(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec16(result) : bits(1) in
      let of = b_xor(unsigned(cf), unsigned(logbit(15, result))) : int in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, 16 - unsigned(src), 1) : bits(1);
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function sal_shl_spec_32 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = ash(unsigned(dst), unsigned(src)) : int;
    let result = n_size(32, raw_result) : bits(32);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 31, 1) : bits(1) in
      let pf = pf_spec32(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec32(result) : bits(1) in
      let of = b_xor(unsigned(cf), unsigned(logbit(31, result))) : int in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, 32 - unsigned(src), 1) : bits(1);
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sal_shl_spec_64 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let raw_result = ash(unsigned(dst), unsigned(src)) : int;
    let result = n_size(64, raw_result) : bits(64);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 63, 1) : bits(1) in
      let pf = pf_spec64(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec64(result) : bits(1) in
      let of = b_xor(unsigned(cf), unsigned(logbit(63, result))) : int in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, 64 - unsigned(src), 1) : bits(1);
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sal_shl_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) =
        sal_shl_spec_8(the_bits(8, dst), src, input_rflags) : (bits(8), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      2 => let (elem0, elem1, elem2) =
        sal_shl_spec_16(the_bits(16, dst), src, input_rflags) : (bits(16), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      4 => let (elem0, elem1, elem2) =
        sal_shl_spec_32(the_bits(32, dst), src, input_rflags) : (bits(32), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      8 => sal_shl_spec_64(dst, src, input_rflags),
      _ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    }) : (bits(64), bits(32), bits(32))
}

val shr_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function shr_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(9, negate(unsigned(src))) : sbits(9);
    let raw_result = the_bits(8, ash(unsigned(dst), signed(neg_src))) : bits(8);
    let result = n_size(8, raw_result) : bits(8);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec8(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec8(result) : bits(1) in
      let of = slice(dst, 7, 1) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val shr_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function shr_spec_16 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(17, negate(unsigned(src))) : sbits(17);
    let raw_result = the_bits(16, ash(unsigned(dst), signed(neg_src))) : bits(16);
    let result = n_size(16, raw_result) : bits(16);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec16(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec16(result) : bits(1) in
      let of = slice(dst, 15, 1) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val shr_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function shr_spec_32 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(33, negate(unsigned(src))) : sbits(33);
    let raw_result = the_bits(32, ash(unsigned(dst), signed(neg_src))) : bits(32);
    let result = n_size(32, raw_result) : bits(32);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec32(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec32(result) : bits(1) in
      let of = slice(dst, 31, 1) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val shr_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function shr_spec_64 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(65, negate(unsigned(src))) : sbits(65);
    let raw_result = the_bits(64, ash(unsigned(dst), signed(neg_src))) : bits(64);
    let result = n_size(64, raw_result) : bits(64);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec64(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec64(result) : bits(1) in
      let of = slice(dst, 63, 1) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val shr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function shr_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) = shr_spec_8(the_bits(8, dst), src, input_rflags) : (bits(8), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      2 => let (elem0, elem1, elem2) =
        shr_spec_16(the_bits(16, dst), src, input_rflags) : (bits(16), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      4 => let (elem0, elem1, elem2) =
        shr_spec_32(the_bits(32, dst), src, input_rflags) : (bits(32), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      8 => shr_spec_64(dst, src, input_rflags),
      _ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    }) : (bits(64), bits(32), bits(32))
}

val sar_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function sar_spec_8 (dst, src, input_rflags) = {
    let dst = n_size(8, dst) : bits(8);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(9, negate(unsigned(src))) : sbits(9);
    let raw_result_not_sign_extended = the_bits(8, ash(unsigned(dst), signed(neg_src))) : bits(8);
    let raw_result = (if the_bits(8, logbit(7, dst)) == get_slice_int(8, 1, 0) then {
        loghead(8, ash(binary_logext(8, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    }) : int;
    let result = n_size(8, raw_result) : bits(8);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec8(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec8(result) : bits(1) in
      let of = 0 : {|0|} in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec8(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec8(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sar_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function sar_spec_16 (dst, src, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(17, negate(unsigned(src))) : sbits(17);
    let raw_result_not_sign_extended = the_bits(16, ash(unsigned(dst), signed(neg_src))) : bits(16);
    let raw_result = (if the_bits(8, logbit(15, dst)) == get_slice_int(8, 1, 0) then {
        loghead(16, ash(binary_logext(16, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    }) : int;
    let result = n_size(16, raw_result) : bits(16);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec16(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec16(result) : bits(1) in
      let of = 0 : {|0|} in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec16(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec16(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sar_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function sar_spec_32 (dst, src, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(33, negate(unsigned(src))) : sbits(33);
    let raw_result_not_sign_extended = the_bits(32, ash(unsigned(dst), signed(neg_src))) : bits(32);
    let raw_result = (if the_bits(8, logbit(31, dst)) == get_slice_int(8, 1, 0) then {
        loghead(32, ash(binary_logext(32, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    }) : int;
    let result = n_size(32, raw_result) : bits(32);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec32(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec32(result) : bits(1) in
      let of = 0 : {|0|} in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec32(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec32(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sar_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sar_spec_64 (dst, src, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(6, src) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let neg_src = the_sbits(65, negate(unsigned(src))) : sbits(65);
    let raw_result_not_sign_extended = the_bits(64, ash(unsigned(dst), signed(neg_src))) : bits(64);
    let raw_result = (if the_bits(8, logbit(63, dst)) == get_slice_int(8, 1, 0) then {
        loghead(64, ash(binary_logext(64, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    }) : int;
    let result = n_size(64, raw_result) : bits(64);
    let (output_rflags, undefined_flags) = (match src {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let pf = pf_spec64(result) : bits(1) in
      let zf = zf_spec(unsigned(result)) : {|0, 1|} in
      let sf = sf_spec64(result) : bits(1) in
      let of = 0 : {|0|} in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), Some(get_slice_int(1, 1, 0)), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let cf = slice(dst, unsigned(src) - 1, 1) : bits(1);
          let pf = pf_spec64(result) : bits(1);
          let zf = zf_spec(unsigned(result)) : {|0, 1|};
          let sf = sf_spec64(result) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (result, output_rflags, undefined_flags)
}

val sar_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sar_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) = sar_spec_8(the_bits(8, dst), src, input_rflags) : (bits(8), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      2 => let (elem0, elem1, elem2) =
        sar_spec_16(the_bits(16, dst), src, input_rflags) : (bits(16), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      4 => let (elem0, elem1, elem2) =
        sar_spec_32(the_bits(32, dst), src, input_rflags) : (bits(32), bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2),
      8 => sar_spec_64(dst, src, input_rflags),
      _ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    }) : (bits(64), bits(32), bits(32))
}

val shld_spec_16 : (bits(16), bits(16), bits(6), bits(32)) -> (bits(16), bool, bits(32), bits(32))

function shld_spec_16 (dst, src, cnt, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(16, src) : bits(16);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let dst_src = logior(the_bits(32, ash(unsigned(dst), 16)), the_bits(32, src)) : bits(32);
    let output_dst = n_size(16, ash(unsigned(dst_src), unsigned(the_bits(16, cnt)))) : bits(16);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 15, 1) : bits(1) in
      let of = (if unsigned(logbit(15, dst)) == unsigned(logbit(15, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec16(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec16(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 16 then {
          let cf = slice(dst, 16 - unsigned(cnt), 1) : bits(1);
          let pf = pf_spec16(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec16(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shld_spec_32 : (bits(32), bits(32), bits(6), bits(32)) -> (bits(32), bool, bits(32), bits(32))

function shld_spec_32 (dst, src, cnt, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(32, src) : bits(32);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let dst_src = logior(the_bits(64, ash(unsigned(dst), 32)), the_bits(64, src)) : bits(64);
    let output_dst = n_size(32, ash(unsigned(dst_src), unsigned(the_bits(32, cnt)))) : bits(32);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 31, 1) : bits(1) in
      let of = (if unsigned(logbit(31, dst)) == unsigned(logbit(31, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec32(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec32(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 32 then {
          let cf = slice(dst, 32 - unsigned(cnt), 1) : bits(1);
          let pf = pf_spec32(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec32(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shld_spec_64 : (bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shld_spec_64 (dst, src, cnt, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(64, src) : bits(64);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let dst_src = logior(the_bits(128, ash(unsigned(dst), 64)), the_bits(128, src)) : bits(128);
    let output_dst = n_size(64, ash(unsigned(dst_src), unsigned(the_bits(64, cnt)))) : bits(64);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 63, 1) : bits(1) in
      let of = (if unsigned(logbit(63, dst)) == unsigned(logbit(63, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec64(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec64(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 64 then {
          let cf = slice(dst, 64 - unsigned(cnt), 1) : bits(1);
          let pf = pf_spec64(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec64(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shld_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shld_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => let (elem0, elem1, elem2, elem3) =
        shld_spec_16(the_bits(16, dst), the_bits(16, src), cnt, input_rflags) : (bits(16), bool, bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2, elem3),
      4 => let (elem0, elem1, elem2, elem3) =
        shld_spec_32(the_bits(32, dst), the_bits(32, src), cnt, input_rflags) : (bits(32), bool, bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2, elem3),
      8 => shld_spec_64(dst, src, cnt, input_rflags),
      _ => (get_slice_int(64, 0, 0), false, get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    }) : (bits(64), bool, bits(32), bits(32))
}

val shrd_spec_16 : (bits(16), bits(16), bits(6), bits(32)) -> (bits(16), bool, bits(32), bits(32))

function shrd_spec_16 (dst, src, cnt, input_rflags) = {
    let dst = n_size(16, dst) : bits(16);
    let src = n_size(16, src) : bits(16);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let src_dst = logior(the_bits(32, ash(unsigned(src), 16)), the_bits(32, dst)) : bits(32);
    let neg_cnt = the_sbits(17, negate(unsigned(cnt))) : sbits(17);
    let output_dst = n_size(16, ash(unsigned(src_dst), signed(neg_cnt))) : bits(16);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let of = (if unsigned(logbit(15, dst)) == unsigned(logbit(15, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec16(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec16(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 16 then {
          let cf = slice(dst, unsigned(cnt) - 1, 1) : bits(1);
          let pf = pf_spec16(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec16(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shrd_spec_32 : (bits(32), bits(32), bits(6), bits(32)) -> (bits(32), bool, bits(32), bits(32))

function shrd_spec_32 (dst, src, cnt, input_rflags) = {
    let dst = n_size(32, dst) : bits(32);
    let src = n_size(32, src) : bits(32);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let src_dst = logior(the_bits(64, ash(unsigned(src), 32)), the_bits(64, dst)) : bits(64);
    let neg_cnt = the_sbits(33, negate(unsigned(cnt))) : sbits(33);
    let output_dst = n_size(32, ash(unsigned(src_dst), signed(neg_cnt))) : bits(32);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let of = (if unsigned(logbit(31, dst)) == unsigned(logbit(31, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec32(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec32(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 32 then {
          let cf = slice(dst, unsigned(cnt) - 1, 1) : bits(1);
          let pf = pf_spec32(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec32(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shrd_spec_64 : (bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shrd_spec_64 (dst, src, cnt, input_rflags) = {
    let dst = n_size(64, dst) : bits(64);
    let src = n_size(64, src) : bits(64);
    let cnt = n_size(6, cnt) : bits(6);
    let input_rflags = n32(input_rflags) : bits(32);
    let src_dst = logior(the_bits(128, ash(unsigned(src), 64)), the_bits(128, dst)) : bits(128);
    let neg_cnt = the_sbits(65, negate(unsigned(cnt))) : sbits(65);
    let output_dst = n_size(64, ash(unsigned(src_dst), signed(neg_cnt))) : bits(64);
    let (output_rflags, undefined_flags) = (match cnt {
      0 => (input_rflags, get_slice_int(32, 0, 0)),
      1 => let cf = slice(dst, 0, 1) : bits(1) in
      let of = (if unsigned(logbit(63, dst)) == unsigned(logbit(63, output_dst)) then 0 else 1) : {|0, 1|} in
      let pf = pf_spec64(output_dst) : bits(1) in
      let zf = zf_spec(unsigned(output_dst)) : {|0, 1|} in
      let sf = sf_spec64(output_dst) : bits(1) in
      let output_rflags =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), Some(get_slice_int(1, of, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32)
      in
      let undefined_flags = set_rflagsbits_get_af(get_slice_int(1, 1, 0), get_slice_int(32, 0, 0)) : bits(32) in
        (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 64 then {
          let cf = slice(dst, unsigned(cnt) - 1, 1) : bits(1);
          let pf = pf_spec64(output_dst) : bits(1);
          let zf = zf_spec(unsigned(output_dst)) : {|0, 1|};
          let sf = sf_spec64(output_dst) : bits(1);
          let output_rflags = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(get_slice_int(1, zf, 0)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          let undefined_flags = change_rflagsbits(get_slice_int(32, 0, 0), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), Some(get_slice_int(1, 1, 0)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None()) : bits(32);
          (output_rflags, undefined_flags)
      } else {
          let output_rflags = input_rflags : bits(32);
          let undefined_flags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), set_rflagsbits_get_of(get_slice_int(1, 1, 0), set_rflagsbits_get_pf(get_slice_int(1, 1, 0), set_rflagsbits_get_zf(get_slice_int(1, 1, 0), set_rflagsbits_get_sf(get_slice_int(1, 1, 0), set_rflagsbits_get_af(get_slice_int(1, 1, 0), input_rflags)))))) : bits(32);
          (output_rflags, undefined_flags)
      }
    }) : (bits(32), bits(32)) : (bits(32), bits(32));
    let output_rflags = output_rflags : bits(32);
    let undefined_flags = undefined_flags : bits(32);
    let output_rflags = n32(output_rflags) : bits(32);
    let undefined_flags = n32(undefined_flags) : bits(32);
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shrd_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shrd_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => let (elem0, elem1, elem2, elem3) =
        shrd_spec_16(the_bits(16, dst), the_bits(16, src), cnt, input_rflags) : (bits(16), bool, bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2, elem3),
      4 => let (elem0, elem1, elem2, elem3) =
        shrd_spec_32(the_bits(32, dst), the_bits(32, src), cnt, input_rflags) : (bits(32), bool, bits(32), bits(32))
      in
        (the_bits(64, elem0), elem1, elem2, elem3),
      8 => shrd_spec_64(dst, src, cnt, input_rflags),
      _ => (get_slice_int(64, 0, 0), false, get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
    }) : (bits(64), bool, bits(32), bits(32))
}
