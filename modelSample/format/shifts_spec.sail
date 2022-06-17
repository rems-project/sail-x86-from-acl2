
val sal_shl_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function sal_shl_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 7, 1) in
      let pf : bits(1) = pf_spec8(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec8(result) in
      let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result))) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, 8 - unsigned(src), 1);
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function sal_shl_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 15, 1) in
      let pf : bits(1) = pf_spec16(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec16(result) in
      let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result))) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, 16 - unsigned(src), 1);
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function sal_shl_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 31, 1) in
      let pf : bits(1) = pf_spec32(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec32(result) in
      let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result))) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, 32 - unsigned(src), 1);
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sal_shl_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 63, 1) in
      let pf : bits(1) = pf_spec64(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec64(result) in
      let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result))) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, 64 - unsigned(src), 1);
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sal_shl_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        sal_shl_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        sal_shl_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        sal_shl_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => sal_shl_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val shr_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function shr_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(9) = bits_of_int(negate(unsigned(src)), 9);
    let raw_result : bits(8) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 8);
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec8(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec8(result) in
      let of : bits(1) = slice(dst, 7, 1) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val shr_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function shr_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(17) = bits_of_int(negate(unsigned(src)), 17);
    let raw_result : bits(16) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 16);
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec16(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec16(result) in
      let of : bits(1) = slice(dst, 15, 1) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val shr_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function shr_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(33) = bits_of_int(negate(unsigned(src)), 33);
    let raw_result : bits(32) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 32);
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec32(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec32(result) in
      let of : bits(1) = slice(dst, 31, 1) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val shr_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function shr_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(65) = bits_of_int(negate(unsigned(src)), 65);
    let raw_result : bits(64) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 64);
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec64(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec64(result) in
      let of : bits(1) = slice(dst, 63, 1) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(of), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val shr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function shr_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = shr_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        shr_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        shr_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => shr_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val sar_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function sar_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(9) = bits_of_int(negate(unsigned(src)), 9);
    let raw_result_not_sign_extended : bits(8) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 8);
    let raw_result : int = if logbit(7, dst) == 0b1 then {
        loghead(8, ash(binary_logext(8, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    };
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec8(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec8(result) in
      let of : {|0|} = 0 in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 8 <= unsigned(src) then {
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec8(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec8(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sar_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function sar_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(17) = bits_of_int(negate(unsigned(src)), 17);
    let raw_result_not_sign_extended : bits(16) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 16);
    let raw_result : int = if logbit(15, dst) == 0b1 then {
        loghead(16, ash(binary_logext(16, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    };
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec16(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec16(result) in
      let of : {|0|} = 0 in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 16 <= unsigned(src) then {
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec16(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec16(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sar_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function sar_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(33) = bits_of_int(negate(unsigned(src)), 33);
    let raw_result_not_sign_extended : bits(32) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 32);
    let raw_result : int = if logbit(31, dst) == 0b1 then {
        loghead(32, ash(binary_logext(32, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    };
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec32(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec32(result) in
      let of : {|0|} = 0 in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 32 <= unsigned(src) then {
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec32(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec32(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sar_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sar_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let neg_src : sbits(65) = bits_of_int(negate(unsigned(src)), 65);
    let raw_result_not_sign_extended : bits(64) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 64);
    let raw_result : int = if logbit(63, dst) == 0b1 then {
        loghead(64, ash(binary_logext(64, unsigned(dst)), signed(neg_src)))
    } else {
        unsigned(raw_result_not_sign_extended)
    };
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let pf : bits(1) = pf_spec64(result) in
      let zf : {|0, 1|} = zf_spec(unsigned(result)) in
      let sf : bits(1) = sf_spec64(result) in
      let of : {|0|} = 0 in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if 64 <= unsigned(src) then {
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, None(), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, Some(0b1), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let cf : bits(1) = slice(dst, unsigned(src) - 1, 1);
          let pf : bits(1) = pf_spec64(result);
          let zf : {|0, 1|} = zf_spec(unsigned(result));
          let sf : bits(1) = sf_spec64(result);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (result, output_rflags, undefined_flags)
}

val sar_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function sar_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = sar_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        sar_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        sar_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => sar_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val shld_spec_16 : (bits(16), bits(16), bits(6), bits(32)) -> (bits(16), bool, bits(32), bits(32))

function shld_spec_16 (dst, src, cnt, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(16) = 0x0010;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let dst_src : bits(32) = logior(bits_of_int(ash(unsigned(dst), 16), 32), sail_zero_extend(src, 32));
    let output_dst : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 15, 1) in
      let of : {|0, 1|} = if logbit(15, dst) == logbit(15, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec16(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec16(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 16 then {
          let cf : bits(1) = slice(dst, 16 - unsigned(cnt), 1);
          let pf : bits(1) = pf_spec16(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec16(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shld_spec_32 : (bits(32), bits(32), bits(6), bits(32)) -> (bits(32), bool, bits(32), bits(32))

function shld_spec_32 (dst, src, cnt, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(32) = 0x00000020;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let dst_src : bits(64) = logior(bits_of_int(ash(unsigned(dst), 32), 64), sail_zero_extend(src, 64));
    let output_dst : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 31, 1) in
      let of : {|0, 1|} = if logbit(31, dst) == logbit(31, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec32(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec32(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 32 then {
          let cf : bits(1) = slice(dst, 32 - unsigned(cnt), 1);
          let pf : bits(1) = pf_spec32(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec32(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shld_spec_64 : (bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shld_spec_64 (dst, src, cnt, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(64) = 0x0000000000000040;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let dst_src : bits(128) = logior(bits_of_int(ash(unsigned(dst), 64), 128), sail_zero_extend(src, 128));
    let output_dst : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 63, 1) in
      let of : {|0, 1|} = if logbit(63, dst) == logbit(63, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec64(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec64(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 64 then {
          let cf : bits(1) = slice(dst, 64 - unsigned(cnt), 1);
          let pf : bits(1) = pf_spec64(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec64(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shld_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shld_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => let (elem0, elem1, elem2, elem3) : (bits(16), bool, bits(32), bits(32)) =
        shld_spec_16(truncate(dst, 16), truncate(src, 16), cnt, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2, elem3),
      4 => let (elem0, elem1, elem2, elem3) : (bits(32), bool, bits(32), bits(32)) =
        shld_spec_32(truncate(dst, 32), truncate(src, 32), cnt, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2, elem3),
      8 => shld_spec_64(dst, src, cnt, input_rflags),
      _ => (0x0000000000000000, false, 0x00000000, 0x00000000)
    }) : (bits(64), bool, bits(32), bits(32))
}

val shrd_spec_16 : (bits(16), bits(16), bits(6), bits(32)) -> (bits(16), bool, bits(32), bits(32))

function shrd_spec_16 (dst, src, cnt, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(16) = 0x0010;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let src_dst : bits(32) = logior(bits_of_int(ash(unsigned(src), 16), 32), sail_zero_extend(dst, 32));
    let neg_cnt : sbits(17) = bits_of_int(negate(unsigned(cnt)), 17);
    let output_dst : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let of : {|0, 1|} = if logbit(15, dst) == logbit(15, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec16(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec16(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 16 then {
          let cf : bits(1) = slice(dst, unsigned(cnt) - 1, 1);
          let pf : bits(1) = pf_spec16(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec16(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shrd_spec_32 : (bits(32), bits(32), bits(6), bits(32)) -> (bits(32), bool, bits(32), bits(32))

function shrd_spec_32 (dst, src, cnt, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(32) = 0x00000020;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let src_dst : bits(64) = logior(bits_of_int(ash(unsigned(src), 32), 64), sail_zero_extend(dst, 64));
    let neg_cnt : sbits(33) = bits_of_int(negate(unsigned(cnt)), 33);
    let output_dst : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let of : {|0, 1|} = if logbit(31, dst) == logbit(31, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec32(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec32(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 32 then {
          let cf : bits(1) = slice(dst, unsigned(cnt) - 1, 1);
          let pf : bits(1) = pf_spec32(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec32(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shrd_spec_64 : (bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shrd_spec_64 (dst, src, cnt, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(64) = 0x0000000000000040;
    let cnt : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let src_dst : bits(128) = logior(bits_of_int(ash(unsigned(src), 64), 128), sail_zero_extend(dst, 128));
    let neg_cnt : sbits(65) = bits_of_int(negate(unsigned(cnt)), 65);
    let output_dst : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match cnt {
      0 => (input_rflags, 0x00000000),
      1 => let cf : bits(1) = slice(dst, 0, 1) in
      let of : {|0, 1|} = if logbit(63, dst) == logbit(63, output_dst) then 0 else 1 in
      let pf : bits(1) = pf_spec64(output_dst) in
      let zf : {|0, 1|} = zf_spec(unsigned(output_dst)) in
      let sf : bits(1) = sf_spec64(output_dst) in
      let output_rflags : bits(32) =
        change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
      in
      let undefined_flags : bits(32) = set_rflagsbits_get_af(0b1, 0x00000000) in (output_rflags, undefined_flags),
      _ => if unsigned(cnt) <= 64 then {
          let cf : bits(1) = slice(dst, unsigned(cnt) - 1, 1);
          let pf : bits(1) = pf_spec64(output_dst);
          let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
          let sf : bits(1) = sf_spec64(output_dst);
          let output_rflags : bits(32) = change_rflagsbits(input_rflags, Some(cf), None(), Some(pf), None(), None(), None(), Some(bits_of_int(zf, 1)), Some(sf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          let undefined_flags : bits(32) = change_rflagsbits(0x00000000, None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), Some(0b1), None(), None(), None(), None(), None(), None(), None(), None(), None(), None());
          (output_rflags, undefined_flags)
      } else {
          let output_rflags : bits(32) = input_rflags;
          let undefined_flags : bits(32) = set_rflagsbits_get_cf(0b1, set_rflagsbits_get_of(0b1, set_rflagsbits_get_pf(0b1, set_rflagsbits_get_zf(0b1, set_rflagsbits_get_sf(0b1, set_rflagsbits_get_af(0b1, input_rflags))))));
          (output_rflags, undefined_flags)
      }
    };
    let output_rflags : bits(32) = n32(output_rflags);
    let undefined_flags : bits(32) = n32(undefined_flags);
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shrd_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), bits(32)) -> (bits(64), bool, bits(32), bits(32))

function shrd_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => let (elem0, elem1, elem2, elem3) : (bits(16), bool, bits(32), bits(32)) =
        shrd_spec_16(truncate(dst, 16), truncate(src, 16), cnt, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2, elem3),
      4 => let (elem0, elem1, elem2, elem3) : (bits(32), bool, bits(32), bits(32)) =
        shrd_spec_32(truncate(dst, 32), truncate(src, 32), cnt, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2, elem3),
      8 => shrd_spec_64(dst, src, cnt, input_rflags),
      _ => (0x0000000000000000, false, 0x00000000, 0x00000000)
    }) : (bits(64), bool, bits(32), bits(32))
}
