
val rcl_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function rcl_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(9) = changeSlice(sail_zero_extend(dst, 9), 8, 1, old_cf);
    let raw_result : bits(9) = bits_of_int(rotate_left(unsigned(new_dst), 9, unsigned(src)), 9);
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(8, raw_result) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(8, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rcl_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function rcl_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(17) = changeSlice(sail_zero_extend(dst, 17), 16, 1, old_cf);
    let raw_result : bits(17) = bits_of_int(rotate_left(unsigned(new_dst), 17, unsigned(src)), 17);
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(16, raw_result) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(16, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rcl_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function rcl_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(33) = changeSlice(sail_zero_extend(dst, 33), 32, 1, old_cf);
    let raw_result : bits(33) = bits_of_int(rotate_left(unsigned(new_dst), 33, unsigned(src)), 33);
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(32, raw_result) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(32, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rcl_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rcl_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(65) = changeSlice(sail_zero_extend(dst, 65), 64, 1, old_cf);
    let raw_result : bits(65) = bits_of_int(rotate_left(unsigned(new_dst), 65, unsigned(src)), 65);
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(64, raw_result) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(64, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rcl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rcl_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = rcl_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        rcl_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        rcl_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => rcl_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val rol_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function rol_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = slice(result, 0, 1) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rol_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function rol_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = slice(result, 0, 1) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rol_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function rol_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = slice(result, 0, 1) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rol_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rol_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = slice(result, 0, 1) in
        let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val rol_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rol_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = rol_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        rol_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        rol_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => rol_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val rcr_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function rcr_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(9) = changeSlice(sail_zero_extend(dst, 9), 8, 1, input_cf);
    let raw_result : bits(9) = bits_of_int(rotate_right(unsigned(new_dst), 9, unsigned(src)), 9);
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(8, raw_result) in
        let of : int = b_xor(unsigned(logbit(7, result)), unsigned(slice(result, 6, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(8, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function rcr_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(17) = changeSlice(sail_zero_extend(dst, 17), 16, 1, input_cf);
    let raw_result : bits(17) = bits_of_int(rotate_right(unsigned(new_dst), 17, unsigned(src)), 17);
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(16, raw_result) in
        let of : int = b_xor(unsigned(logbit(15, result)), unsigned(slice(result, 14, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(16, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function rcr_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(33) = changeSlice(sail_zero_extend(dst, 33), 32, 1, input_cf);
    let raw_result : bits(33) = bits_of_int(rotate_right(unsigned(new_dst), 33, unsigned(src)), 33);
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(32, raw_result) in
        let of : int = b_xor(unsigned(logbit(31, result)), unsigned(slice(result, 30, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(32, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rcr_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let input_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let new_dst : bits(65) = changeSlice(sail_zero_extend(dst, 65), 64, 1, input_cf);
    let raw_result : bits(65) = bits_of_int(rotate_right(unsigned(new_dst), 65, unsigned(src)), 65);
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => (input_rflags, 0x00000000),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(64, raw_result) in
        let of : int = b_xor(unsigned(logbit(63, result)), unsigned(slice(result, 62, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = logbit(64, raw_result) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function rcr_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = rcr_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        rcr_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        rcr_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => rcr_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}

val ror_spec_8 : (bits(8), bits(6), bits(32)) -> (bits(8), bits(32), bits(32))

function ror_spec_8 (dst, src, input_rflags) = {
    let dst : bits(8) = 0x08;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(8) = 0x08;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(7, result) in
        let of : int = b_xor(unsigned(logbit(7, result)), unsigned(slice(result, 6, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val ror_spec_16 : (bits(16), bits(6), bits(32)) -> (bits(16), bits(32), bits(32))

function ror_spec_16 (dst, src, input_rflags) = {
    let dst : bits(16) = 0x0010;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(16) = 0x0010;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(15, result) in
        let of : int = b_xor(unsigned(logbit(15, result)), unsigned(slice(result, 14, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val ror_spec_32 : (bits(32), bits(6), bits(32)) -> (bits(32), bits(32), bits(32))

function ror_spec_32 (dst, src, input_rflags) = {
    let dst : bits(32) = 0x00000020;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(32) = 0x00000020;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(31, result) in
        let of : int = b_xor(unsigned(logbit(31, result)), unsigned(slice(result, 30, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val ror_spec_64 : (bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function ror_spec_64 (dst, src, input_rflags) = {
    let dst : bits(64) = 0x0000000000000040;
    let src : bits(6) = 0b000110;
    let input_rflags : bits(32) = n32(input_rflags);
    let result : bits(64) = 0x0000000000000040;
    let (output_rflags, undefined_flags) : (bits(32), bits(32)) = match src {
      0 => let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (input_rflags, undefined_flags),
      1 => let (elem0, elem1) : (bits(32), {|0|}) =
        let cf : bits(1) = logbit(63, result) in
        let of : int = b_xor(unsigned(logbit(63, result)), unsigned(slice(result, 62, 1))) in
        let output_rflags : bits(32) =
          change_rflagsbits(input_rflags, Some(cf), None(), None(), None(), None(), None(), None(), None(), None(), None(), None(), Some(bits_of_int(of, 1)), None(), None(), None(), None(), None(), None(), None(), None(), None(), None())
        in
          (output_rflags, 0)
      in
        (elem0, bits_of_int(elem1, 32)),
      _ => let cf : bits(1) = slice(result, 0, 1) in
      let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, input_rflags) in
      let undefined_flags : bits(32) = set_rflagsbits_get_of(0b1, 0x00000000) in (output_rflags, undefined_flags)
    };
    let output_rflags : bits(32) = n32(output_rflags);
    (result, output_rflags, undefined_flags)
}

val ror_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), bits(32)) -> (bits(64), bits(32), bits(32))

function ror_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) = ror_spec_8(truncate(dst, 8), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        ror_spec_16(truncate(dst, 16), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        ror_spec_32(truncate(dst, 32), src, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      8 => ror_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, 0x00000000, 0x00000000)
    }) : (bits(64), bits(32), bits(32))
}
