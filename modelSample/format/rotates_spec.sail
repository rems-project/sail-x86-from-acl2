
val rcl_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function rcl_spec_8 (dst, src, input_rflags) = {
    let old_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(9) = changeSlice(sail_zero_extend(dst, 9), 8, 1, old_cf);
    let raw_result : bits(9) = bits_of_int(rotate_left(unsigned(new_dst), 9, unsigned(src)), 9);
    let result : bits(8) = truncate(raw_result, 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(8, raw_result);
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(8, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcl_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function rcl_spec_16 (dst, src, input_rflags) = {
    let old_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(17) = changeSlice(sail_zero_extend(dst, 17), 16, 1, old_cf);
    let raw_result : bits(17) = bits_of_int(rotate_left(unsigned(new_dst), 17, unsigned(src)), 17);
    let result : bits(16) = truncate(raw_result, 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(16, raw_result);
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(16, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcl_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function rcl_spec_32 (dst, src, input_rflags) = {
    let old_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(33) = changeSlice(sail_zero_extend(dst, 33), 32, 1, old_cf);
    let raw_result : bits(33) = bits_of_int(rotate_left(unsigned(new_dst), 33, unsigned(src)), 33);
    let result : bits(32) = truncate(raw_result, 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(32, raw_result);
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(32, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcl_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rcl_spec_64 (dst, src, input_rflags) = {
    let old_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(65) = changeSlice(sail_zero_extend(dst, 65), 64, 1, old_cf);
    let raw_result : bits(65) = bits_of_int(rotate_left(unsigned(new_dst), 65, unsigned(src)), 65);
    let result : bits(64) = truncate(raw_result, 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(64, raw_result);
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(64, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rcl_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = rcl_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = rcl_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = rcl_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => rcl_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val rol_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function rol_spec_8 (dst, src, input_rflags) = {
    let result : bits(8) = truncate(bits_of_int(rotate_left(unsigned(dst), 8, unsigned(src)), 9), 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = result[0 .. 0];
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rol_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function rol_spec_16 (dst, src, input_rflags) = {
    let result : bits(16) = truncate(bits_of_int(rotate_left(unsigned(dst), 16, unsigned(src)), 17), 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = result[0 .. 0];
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rol_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function rol_spec_32 (dst, src, input_rflags) = {
    let result : bits(32) = truncate(bits_of_int(rotate_left(unsigned(dst), 32, unsigned(src)), 33), 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = result[0 .. 0];
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rol_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rol_spec_64 (dst, src, input_rflags) = {
    let result : bits(64) = truncate(bits_of_int(rotate_left(unsigned(dst), 64, unsigned(src)), 65), 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = result[0 .. 0];
                  let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result)));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rol_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rol_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = rol_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = rol_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = rol_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => rol_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val rcr_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function rcr_spec_8 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(9) = changeSlice(sail_zero_extend(dst, 9), 8, 1, input_cf);
    let raw_result : bits(9) = bits_of_int(rotate_right(unsigned(new_dst), 9, unsigned(src)), 9);
    let result : bits(8) = truncate(raw_result, 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(8, raw_result);
                  let of : int = b_xor(unsigned(logbit(7, result)), unsigned(result[6 .. 6]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(8, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function rcr_spec_16 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(17) = changeSlice(sail_zero_extend(dst, 17), 16, 1, input_cf);
    let raw_result : bits(17) = bits_of_int(rotate_right(unsigned(new_dst), 17, unsigned(src)), 17);
    let result : bits(16) = truncate(raw_result, 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(16, raw_result);
                  let of : int = b_xor(unsigned(logbit(15, result)), unsigned(result[14 .. 14]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(16, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function rcr_spec_32 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(33) = changeSlice(sail_zero_extend(dst, 33), 32, 1, input_cf);
    let raw_result : bits(33) = bits_of_int(rotate_right(unsigned(new_dst), 33, unsigned(src)), 33);
    let result : bits(32) = truncate(raw_result, 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(32, raw_result);
                  let of : int = b_xor(unsigned(logbit(31, result)), unsigned(result[30 .. 30]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(32, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rcr_spec_64 (dst, src, input_rflags) = {
    let input_cf : bits(1) = input_rflags[cf];
    let new_dst : bits(65) = changeSlice(sail_zero_extend(dst, 65), 64, 1, input_cf);
    let raw_result : bits(65) = bits_of_int(rotate_right(unsigned(new_dst), 65, unsigned(src)), 65);
    let result : bits(64) = truncate(raw_result, 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(64, raw_result);
                  let of : int = b_xor(unsigned(logbit(63, result)), unsigned(result[62 .. 62]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = logbit(64, raw_result);
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val rcr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function rcr_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = rcr_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = rcr_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = rcr_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => rcr_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val ror_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function ror_spec_8 (dst, src, input_rflags) = {
    let result : bits(8) = bits_of_int(rotate_right(unsigned(dst), 8, unsigned(src)), 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(7, result);
                  let of : int = b_xor(unsigned(logbit(7, result)), unsigned(result[6 .. 6]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val ror_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function ror_spec_16 (dst, src, input_rflags) = {
    let result : bits(16) = bits_of_int(rotate_right(unsigned(dst), 16, unsigned(src)), 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(15, result);
                  let of : int = b_xor(unsigned(logbit(15, result)), unsigned(result[14 .. 14]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val ror_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function ror_spec_32 (dst, src, input_rflags) = {
    let result : bits(32) = bits_of_int(rotate_right(unsigned(dst), 32, unsigned(src)), 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(31, result);
                  let of : int = b_xor(unsigned(logbit(31, result)), unsigned(result[30 .. 30]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val ror_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function ror_spec_64 (dst, src, input_rflags) = {
    let result : bits(64) = bits_of_int(rotate_right(unsigned(dst), 64, unsigned(src)), 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => {
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (input_rflags, undefined_flags)
          },
          1 => {
              let (elem0, elem1) : (rflagsbits, {|0|}) = {
                  let cf : bits(1) = logbit(63, result);
                  let of : int = b_xor(unsigned(logbit(63, result)), unsigned(result[62 .. 62]));
                  let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1)];
                  (output_rflags, 0)
              };
              (elem0, Mk_rflagsbits(bits_of_int(elem1, 32)))
          },
          _ => {
              let cf : bits(1) = result[0 .. 0];
              let output_rflags : rflagsbits = [input_rflags with cf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with of = 0b1];
              (output_rflags, undefined_flags)
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val ror_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function ror_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = ror_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = ror_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = ror_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => ror_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}
