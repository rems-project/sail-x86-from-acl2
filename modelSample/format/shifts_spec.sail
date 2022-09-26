
val sal_shl_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function sal_shl_spec_8 (dst, src, input_rflags) = {
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(8) = bits_of_int(raw_result, 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[7 .. 7];
              let pf : bits(1) = pf_spec8(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec8(result);
              let of : int = b_xor(unsigned(cf), unsigned(logbit(7, result)));
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 8 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, 8 - unsigned(src), 1);
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function sal_shl_spec_16 (dst, src, input_rflags) = {
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(16) = bits_of_int(raw_result, 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[15 .. 15];
              let pf : bits(1) = pf_spec16(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec16(result);
              let of : int = b_xor(unsigned(cf), unsigned(logbit(15, result)));
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 16 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, 16 - unsigned(src), 1);
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function sal_shl_spec_32 (dst, src, input_rflags) = {
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(32) = bits_of_int(raw_result, 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[31 .. 31];
              let pf : bits(1) = pf_spec32(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec32(result);
              let of : int = b_xor(unsigned(cf), unsigned(logbit(31, result)));
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 32 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, 32 - unsigned(src), 1);
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function sal_shl_spec_64 (dst, src, input_rflags) = {
    let raw_result : int = ash(unsigned(dst), unsigned(src));
    let result : bits(64) = bits_of_int(raw_result, 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[63 .. 63];
              let pf : bits(1) = pf_spec64(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec64(result);
              let of : int = b_xor(unsigned(cf), unsigned(logbit(63, result)));
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 64 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, 64 - unsigned(src), 1);
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sal_shl_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function sal_shl_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = sal_shl_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = sal_shl_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = sal_shl_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => sal_shl_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val shr_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function shr_spec_8 (dst, src, input_rflags) = {
    let neg_src : sbits(9) = bits_of_int(negate(unsigned(src)), 9);
    let raw_result : bits(8) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 8);
    let result : bits(8) = raw_result;
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec8(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec8(result);
              let of : bits(1) = dst[7 .. 7];
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 8 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val shr_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function shr_spec_16 (dst, src, input_rflags) = {
    let neg_src : sbits(17) = bits_of_int(negate(unsigned(src)), 17);
    let raw_result : bits(16) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 16);
    let result : bits(16) = raw_result;
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec16(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec16(result);
              let of : bits(1) = dst[15 .. 15];
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 16 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val shr_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function shr_spec_32 (dst, src, input_rflags) = {
    let neg_src : sbits(33) = bits_of_int(negate(unsigned(src)), 33);
    let raw_result : bits(32) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 32);
    let result : bits(32) = raw_result;
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec32(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec32(result);
              let of : bits(1) = dst[31 .. 31];
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 32 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val shr_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function shr_spec_64 (dst, src, input_rflags) = {
    let neg_src : sbits(65) = bits_of_int(negate(unsigned(src)), 65);
    let raw_result : bits(64) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 64);
    let result : bits(64) = raw_result;
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec64(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec64(result);
              let of : bits(1) = dst[63 .. 63];
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 64 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val shr_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function shr_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = shr_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = shr_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = shr_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => shr_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val sar_spec_8 : (bits(8), bits(6), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function sar_spec_8 (dst, src, input_rflags) = {
    let neg_src : sbits(9) = bits_of_int(negate(unsigned(src)), 9);
    let raw_result_not_sign_extended : bits(8) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 8);
    let raw_result : int = {
        if logbit(7, dst) == 0b1 then {
            loghead(8, ash(binary_logext(8, unsigned(dst)), signed(neg_src)))
        } else {
            unsigned(raw_result_not_sign_extended)
        }
    };
    let result : bits(8) = bits_of_int(raw_result, 8);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec8(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec8(result);
              let of : {|0|} = 0;
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 8 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec8(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec8(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sar_spec_16 : (bits(16), bits(6), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function sar_spec_16 (dst, src, input_rflags) = {
    let neg_src : sbits(17) = bits_of_int(negate(unsigned(src)), 17);
    let raw_result_not_sign_extended : bits(16) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 16);
    let raw_result : int = {
        if logbit(15, dst) == 0b1 then {
            loghead(16, ash(binary_logext(16, unsigned(dst)), signed(neg_src)))
        } else {
            unsigned(raw_result_not_sign_extended)
        }
    };
    let result : bits(16) = bits_of_int(raw_result, 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec16(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec16(result);
              let of : {|0|} = 0;
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 16 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec16(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec16(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sar_spec_32 : (bits(32), bits(6), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function sar_spec_32 (dst, src, input_rflags) = {
    let neg_src : sbits(33) = bits_of_int(negate(unsigned(src)), 33);
    let raw_result_not_sign_extended : bits(32) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 32);
    let raw_result : int = {
        if logbit(31, dst) == 0b1 then {
            loghead(32, ash(binary_logext(32, unsigned(dst)), signed(neg_src)))
        } else {
            unsigned(raw_result_not_sign_extended)
        }
    };
    let result : bits(32) = bits_of_int(raw_result, 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec32(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec32(result);
              let of : {|0|} = 0;
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 32 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec32(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec32(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sar_spec_64 : (bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function sar_spec_64 (dst, src, input_rflags) = {
    let neg_src : sbits(65) = bits_of_int(negate(unsigned(src)), 65);
    let raw_result_not_sign_extended : bits(64) = bits_of_int(ash(unsigned(dst), signed(neg_src)), 64);
    let raw_result : int = {
        if logbit(63, dst) == 0b1 then {
            loghead(64, ash(binary_logext(64, unsigned(dst)), signed(neg_src)))
        } else {
            unsigned(raw_result_not_sign_extended)
        }
    };
    let result : bits(64) = bits_of_int(raw_result, 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match src {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let pf : bits(1) = pf_spec64(result);
              let zf : {|0, 1|} = zf_spec(unsigned(result));
              let sf : bits(1) = sf_spec64(result);
              let of : {|0|} = 0;
              let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf, of = bits_of_int(of, 1)];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if 64 <= unsigned(src) then {
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with cf = 0b1, af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let cf : bits(1) = bitslice(dst, unsigned(src) - 1, 1);
                  let pf : bits(1) = pf_spec64(result);
                  let zf : {|0, 1|} = zf_spec(unsigned(result));
                  let sf : bits(1) = sf_spec64(result);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (result, output_rflags, undefined_flags)
}

val sar_spec : ({|1, 2, 4, 8|}, bits(64), bits(6), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function sar_spec (size, dst, src, input_rflags) = {
    (match size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = sar_spec_8(truncate(dst, 8), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = sar_spec_16(truncate(dst, 16), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = sar_spec_32(truncate(dst, 32), src, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      8 => sar_spec_64(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val shld_spec_16 : (bits(16), bits(16), bits(6), rflagsbits) -> (bits(16), bool, rflagsbits, rflagsbits)

function shld_spec_16 (dst, src, cnt, input_rflags) = {
    let dst_src : bits(32) = logior(bits_of_int(ash(unsigned(dst), 16), 32), sail_zero_extend(src, 32));
    let output_dst : bits(16) = bits_of_int(ash(unsigned(dst_src), unsigned(sail_zero_extend(cnt, 16))), 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[15 .. 15];
              let of : {|0, 1|} = {
                  if logbit(15, dst) == logbit(15, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec16(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec16(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 16 then {
                  let cf : bits(1) = bitslice(dst, 16 - unsigned(cnt), 1);
                  let pf : bits(1) = pf_spec16(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec16(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shld_spec_32 : (bits(32), bits(32), bits(6), rflagsbits) -> (bits(32), bool, rflagsbits, rflagsbits)

function shld_spec_32 (dst, src, cnt, input_rflags) = {
    let dst_src : bits(64) = logior(bits_of_int(ash(unsigned(dst), 32), 64), sail_zero_extend(src, 64));
    let output_dst : bits(32) = bits_of_int(ash(unsigned(dst_src), unsigned(sail_zero_extend(cnt, 32))), 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[31 .. 31];
              let of : {|0, 1|} = {
                  if logbit(31, dst) == logbit(31, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec32(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec32(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 32 then {
                  let cf : bits(1) = bitslice(dst, 32 - unsigned(cnt), 1);
                  let pf : bits(1) = pf_spec32(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec32(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shld_spec_64 : (bits(64), bits(64), bits(6), rflagsbits) -> (bits(64), bool, rflagsbits, rflagsbits)

function shld_spec_64 (dst, src, cnt, input_rflags) = {
    let dst_src : bits(128) = logior(bits_of_int(ash(unsigned(dst), 64), 128), sail_zero_extend(src, 128));
    let output_dst : bits(64) = bits_of_int(ash(unsigned(dst_src), unsigned(sail_zero_extend(cnt, 64))), 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[63 .. 63];
              let of : {|0, 1|} = {
                  if logbit(63, dst) == logbit(63, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec64(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec64(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 64 then {
                  let cf : bits(1) = bitslice(dst, 64 - unsigned(cnt), 1);
                  let pf : bits(1) = pf_spec64(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec64(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shld_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), rflagsbits) -> (bits(64), bool, rflagsbits, rflagsbits)

function shld_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => {
          let (elem0, elem1, elem2, elem3) : (bits(16), bool, rflagsbits, rflagsbits) = shld_spec_16(truncate(dst, 16), truncate(src, 16), cnt, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2, elem3)
      },
      4 => {
          let (elem0, elem1, elem2, elem3) : (bits(32), bool, rflagsbits, rflagsbits) = shld_spec_32(truncate(dst, 32), truncate(src, 32), cnt, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2, elem3)
      },
      8 => shld_spec_64(dst, src, cnt, input_rflags),
      _ => (0x0000000000000000, false, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), bool, rflagsbits, rflagsbits)
}

val shrd_spec_16 : (bits(16), bits(16), bits(6), rflagsbits) -> (bits(16), bool, rflagsbits, rflagsbits)

function shrd_spec_16 (dst, src, cnt, input_rflags) = {
    let src_dst : bits(32) = logior(bits_of_int(ash(unsigned(src), 16), 32), sail_zero_extend(dst, 32));
    let neg_cnt : sbits(17) = bits_of_int(negate(unsigned(cnt)), 17);
    let output_dst : bits(16) = bits_of_int(ash(unsigned(src_dst), signed(neg_cnt)), 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let of : {|0, 1|} = {
                  if logbit(15, dst) == logbit(15, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec16(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec16(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 16 then {
                  let cf : bits(1) = bitslice(dst, unsigned(cnt) - 1, 1);
                  let pf : bits(1) = pf_spec16(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec16(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 16, output_rflags, undefined_flags)
}

val shrd_spec_32 : (bits(32), bits(32), bits(6), rflagsbits) -> (bits(32), bool, rflagsbits, rflagsbits)

function shrd_spec_32 (dst, src, cnt, input_rflags) = {
    let src_dst : bits(64) = logior(bits_of_int(ash(unsigned(src), 32), 64), sail_zero_extend(dst, 64));
    let neg_cnt : sbits(33) = bits_of_int(negate(unsigned(cnt)), 33);
    let output_dst : bits(32) = bits_of_int(ash(unsigned(src_dst), signed(neg_cnt)), 32);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let of : {|0, 1|} = {
                  if logbit(31, dst) == logbit(31, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec32(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec32(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 32 then {
                  let cf : bits(1) = bitslice(dst, unsigned(cnt) - 1, 1);
                  let pf : bits(1) = pf_spec32(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec32(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 32, output_rflags, undefined_flags)
}

val shrd_spec_64 : (bits(64), bits(64), bits(6), rflagsbits) -> (bits(64), bool, rflagsbits, rflagsbits)

function shrd_spec_64 (dst, src, cnt, input_rflags) = {
    let src_dst : bits(128) = logior(bits_of_int(ash(unsigned(src), 64), 128), sail_zero_extend(dst, 128));
    let neg_cnt : sbits(65) = bits_of_int(negate(unsigned(cnt)), 65);
    let output_dst : bits(64) = bits_of_int(ash(unsigned(src_dst), signed(neg_cnt)), 64);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[0 .. 0];
              let of : {|0, 1|} = {
                  if logbit(63, dst) == logbit(63, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec64(output_dst);
              let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec64(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf = bits_of_int(zf, 1), sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 64 then {
                  let cf : bits(1) = bitslice(dst, unsigned(cnt) - 1, 1);
                  let pf : bits(1) = pf_spec64(output_dst);
                  let zf : {|0, 1|} = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec64(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf = bits_of_int(zf, 1), sf];
                  let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1, of = 0b1];
                  (output_rflags, undefined_flags)
              } else {
                  let output_rflags : rflagsbits = input_rflags;
                  let undefined_flags : rflagsbits = [input_rflags with af = 0b1, sf = 0b1, zf = 0b1, pf = 0b1, of = 0b1, cf = 0b1];
                  (output_rflags, undefined_flags)
              }
          }
        }
    };
    (output_dst, unsigned(cnt) > 64, output_rflags, undefined_flags)
}

val shrd_spec : ({|2, 4, 8|}, bits(64), bits(64), bits(6), rflagsbits) -> (bits(64), bool, rflagsbits, rflagsbits)

function shrd_spec (size, dst, src, cnt, input_rflags) = {
    (match size {
      2 => {
          let (elem0, elem1, elem2, elem3) : (bits(16), bool, rflagsbits, rflagsbits) = shrd_spec_16(truncate(dst, 16), truncate(src, 16), cnt, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2, elem3)
      },
      4 => {
          let (elem0, elem1, elem2, elem3) : (bits(32), bool, rflagsbits, rflagsbits) = shrd_spec_32(truncate(dst, 32), truncate(src, 32), cnt, input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2, elem3)
      },
      8 => shrd_spec_64(dst, src, cnt, input_rflags),
      _ => (0x0000000000000000, false, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), bool, rflagsbits, rflagsbits)
}
