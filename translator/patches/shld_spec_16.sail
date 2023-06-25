
val shld_spec_16 : (bits(16), bits(16), bits(6), rflagsbits) -> (bits(16), bool, rflagsbits, rflagsbits)

function shld_spec_16 (dst, src, cnt, input_rflags) = {
    let dst_src : bits(32) = logior(bits_of_int(ash(unsigned(dst), 16), 32), sail_zero_extend(src, 32));
    let output_dst : bits(16) = bits_of_int(ash(unsigned(dst_src), unsigned(sail_zero_extend(cnt, 16)) - 16), 16);
    let (output_rflags, undefined_flags) : (rflagsbits, rflagsbits) = {
        match cnt {
          0 => (input_rflags, Mk_rflagsbits(0x00000000)),
          1 => {
              let cf : bits(1) = dst[15 .. 15];
              let of : {|0, 1|} = {
                  if logbit(15, dst) == logbit(15, output_dst) then 0 else 1
              };
              let pf : bits(1) = pf_spec16(output_dst);
              let zf : bits(1) = zf_spec(unsigned(output_dst));
              let sf : bits(1) = sf_spec16(output_dst);
              let output_rflags : rflagsbits = [input_rflags with cf, of = bits_of_int(of, 1), pf, zf, sf];
              let undefined_flags : rflagsbits = [Mk_rflagsbits(0x00000000) with af = 0b1];
              (output_rflags, undefined_flags)
          },
          _ => {
              if unsigned(cnt) <= 16 then {
                  let cf : bits(1) = bitslice(dst, 16 - unsigned(cnt), 1);
                  let pf : bits(1) = pf_spec16(output_dst);
                  let zf : bits(1) = zf_spec(unsigned(output_dst));
                  let sf : bits(1) = sf_spec16(output_dst);
                  let output_rflags : rflagsbits = [input_rflags with cf, pf, zf, sf];
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
