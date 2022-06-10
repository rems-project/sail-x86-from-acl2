$include "./other_non_det.sail"

val read_iptr : range(0, 4) -> sbits(50)

function read_iptr proc_mode = {
    let iptr = the_sbits(48, rip) : sbits(48);
    (match proc_mode {
      0 => the_sbits(50, iptr),
      1 => the_bits(50, let cs_attr = seg_hidden_attrs[1] : bits(16) in
      let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : bits(1) in
        if unsigned(cs_d) == 1 then {
            n32(the_bits(32, iptr))
        } else {
            the_bits(32, n16(the_bits(16, iptr)))
        }),
      _ => get_slice_int(50, 0, 0)
    }) : sbits(50)
}

val add_to_iptr : (range(0, 4), sbits(48), sbits(48)) -> (option(string), sbits(52))

function add_to_iptr (proc_mode, iptr, delta) = {
    let iptr_plus_delta = the_sbits(49, signed(iptr) + signed(delta)) : sbits(49);
    (match proc_mode {
      0 => let (elem0, elem1) =
        (if canonical_address_p(the_sbits(48, iptr_plus_delta)) then {
            (None() : option(string), the_sbits(50, iptr_plus_delta))
        } else {
            (Some(":NON-CANONICAL-INSTRUCTION-POINTER"), get_slice_int(50, 0, 0))
        }) : (option(string), sbits(50))
      in
        (elem0, the_sbits(52, elem1)),
      1 => let (elem0, elem1) =
        (let cs_limit = the_bits(32, loghead(32, unsigned(seg_hidden_limits[1]))) : bits(32) in
          if 0 <= signed(iptr_plus_delta) & signed(iptr_plus_delta) <= unsigned(cs_limit) then {
              (None() : option(string), the_bits(32, iptr_plus_delta))
          } else {
              (Some(":OUT-OF-SEGMENT-INSTRUCTION-POINTER"), get_slice_int(32, 0, 0))
          }) : (option(string), bits(32))
      in
        (elem0, the_bits(52, elem1)),
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), get_slice_int(52, 0, 0))
    }) : (option(string), sbits(52))
}

val write_iptr : (range(0, 4), sbits(48)) -> unit

function write_iptr (proc_mode, iptr) = {
    (match proc_mode {
      0 => rip = the_bits(64, iptr),
      1 => let cs_attr = seg_hidden_attrs[1] : bits(16) in
      let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : bits(1) in
        if unsigned(cs_d) == 1 then {
            rip = the_bits(64, n32(the_bits(32, iptr)))
        } else {
            let rip_var = the_sbits(48, rip) : sbits(48);
            let rip_new = changeSlice(rip_var, 0, 16, n16(the_bits(16, iptr))) : sbits(48);
            rip = the_bits(64, rip_new)
        },
      _ => ()
    }) : unit
}

val read_sptr : range(0, 4) -> bits(64)

function read_sptr proc_mode = {
    let sptr = rgfi(4) : bits(64);
    (match proc_mode {
      0 => sptr,
      1 => the_bits(64, let ss_attr = seg_hidden_attrs[2] : bits(16) in
      let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : bits(1) in
        if unsigned(ss_b) == 1 then {
            n32(the_bits(32, sptr))
        } else {
            the_bits(32, n16(the_bits(16, sptr)))
        }),
      _ => get_slice_int(64, 0, 0)
    }) : bits(64)
}

val add_to_sptr : (range(0, 4), sbits(64), sbits(64)) -> (option(string), sbits(67))

function add_to_sptr (proc_mode, sptr, delta) = {
    let sptr_plus_delta = the_sbits(65, signed(sptr) + signed(delta)) : sbits(65);
    (match proc_mode {
      0 => let (elem0, elem1) =
        (let sptr_plus_delta = I64(the_bits(64, sptr_plus_delta)) : sbits(64) in
          if canonical_address_p(the_sbits(48, sptr_plus_delta)) then {
              (None() : option(string), the_sbits(65, sptr_plus_delta))
          } else {
              (Some(":NON-CANONICAL-STACK-ADDRESS"), get_slice_int(65, 0, 0))
          }) : (option(string), sbits(65))
      in
        (elem0, the_sbits(67, elem1)),
      1 => let (elem0, elem1) =
        (let ss_limit = seg_hidden_limits[2] : bits(32) in
        let ss_attr = seg_hidden_attrs[2] : bits(16) in
        let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : bits(1) in
        let ss_e = data_segment_descriptor_attributesbits_get_e(ss_attr) : bits(1) in
        let ss_lower =
          (if unsigned(ss_e) == 1 then {
              1 + unsigned(ss_limit)
          } else 0) : int
        in
        let ss_upper =
          (if unsigned(ss_e) == 1 then {
              get_slice_int(32, if unsigned(ss_b) == 1 then 4294967295 else 65535, 0)
          } else {
              ss_limit
          }) : bits(32)
        in
        let sptr_plus_delta =
          (if unsigned(ss_b) == 1 then {
              n32(the_bits(32, sptr_plus_delta))
          } else {
              the_bits(32, n16(the_bits(16, sptr_plus_delta)))
          }) : bits(32)
        in
          if not_bool(ss_lower <= unsigned(sptr_plus_delta) & unsigned(sptr_plus_delta) <= unsigned(ss_upper)) then {
              (Some(":OUT-OF-SEGMENT-STACK-ADDRESS"), get_slice_int(32, 0, 0))
          } else {
              (None() : option(string), sptr_plus_delta)
          }) : (option(string), bits(32))
      in
        (elem0, the_bits(67, elem1)),
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), get_slice_int(67, 0, 0))
    }) : (option(string), sbits(67))
}

val write_sptr : (range(0, 4), sbits(64)) -> unit

function write_sptr (proc_mode, sptr) = {
    (match proc_mode {
      0 => write_rgfi(4, sptr),
      1 => let ss_attr = seg_hidden_attrs[2] : bits(16) in
      let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : bits(1) in
        if unsigned(ss_b) == 1 then {
            write_rgfi(4, the_bits(64, n32(the_bits(32, sptr))))
        } else {
            let rsp_var = rgfi(4) : bits(64);
            let rsp_new = changeSlice(rsp_var, 0, 16, n16(the_bits(16, sptr))) : bits(64);
            write_rgfi(4, rsp_new)
        },
      _ => ()
    }) : unit
}

val select_address_size : (range(0, 4), bool) -> {|8, 2, 4|}

function select_address_size (proc_mode, p4?) = {
    (match proc_mode {
      0 => if p4? then 4 else 8,
      _ => let cs_attr = seg_hidden_attrs[1] : bits(16) in
      let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : bits(1) in
        if unsigned(cs_d) == 1 then {
            if p4? then 2 else 4
        } else if p4? then 4 else 2
    }) : {|8, 2, 4|}
}

val x86_effective_addr_from_sib : (range(0, 4), sbits(48), bits(8), bits(2), bits(8)) -> (option(string), int, sbits(71), {|0, 1, 4|})

function x86_effective_addr_from_sib (proc_mode, temp_rip, rex_byte, mod_var, sib) = {
    let b = sib_get_base(sib) : bits(3);
    let check_alignment? = false : bool;
    let (flg, base, displacement, nrip_bytes) = (match mod_var {
      0 => let (elem0, elem1, elem2, elem3) =
        (if the_bits(8, b) == get_slice_int(8, 5, 0) then {
            let (elem0, elem1, elem2, elem3) = (let (flg0, dword) =
              rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct { mem_ptr? = false }) : (option(string), sbits(68))
            in
              if is_some(flg0) then {
                  (flg0, 0 : {|0|}, get_slice_int(69, 0, 0), 0 : {|0|})
              } else {
                  (None() : option(string), 0 : {|0|}, the_sbits(69, dword), 4 : {|4|})
              }) : (option(string), {|0|}, sbits(69), {|0, 4|});
            (elem0, get_slice_int(64, elem1, 0), the_sbits(70, elem2), elem3)
        } else {
            (None() : option(string), if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
            } else {
                the_bits(64, rr32(the_bits(4, b)))
            }, get_slice_int(70, 0, 0), 0 : {|0|})
        }) : (option(string), bits(64), sbits(70), {|0, 4|})
      in
        (elem0, elem1, the_sbits(71, elem2), elem3),
      1 => let (elem0, elem1, elem2, elem3) =
        (let (flg1, byte) =
          rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct { mem_ptr? = false }) : (option(string), sbits(68))
        in
          if is_some(flg1) then {
              (flg1, get_slice_int(64, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                  rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
              } else {
                  the_bits(64, rr32(the_bits(4, b)))
              }, the_sbits(69, byte), 1 : {|1|})
          }) : (option(string), bits(64), sbits(69), {|0, 1|})
      in
        (elem0, elem1, the_sbits(71, elem2), elem3),
      2 => let (elem0, elem1, elem2, elem3) =
        (let (flg2, dword) =
          rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct { mem_ptr? = false }) : (option(string), sbits(68))
        in
          if is_some(flg2) then {
              (flg2, get_slice_int(64, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                  rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
              } else {
                  the_bits(64, rr32(the_bits(4, b)))
              }, the_sbits(69, dword), 4 : {|4|})
          }) : (option(string), bits(64), sbits(69), {|0, 4|})
      in
        (elem0, elem1, the_sbits(71, elem2), elem3),
      _ => (Some("mod-can-not-be-anything-other-than-0-1-or-2"), get_slice_int(64, 0, 0), get_slice_int(71, 0, 0), 0 : {|0|})
    }) : (option(string), bits(64), sbits(71), {|0, 1, 4|}) : (option(string), bits(64), sbits(71), {|0, 1, 4|});
    let ix = reg_index(sib_get_index(sib), rex_byte, get_slice_int(2, 1, 0)) : bits(4);
    let index = (match ix {
      4 => get_slice_int(66, 0, 0),
      _ => the_sbits(66, if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
          the_bits(65, rgfi(unsigned(ix)))
      } else {
          the_sbits(65, I32(the_bits(32, rgfi(unsigned(ix)))))
      })
    }) : sbits(66) : sbits(66);
    let scale = sib_get_scale(sib) : bits(2);
    let scaled_index = ash(signed(index), unsigned(scale)) : int;
    let effective_addr = (unsigned(base) + scaled_index) : int;
    (flg, effective_addr, displacement, nrip_bytes)
}

val x86_effective_addr_16_disp : (range(0, 4), sbits(48), bits(2)) -> (option(string), sbits(71), {|0, 1, 2|})

function x86_effective_addr_16_disp (proc_mode, temp_rip, mod_var) = {
    (match mod_var {
      0 => (None() : option(string), get_slice_int(71, 0, 0), 0 : {|0|}),
      1 => let (elem0, elem1, elem2) =
        (let (flg, byte) =
          rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
        in
          if is_some(flg) then {
              (flg, get_slice_int(69, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), the_sbits(69, byte), 1 : {|1|})
          }) : (option(string), sbits(69), {|0, 1|})
      in
        (elem0, the_sbits(71, elem1), elem2),
      2 => let (elem0, elem1, elem2) =
        (let (flg, word) =
          rime_size(proc_mode, 2, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
        in
          if is_some(flg) then {
              (flg, get_slice_int(69, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), the_sbits(69, word), 2 : {|2|})
          }) : (option(string), sbits(69), {|0, 2|})
      in
        (elem0, the_sbits(71, elem1), elem2),
      _ => (Some("mod-value-wrong"), get_slice_int(71, 0, 0), 0 : {|0|})
    }) : (option(string), sbits(71), {|0, 1, 2|})
}

val x86_effective_addr_16 : (range(0, 4), sbits(48), bits(3), bits(2)) -> (option(string), bits(16), {|0, 1, 2|})

function x86_effective_addr_16 (proc_mode, temp_rip, r_m, mod_var) = {
    (match r_m {
      0 => let bx = rr16(get_slice_int(4, 3, 0)) : bits(16) in
      let si = rr16(get_slice_int(4, 6, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(bx) + (unsigned(si) + signed(disp)), 0)), increment_rip_by)
        },
      1 => let bx = rr16(get_slice_int(4, 3, 0)) : bits(16) in
      let di = rr16(get_slice_int(4, 7, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(bx) + (unsigned(di) + signed(disp)), 0)), increment_rip_by)
        },
      2 => let bp = rr16(get_slice_int(4, 5, 0)) : bits(16) in
      let si = rr16(get_slice_int(4, 6, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(bp) + (unsigned(si) + signed(disp)), 0)), increment_rip_by)
        },
      3 => let bp = rr16(get_slice_int(4, 5, 0)) : bits(16) in
      let di = rr16(get_slice_int(4, 7, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(bp) + (unsigned(di) + signed(disp)), 0)), increment_rip_by)
        },
      4 => let si = rr16(get_slice_int(4, 6, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(si) + signed(disp), 0)), increment_rip_by)
        },
      5 => let di = rr16(get_slice_int(4, 7, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(di) + signed(disp), 0)), increment_rip_by)
        },
      6 => (match mod_var {
        0 => let (flg, disp) =
          rime_size(proc_mode, 2, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
        in
          if is_some(flg) then {
              (flg, get_slice_int(16, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), n16(the_bits(16, disp)), 2 : {|2|})
          },
        _ => let bp = rr16(get_slice_int(4, 5, 0)) : bits(16) in
        let (flg, disp, increment_rip_by) =
          x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
        in
          if is_some(flg) then {
              (flg, get_slice_int(16, 0, 0), 0 : {|0|})
          } else {
              (None() : option(string), n16(get_slice_int(16, unsigned(bp) + signed(disp), 0)), increment_rip_by)
          }
      }) : (option(string), bits(16), {|0, 1, 2|}),
      7 => let bx = rr16(get_slice_int(4, 3, 0)) : bits(16) in
      let (flg, disp, increment_rip_by) =
        x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var) : (option(string), sbits(71), {|0, 1, 2|})
      in
        if is_some(flg) then {
            (flg, get_slice_int(16, 0, 0), 0 : {|0|})
        } else {
            (None() : option(string), n16(get_slice_int(16, unsigned(bx) + signed(disp), 0)), increment_rip_by)
        },
      _ => (Some(":R/M-OUT-OF-RANGE"), get_slice_int(16, 0, 0), 0 : {|0|})
    }) : (option(string), bits(16), {|0, 1, 2|})
}

val x86_effective_addr_32_64 : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(66), {|0, 1, 4|})

function x86_effective_addr_32_64 (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (flg, addr, displacement, increment_rip_by) = (match mod_var {
      0 => let (elem0, elem1, elem2, elem3) =
        (match r_m {
          4 => let (elem0, elem1, elem2, elem3) =
            x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib) : (option(string), int, sbits(71), {|0, 1, 4|})
          in
            (elem0, elem1, the_sbits(72, elem2), elem3),
          5 => let (elem0, elem1, elem2, elem3) =
            (if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                let (elem0, elem1, elem2, elem3) = (let (flg0, dword) =
                  rime_size(0, 4, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
                in
                let (flg, next_rip) =
                  add_to_iptr(0, temp_rip, get_slice_int(48, 4 + unsigned(num_imm_bytes), 0)) : (option(string), sbits(52))
                in
                  if is_some(flg) then {
                      (flg, get_slice_int(53, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
                  } else {
                      (flg0, the_sbits(53, next_rip), the_sbits(69, dword), 4 : {|4|})
                  }) : (option(string), sbits(53), sbits(69), {|0, 4|});
                (elem0, the_sbits(54, elem1), elem2, elem3)
            } else {
                let (elem0, elem1, elem2, elem3) = (let (flg, dword) =
                  rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
                in
                  if is_some(flg) then {
                      (flg, 0 : {|0|}, get_slice_int(69, 0, 0), 0 : {|0|})
                  } else {
                      (None() : option(string), 0 : {|0|}, the_sbits(69, dword), 4 : {|4|})
                  }) : (option(string), {|0|}, sbits(69), {|0, 4|});
                (elem0, get_slice_int(54, elem1, 0), elem2, elem3)
            }) : (option(string), sbits(54), sbits(69), {|0, 4|})
          in
            (elem0, signed(elem1), the_sbits(72, elem2), elem3),
          _ => (None() : option(string), unsigned(if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
              rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
          } else {
              the_bits(64, rr32(the_bits(4, r_m)))
          }), get_slice_int(72, 0, 0), 0 : {|0|})
        }) : (option(string), int, sbits(72), {|0, 1, 4|}) : (option(string), int, sbits(72), {|0, 1, 4|})
      in
        (elem0, elem1, the_sbits(73, elem2), elem3),
      1 => let (elem0, elem1, elem2, elem3) =
        (match r_m {
          4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
          _ => let (elem0, elem1, elem2, elem3) =
            (let (flg2, byte2) =
              rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
            in
            let reg =
              (if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                  rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
              } else {
                  the_bits(64, rr32(the_bits(4, r_m)))
              }) : bits(64)
            in
              (flg2, reg, byte2, 1 : {|1|})) : (option(string), bits(64), sbits(68), {|1|})
          in
            (elem0, unsigned(elem1), the_sbits(71, elem2), elem3)
        }) : (option(string), int, sbits(71), {|0, 1, 4|}) : (option(string), int, sbits(71), {|0, 1, 4|})
      in
        (elem0, elem1, the_sbits(73, elem2), elem3),
      2 => let (elem0, elem1, elem2, elem3) =
        (match r_m {
          4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
          _ => let (elem0, elem1, elem2, elem3) =
            (let (flg1, dword) =
              rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68))
            in
            let reg =
              (if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
                  rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
              } else {
                  the_bits(64, rr32(the_bits(4, r_m)))
              }) : bits(64)
            in
              (flg1, reg, dword, 4 : {|4|})) : (option(string), bits(64), sbits(68), {|4|})
          in
            (elem0, unsigned(elem1), the_sbits(71, elem2), elem3)
        }) : (option(string), int, sbits(71), {|0, 1, 4|}) : (option(string), int, sbits(71), {|0, 1, 4|})
      in
        (elem0, elem1, the_sbits(73, elem2), elem3),
      _ => (Some("mod-value-wrong"), 0 : {|0|}, get_slice_int(73, 0, 0), 0 : {|0|})
    }) : (option(string), int, sbits(73), {|0, 1, 4|}) : (option(string), int, sbits(73), {|0, 1, 4|});
    let dst_base = (addr + signed(displacement)) : int;
    let dst_base = (if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
        the_sbits(66, if p4 then {
            the_bits(65, n32(get_slice_int(32, dst_base, 0)))
        } else {
            the_sbits(65, n64_to_i64(n64(get_slice_int(64, dst_base, 0))))
        })
    } else {
        the_bits(66, n32(get_slice_int(32, dst_base, 0)))
    }) : sbits(66);
    (flg, dst_base, increment_rip_by)
}

val x86_effective_addr : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(67), {|0, 1, 2, 4|})

function x86_effective_addr (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    if get_slice_int(8, 2, 0) == get_slice_int(8, select_address_size(proc_mode, if p4 then true else false), 0) then {
        let (elem0, elem1, elem2) = x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var) : (option(string), bits(16), {|0, 1, 2|});
        (elem0, the_bits(67, elem1), elem2)
    } else {
        let (elem0, elem1, elem2) = x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) : (option(string), sbits(66), {|0, 1, 4|});
        (elem0, the_sbits(67, elem1), elem2)
    }
}

val alignment_checking_enabled_p : unit -> bool

function alignment_checking_enabled_p () = {
    let cr0 = n32(the_bits(32, ctrs[0])) : bits(32);
    let am = cr0bits_get_am(cr0) : bits(1);
    let ac = (let rflags_var = rflags : bits(32) in the_bits(2, rflagsbits_get_ac(rflags_var))) : bits(2);
    let cpl = (let x86 = () : unit in segment_selectorbits_get_rpl(seg_visibles[1])) : bits(2);
    the_bits(8, am) == get_slice_int(8, 1, 0) & the_bits(8, ac) == get_slice_int(8, 1, 0) & the_bits(8, cpl) == get_slice_int(8, 3, 0)
}

val x86_operand_from_modr_m_and_sib_bytes : (range(0, 4), bits(1), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, range(0, 5), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), int, {|0, 1, 2, 4|}, sbits(66))

function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (flg0, addr, increment_rip_by) = (if the_bits(8, mod_var) == get_slice_int(8, 3, 0) then {
        (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
    } else {
        let (elem0, elem1, elem2) = x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) : (option(string), sbits(67), {|0, 1, 2, 4|});
        (elem0, the_sbits(68, elem1), elem2)
    }) : (option(string), sbits(68), {|0, 1, 2, 4|});
    let addr = the_sbits(64, addr) : sbits(64);
    let increment_rip_by = the_range(0, 4, increment_rip_by) : {|0, 1, 2, 4|};
    if is_some(flg0) then {
        (Some("x86-effective-addr-error"), 0 : {|0|}, 0 : {|0|}, get_slice_int(66, 0, 0))
    } else {
        let (elem0, elem1, elem2, elem3) = (let (flg2, operand) =
          (if the_bits(8, mod_var) == get_slice_int(8, 3, 0) then {
              let (elem0, elem1) = (if reg_type == get_slice_int(1, 0, 0) then {
                  (None() : option(string), the_bits(128, rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), rex_byte)))
              } else {
                  (None() : option(string), xmmi_size(get_slice_int(5, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
              }) : (option(string), bits(128));
              (elem0, unsigned(elem1))
          } else {
              let check_alignment? = (inst_ac? & alignment_checking_enabled_p()) : bool;
              rme_size(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, struct { mem_ptr? = memory_ptr? })
          }) : (option(string), int)
        in
          if is_some(flg2) then {
              (Some("Rm-Size-Error"), 0 : {|0|}, 0 : {|0|}, get_slice_int(65, 0, 0))
          } else {
              (None() : option(string), operand, increment_rip_by, the_sbits(65, addr))
          }) : (option(string), int, {|0, 1, 2, 4|}, sbits(65));
        (elem0, elem1, elem2, the_sbits(66, elem3))
    }
}

val x86_operand_to_reg_mem : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, rex_byte, r_m, mod_var) = {
    if the_bits(8, mod_var) == get_slice_int(8, 3, 0) then {
        let x86 = write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), operand, rex_byte) : unit;
        None() : option(string)
    } else {
        let check_alignment? = (inst_ac? & alignment_checking_enabled_p()) : bool;
        let flg = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct { mem_ptr? = memory_ptr? }) : option(string);
        flg
    }
}

val x86_operand_to_xmm_mem : (range(0, 4), {|4, 8, 16|}, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_xmm_mem (proc_mode, operand_size, inst_ac?, operand, seg_reg, addr, rex_byte, r_m, mod_var) = {
    if the_bits(8, mod_var) == get_slice_int(8, 3, 0) then {
        let x86 = write_xmmi_size(get_slice_int(5, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), operand) : unit;
        None() : option(string)
    } else {
        let check_alignment? = (inst_ac? & alignment_checking_enabled_p()) : bool;
        let flg = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct { mem_ptr? = false }) : option(string);
        flg
    }
}

val select_operand_size : (range(0, 4), bool, bits(8), bool, bits(52), bool, bool, bool) -> {|8, 1, 2, 4|}

function select_operand_size (proc_mode, byte_operand?, rex_byte, imm?, prefixes, default64?, ignore_rex?, ignore_p3_64?) = {
    if byte_operand? then 1 else if get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) then {
        if logbitp(3, rex_byte) & not_bool(ignore_rex?) then {
            if imm? then 4 else 8
        } else if get_slice_int(8, 102, 0) == prefixes_get_opr(prefixes) & not_bool(ignore_p3_64?) then 2 else if default64? then 8 else 4
    } else {
        let cs_attr = seg_hidden_attrs[1] : bits(16);
        let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : bits(1);
        let p3? = (get_slice_int(8, 102, 0) == prefixes_get_opr(prefixes)) : bool;
        if unsigned(cs_d) == 1 then {
            if p3? then 2 else 4
        } else if p3? then 4 else 2
    }
}

val select_segment_register : (range(0, 4), bits(8), bool, bits(2), bits(3), bits(8)) -> {|0, 1, 2, 3, 4, 5|}

function select_segment_register (proc_mode, p2, p4?, mod_var, r_m, sib) = {
    (match p2 {
      46 => 1,
      54 => 2,
      62 => 3,
      38 => 0,
      100 => 4,
      101 => 5,
      _ => let addr_size = select_address_size(proc_mode, p4?) : {|8, 2, 4|} in
        if the_range(0, 5, addr_size) == 2 then {
            if not_bool(unsigned(mod_var) == 3) & (the_range(0, 5, unsigned(r_m)) == 2 | the_range(0, 5, unsigned(r_m)) == 3) then 2 else 3
        } else if (unsigned(mod_var) == 1 | unsigned(mod_var) == 2) & the_range(0, 5, unsigned(r_m)) == 5 | not_bool(unsigned(mod_var) == 3) & the_range(0, 5, unsigned(r_m)) == 4 & the_range(0, 5, unsigned(sib_get_base(sib))) == 4 then 2 else 3
    }) : {|0, 1, 2, 3, 4, 5|}
}

val check_instruction_length : (sbits(48), sbits(48), bits(3)) -> option(int)

function check_instruction_length (start_rip, temp_rip, delta_rip) = {
    let start_rip = signed(start_rip) : int;
    let temp_rip = signed(temp_rip) : int;
    let delta_rip = unsigned(delta_rip) : int;
    let end_rip = the_sbits(49, signed(the_sbits(48, temp_rip)) + unsigned(the_bits(3, delta_rip))) : sbits(49);
    let length = the_sbits(50, signed(end_rip) - signed(the_sbits(48, start_rip))) : sbits(50);
    if 15 < signed(length) then {
        Some(signed(length))
    } else {
        None()
    }
}
