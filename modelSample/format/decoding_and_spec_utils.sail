$include "./other_non_det.sail"

val read_iptr : range(0, 4) -> sbits(48)

function read_iptr proc_mode = {
    let iptr : sbits(48) = rip;
    (match proc_mode {
      0 => iptr,
      1 => sail_zero_extend(let cs_attr : bits(16) = seg_hidden_attrs[1] in
      let cs_d : bits(1) = code_segment_descriptor_attributesbits_get_d(cs_attr) in
        if cs_d == 0b1 then {
            truncate(iptr, 32)
        } else {
            sail_zero_extend(truncate(iptr, 16), 32)
        }, 48),
      _ => 0x000000000000
    }) : sbits(48)
}

val add_to_iptr : (range(0, 4), sbits(48), sbits(48)) -> (option(string), sbits(49))

function add_to_iptr (proc_mode, iptr, delta) = {
    let iptr_plus_delta : sbits(49) = bits_of_int(signed(iptr) + signed(delta), 49);
    (match proc_mode {
      0 => {
          if canonical_address_p(signed(iptr_plus_delta)) then {
              (None() : option(string), iptr_plus_delta)
          } else {
              (Some(":NON-CANONICAL-INSTRUCTION-POINTER"), 0b0000000000000000000000000000000000000000000000000)
          }
      },
      1 => {
          let (elem0, elem1) : (option(string), bits(32)) = {
              let cs_limit : bits(32) = bits_of_int(loghead(32, unsigned(seg_hidden_limits[1])), 32);
              if 0 <= signed(iptr_plus_delta) & signed(iptr_plus_delta) <= unsigned(cs_limit) then {
                  (None() : option(string), truncate(iptr_plus_delta, 32))
              } else {
                  (Some(":OUT-OF-SEGMENT-INSTRUCTION-POINTER"), 0x00000000)
              }
          };
          (elem0, sail_zero_extend(elem1, 49))
      },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0b0000000000000000000000000000000000000000000000000)
    }) : (option(string), sbits(49))
}

val write_iptr : (range(0, 4), sbits(48)) -> unit

function write_iptr (proc_mode, iptr) = {
    (match proc_mode {
      0 => rip = iptr,
      1 => {
          let cs_attr : bits(16) = seg_hidden_attrs[1];
          let cs_d : bits(1) = code_segment_descriptor_attributesbits_get_d(cs_attr);
          if cs_d == 0b1 then {
              rip = sail_zero_extend(truncate(iptr, 32), 48)
          } else {
              let rip_var : sbits(48) = rip;
              let rip_new : sbits(48) = changeSlice(rip_var, 0, 16, truncate(iptr, 16));
              rip = rip_new
          }
      },
      _ => ()
    }) : unit
}

val read_sptr : range(0, 4) -> sbits(64)

function read_sptr proc_mode = {
    let sptr : sbits(64) = rgfi(4);
    (match proc_mode {
      0 => sptr,
      1 => sail_zero_extend(let ss_attr : bits(16) = seg_hidden_attrs[2] in
      let ss_b : bits(1) = data_segment_descriptor_attributesbits_get_d_b(ss_attr) in
        if ss_b == 0b1 then {
            truncate(sptr, 32)
        } else {
            sail_zero_extend(truncate(sptr, 16), 32)
        }, 64),
      _ => 0x0000000000000000
    }) : sbits(64)
}

val add_to_sptr : (range(0, 4), sbits(64), sbits(64)) -> (option(string), sbits(64))

function add_to_sptr (proc_mode, sptr, delta) = {
    let sptr_plus_delta : sbits(65) = bits_of_int(signed(sptr) + signed(delta), 65);
    (match proc_mode {
      0 => {
          let sptr_plus_delta : sbits(64) = truncate(sptr_plus_delta, 64);
          if canonical_address_p(signed(sptr_plus_delta)) then {
              (None() : option(string), sptr_plus_delta)
          } else {
              (Some(":NON-CANONICAL-STACK-ADDRESS"), 0x0000000000000000)
          }
      },
      1 => {
          let (elem0, elem1) : (option(string), bits(32)) = {
              let ss_limit : bits(32) = seg_hidden_limits[2];
              let ss_attr : bits(16) = seg_hidden_attrs[2];
              let ss_b : bits(1) = data_segment_descriptor_attributesbits_get_d_b(ss_attr);
              let ss_e : bits(1) = data_segment_descriptor_attributesbits_get_e(ss_attr);
              let ss_lower : int = {
                  if ss_e == 0b1 then {
                      1 + unsigned(ss_limit)
                  } else 0
              };
              let ss_upper : bits(32) = {
                  if ss_e == 0b1 then {
                      bits_of_int(if ss_b == 0b1 then 4294967295 else 65535, 32)
                  } else {
                      ss_limit
                  }
              };
              let sptr_plus_delta : bits(32) = {
                  if ss_b == 0b1 then {
                      truncate(sptr_plus_delta, 32)
                  } else {
                      sail_zero_extend(truncate(sptr_plus_delta, 16), 32)
                  }
              };
              if not_bool(ss_lower <= unsigned(sptr_plus_delta) & unsigned(sptr_plus_delta) <= unsigned(ss_upper)) then {
                  (Some(":OUT-OF-SEGMENT-STACK-ADDRESS"), 0x00000000)
              } else {
                  (None() : option(string), sptr_plus_delta)
              }
          };
          (elem0, sail_zero_extend(elem1, 64))
      },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0x0000000000000000)
    }) : (option(string), sbits(64))
}

val write_sptr : (range(0, 4), sbits(64)) -> unit

function write_sptr (proc_mode, sptr) = {
    (match proc_mode {
      0 => write_rgfi(4, sptr),
      1 => {
          let ss_attr : bits(16) = seg_hidden_attrs[2];
          let ss_b : bits(1) = data_segment_descriptor_attributesbits_get_d_b(ss_attr);
          if ss_b == 0b1 then {
              write_rgfi(4, sail_zero_extend(truncate(sptr, 32), 64))
          } else {
              let rsp_var : sbits(64) = rgfi(4);
              let rsp_new : sbits(64) = changeSlice(rsp_var, 0, 16, truncate(sptr, 16));
              write_rgfi(4, rsp_new)
          }
      },
      _ => ()
    }) : unit
}

val select_address_size : (range(0, 4), bool) -> {|2, 4, 8|}

function select_address_size (proc_mode, p4?) = {
    (match proc_mode {
      0 => {
          if p4? then 4 else 8
      },
      _ => {
          let cs_attr : bits(16) = seg_hidden_attrs[1];
          let cs_d : bits(1) = code_segment_descriptor_attributesbits_get_d(cs_attr);
          if cs_d == 0b1 then {
              if p4? then 2 else 4
          } else if p4? then 4 else 2
      }
    }) : {|2, 4, 8|}
}

val x86_effective_addr_from_sib : (range(0, 4), sbits(48), bits(8), bits(2), bits(8)) -> (option(string), int, sbits(64), {|0, 1, 4|})

function x86_effective_addr_from_sib (proc_mode, temp_rip, rex_byte, mod_var, sib) = {
    let b : bits(3) = sib_get_base(sib);
    let check_alignment? : bool = false;
    let (flg, base, displacement, nrip_bytes) : (option(string), sbits(64), sbits(64), {|0, 1, 4|}) = {
        match mod_var {
          0 => {
              if b == 0b101 then {
                  let (elem0, elem1, elem2, elem3) : (option(string), {|0|}, sbits(64), {|0, 4|}) = {
                      let (flg0, dword) : (option(string), sbits(64)) = rime_size(proc_mode, 4, sail_sign_extend(temp_rip, 64), 1, ":X", check_alignment?, struct { mem_ptr? = false });
                      if is_some(flg0) then {
                          (flg0, 0, 0x0000000000000000, 0)
                      } else {
                          (None() : option(string), 0, dword, 4)
                      }
                  };
                  (elem0, bits_of_int(elem1, 64), elem2, elem3)
              } else {
                  (None() : option(string), if proc_mode == 0 then {
                      rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
                  } else {
                      sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64)
                  }, 0x0000000000000000, 0)
              }
          },
          1 => {
              let (flg1, byte) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", check_alignment?, struct { mem_ptr? = false });
              if is_some(flg1) then {
                  (flg1, 0x0000000000000000, 0x0000000000000000, 0)
              } else {
                  (None() : option(string), if proc_mode == 0 then {
                      rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
                  } else {
                      sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64)
                  }, byte, 1)
              }
          },
          2 => {
              let (flg2, dword) : (option(string), sbits(64)) = rime_size(proc_mode, 4, sail_sign_extend(temp_rip, 64), 1, ":X", check_alignment?, struct { mem_ptr? = false });
              if is_some(flg2) then {
                  (flg2, 0x0000000000000000, 0x0000000000000000, 0)
              } else {
                  (None() : option(string), if proc_mode == 0 then {
                      rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
                  } else {
                      sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64)
                  }, dword, 4)
              }
          },
          _ => (Some("mod-can-not-be-anything-other-than-0-1-or-2"), 0x0000000000000000, 0x0000000000000000, 0)
        }
    };
    let ix : bits(4) = reg_index(sib_get_index(sib), rex_byte, 0b01);
    let index : sbits(64) = {
        match ix {
          4 => 0x0000000000000000,
          _ => {
              if proc_mode == 0 then {
                  rgfi(unsigned(ix))
              } else {
                  sail_sign_extend(truncate(rgfi(unsigned(ix)), 32), 64)
              }
          }
        }
    };
    let scale : bits(2) = sib_get_scale(sib);
    let scaled_index : int = ash(signed(index), unsigned(scale));
    let effective_addr : int = signed(base) + scaled_index;
    (flg, effective_addr, displacement, nrip_bytes)
}

val x86_effective_addr_16_disp : (range(0, 4), sbits(48), bits(2)) -> (option(string), sbits(64), {|0, 1, 2|})

function x86_effective_addr_16_disp (proc_mode, temp_rip, mod_var) = {
    (match mod_var {
      0 => (None() : option(string), 0x0000000000000000, 0),
      1 => {
          let (flg, byte) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
          if is_some(flg) then {
              (flg, 0x0000000000000000, 0)
          } else {
              (None() : option(string), byte, 1)
          }
      },
      2 => {
          let (flg, word) : (option(string), sbits(64)) = rime_size(proc_mode, 2, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
          if is_some(flg) then {
              (flg, 0x0000000000000000, 0)
          } else {
              (None() : option(string), word, 2)
          }
      },
      _ => (Some("mod-value-wrong"), 0x0000000000000000, 0)
    }) : (option(string), sbits(64), {|0, 1, 2|})
}

val x86_effective_addr_16 : (range(0, 4), sbits(48), bits(3), bits(2)) -> (option(string), bits(16), {|0, 1, 2|})

function x86_effective_addr_16 (proc_mode, temp_rip, r_m, mod_var) = {
    (match r_m {
      0 => {
          let bx : bits(16) = rr16(0x3);
          let si : bits(16) = rr16(0x6);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(bx) + (unsigned(si) + signed(disp)), 16), increment_rip_by)
          }
      },
      1 => {
          let bx : bits(16) = rr16(0x3);
          let di : bits(16) = rr16(0x7);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(bx) + (unsigned(di) + signed(disp)), 16), increment_rip_by)
          }
      },
      2 => {
          let bp : bits(16) = rr16(0x5);
          let si : bits(16) = rr16(0x6);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(bp) + (unsigned(si) + signed(disp)), 16), increment_rip_by)
          }
      },
      3 => {
          let bp : bits(16) = rr16(0x5);
          let di : bits(16) = rr16(0x7);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(bp) + (unsigned(di) + signed(disp)), 16), increment_rip_by)
          }
      },
      4 => {
          let si : bits(16) = rr16(0x6);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(si) + signed(disp), 16), increment_rip_by)
          }
      },
      5 => {
          let di : bits(16) = rr16(0x7);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(di) + signed(disp), 16), increment_rip_by)
          }
      },
      6 => {
          (match mod_var {
            0 => {
                let (flg, disp) : (option(string), sbits(64)) = rime_size(proc_mode, 2, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
                if is_some(flg) then {
                    (flg, 0x0000, 0)
                } else {
                    (None() : option(string), truncate(disp, 16), 2)
                }
            },
            _ => {
                let bp : bits(16) = rr16(0x5);
                let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
                if is_some(flg) then {
                    (flg, 0x0000, 0)
                } else {
                    (None() : option(string), bits_of_int(unsigned(bp) + signed(disp), 16), increment_rip_by)
                }
            }
          }) : (option(string), bits(16), {|0, 1, 2|})
      },
      7 => {
          let bx : bits(16) = rr16(0x3);
          let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var);
          if is_some(flg) then {
              (flg, 0x0000, 0)
          } else {
              (None() : option(string), bits_of_int(unsigned(bx) + signed(disp), 16), increment_rip_by)
          }
      },
      _ => (Some(":R/M-OUT-OF-RANGE"), 0x0000, 0)
    }) : (option(string), bits(16), {|0, 1, 2|})
}

val x86_effective_addr_32_64 : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(64), {|0, 1, 4|})

function x86_effective_addr_32_64 (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (flg, addr, displacement, increment_rip_by) : (option(string), int, sbits(64), {|0, 1, 4|}) = {
        match mod_var {
          0 => {
              (match r_m {
                4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
                5 => {
                    let (elem0, elem1, elem2, elem3) : (option(string), sbits(49), sbits(64), {|0, 4|}) = {
                        if proc_mode == 0 then {
                            let (flg0, dword) : (option(string), sbits(64)) = rime_size(0, 4, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
                            let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(0, temp_rip, bits_of_int(4 + unsigned(num_imm_bytes), 48));
                            if is_some(flg) then {
                                (flg, 0b0000000000000000000000000000000000000000000000000, 0x0000000000000000, 0)
                            } else {
                                (flg0, next_rip, dword, 4)
                            }
                        } else {
                            let (elem0, elem1, elem2, elem3) : (option(string), {|0|}, sbits(64), {|0, 4|}) = {
                                let (flg, dword) : (option(string), sbits(64)) = rime_size(proc_mode, 4, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
                                if is_some(flg) then {
                                    (flg, 0, 0x0000000000000000, 0)
                                } else {
                                    (None() : option(string), 0, dword, 4)
                                }
                            };
                            (elem0, bits_of_int(elem1, 49), elem2, elem3)
                        }
                    };
                    (elem0, signed(elem1), elem2, elem3)
                },
                _ => (None() : option(string), signed(if proc_mode == 0 then {
                    rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
                } else {
                    sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)
                }), 0x0000000000000000, 0)
              }) : (option(string), int, sbits(64), {|0, 1, 4|})
          },
          1 => {
              (match r_m {
                4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
                _ => {
                    let (elem0, elem1, elem2, elem3) : (option(string), sbits(64), sbits(64), {|1|}) = {
                        let (flg2, byte2) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
                        let reg : sbits(64) = {
                            if proc_mode == 0 then {
                                rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
                            } else {
                                sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)
                            }
                        };
                        (flg2, reg, byte2, 1)
                    };
                    (elem0, signed(elem1), elem2, elem3)
                }
              }) : (option(string), int, sbits(64), {|0, 1, 4|})
          },
          2 => {
              (match r_m {
                4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
                _ => {
                    let (elem0, elem1, elem2, elem3) : (option(string), sbits(64), sbits(64), {|4|}) = {
                        let (flg1, dword) : (option(string), sbits(64)) = rime_size(proc_mode, 4, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
                        let reg : sbits(64) = {
                            if proc_mode == 0 then {
                                rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
                            } else {
                                sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)
                            }
                        };
                        (flg1, reg, dword, 4)
                    };
                    (elem0, signed(elem1), elem2, elem3)
                }
              }) : (option(string), int, sbits(64), {|0, 1, 4|})
          },
          _ => (Some("mod-value-wrong"), 0, 0x0000000000000000, 0)
        }
    };
    let dst_base : int = addr + signed(displacement);
    let dst_base : sbits(64) = {
        if proc_mode == 0 then {
            if p4 then {
                sail_zero_extend(bits_of_int(dst_base, 32), 64)
            } else {
                bits_of_int(dst_base, 64)
            }
        } else {
            sail_zero_extend(bits_of_int(dst_base, 32), 64)
        }
    };
    (flg, dst_base, increment_rip_by)
}

val x86_effective_addr : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(64), {|0, 1, 2, 4|})

function x86_effective_addr (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    if 2 == select_address_size(proc_mode, if p4 then true else false) then {
        let (elem0, elem1, elem2) : (option(string), bits(16), {|0, 1, 2|}) = x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var);
        (elem0, sail_zero_extend(elem1, 64), elem2)
    } else {
        x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
    }
}

val alignment_checking_enabled_p : unit -> bool

function alignment_checking_enabled_p () = {
    let cr0 : bits(32) = truncate(ctrs[0], 32);
    let am : bits(1) = cr0bits_get_am(cr0);
    let ac : bits(2) = {
        let rflags_var : bits(32) = rflags;
        sail_zero_extend(rflagsbits_get_ac(rflags_var), 2)
    };
    let cpl : bits(2) = {
        let x86 : unit = ();
        segment_selectorbits_get_rpl(seg_visibles[1])
    };
    am == 0b1 & ac == 0b01 & cpl == 0b11
}

val x86_operand_from_modr_m_and_sib_bytes : (range(0, 4), bits(1), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, range(0, 5), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), int, {|0, 1, 2, 4|}, sbits(64))

function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (None() : option(string), 0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
        }
    };
    if is_some(flg0) then {
        (Some("x86-effective-addr-error"), 0, 0, 0x0000000000000000)
    } else {
        let (flg2, operand) : (option(string), int) = {
            if mod_var == 0b11 then {
                let (elem0, elem1) : (option(string), bits(128)) = {
                    if reg_type == 0b0 then {
                        (None() : option(string), sail_zero_extend(rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), rex_byte), 128))
                    } else {
                        (None() : option(string), xmmi_size(bits_of_int(operand_size, 5), reg_index(r_m, rex_byte, 0b00)))
                    }
                };
                (elem0, unsigned(elem1))
            } else {
                let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
                rme_size(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, struct { mem_ptr? = memory_ptr? })
            }
        };
        if is_some(flg2) then {
            (Some("Rm-Size-Error"), 0, 0, 0x0000000000000000)
        } else {
            (None() : option(string), operand, increment_rip_by, addr)
        }
    }
}

val x86_operand_to_reg_mem : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, rex_byte, r_m, mod_var) = {
    if mod_var == 0b11 then {
        let x86 : unit = write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), operand, rex_byte);
        None() : option(string)
    } else {
        let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
        let flg : option(string) = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct { mem_ptr? = memory_ptr? });
        flg
    }
}

val x86_operand_to_xmm_mem : (range(0, 4), {|4, 8, 16|}, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_xmm_mem (proc_mode, operand_size, inst_ac?, operand, seg_reg, addr, rex_byte, r_m, mod_var) = {
    if mod_var == 0b11 then {
        let x86 : unit = write_xmmi_size(bits_of_int(operand_size, 5), reg_index(r_m, rex_byte, 0b00), operand);
        None() : option(string)
    } else {
        let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
        let flg : option(string) = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct { mem_ptr? = false });
        flg
    }
}

val select_operand_size : (range(0, 4), bool, bits(8), bool, bits(52), bool, bool, bool) -> {|1, 2, 4, 8|}

function select_operand_size (proc_mode, byte_operand?, rex_byte, imm?, prefixes, default64?, ignore_rex?, ignore_p3_64?) = {
    if byte_operand? then 1 else if proc_mode == 0 then {
        if logbitp(3, rex_byte) & not_bool(ignore_rex?) then {
            if imm? then 4 else 8
        } else if 0x66 == prefixes_get_opr(prefixes) & not_bool(ignore_p3_64?) then 2 else if default64? then 8 else 4
    } else {
        let cs_attr : bits(16) = seg_hidden_attrs[1];
        let cs_d : bits(1) = code_segment_descriptor_attributesbits_get_d(cs_attr);
        let p3? : bool = 0x66 == prefixes_get_opr(prefixes);
        if cs_d == 0b1 then {
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
      _ => {
          let addr_size : {|2, 4, 8|} = select_address_size(proc_mode, p4?);
          if addr_size == 2 then {
              if not_bool(mod_var == 0b11) & (r_m == 0b010 | r_m == 0b011) then 2 else 3
          } else if (mod_var == 0b01 | mod_var == 0b10) & r_m == 0b101 | not_bool(mod_var == 0b11) & r_m == 0b100 & sib_get_base(sib) == 0b100 then 2 else 3
      }
    }) : {|0, 1, 2, 3, 4, 5|}
}

val check_instruction_length : (sbits(48), sbits(48), bits(3)) -> option(int)

function check_instruction_length (start_rip, temp_rip, delta_rip) = {
    let start_rip : int = signed(start_rip);
    let temp_rip : int = signed(temp_rip);
    let delta_rip : nat = unsigned(delta_rip);
    let end_rip : sbits(49) = bits_of_int(signed(bits_of_int(temp_rip, 48)) + unsigned(bits_of_int(delta_rip, 3)), 49);
    let length : sbits(50) = bits_of_int(signed(end_rip) - signed(bits_of_int(start_rip, 48)), 50);
    if 15 < signed(length) then {
        Some(signed(length))
    } else {
        None()
    }
}
