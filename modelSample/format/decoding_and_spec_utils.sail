$include "./other_non_det.sail"

val read_iptr : (int, int) -> int effect {escape, rreg}

function read_iptr (proc_mode, x86) = {
    let iptr = ripi(0) : int;
    (match proc_mode {
      0 => iptr,
      1 => let cs_attr = seg_hidden_attri(1, 0) : int in
        let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : int in
          if cs_d == 1 then {
              n32(iptr)
          } else {
              n16(iptr)
          },
      _ => 0
    }) : int
}

val add_to_iptr : (int, int, int, int) -> (option(string), int) effect {escape, rreg}

function add_to_iptr (proc_mode, iptr, delta, x86) = {
    let iptr_plus_delta = (iptr + delta) : int;
    (match proc_mode {
      0 => if canonical_address_p(iptr_plus_delta) then {
          (None() : option(string), iptr_plus_delta)
      } else {
          (Some(":NON-CANONICAL-INSTRUCTION-POINTER"), 0 : int)
      },
      1 => let cs_limit = loghead(32, seg_hidden_limiti(1, 0)) : int in
        if 0 <= iptr_plus_delta & iptr_plus_delta <= cs_limit then {
            (None() : option(string), iptr_plus_delta)
        } else {
            (Some(":OUT-OF-SEGMENT-INSTRUCTION-POINTER"), 0 : int)
        },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0 : int)
    }) : (option(string), int)
}

val write_iptr : (int, int, int) -> int effect {escape, rreg, wreg}

function write_iptr (proc_mode, iptr, x86) = {
    (match proc_mode {
      0 => write_rip(iptr, 0),
      1 => let cs_attr = seg_hidden_attri(1, 0) : int in
        let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : int in
          if cs_d == 1 then {
              write_rip(n32(iptr), 0)
          } else {
              let rip_var = ripi(0) : int;
              let rip_new = changeBits(rip_var, 0, 16, n16(iptr)) : int;
              write_rip(rip_new, 0)
          },
      _ => 0
    }) : int
}

val read_sptr : (int, int) -> int effect {escape, rreg}

function read_sptr (proc_mode, x86) = {
    let sptr = rgfi(4, 0) : int;
    (match proc_mode {
      0 => sptr,
      1 => let ss_attr = seg_hidden_attri(2, 0) : int in
        let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : int in
          if ss_b == 1 then {
              n32(sptr)
          } else {
              n16(sptr)
          },
      _ => 0
    }) : int
}

val add_to_sptr : (int, int, int, int) -> (option(string), int) effect {escape, rreg}

function add_to_sptr (proc_mode, sptr, delta, x86) = {
    let sptr_plus_delta = (sptr + delta) : int;
    (match proc_mode {
      0 => let sptr_plus_delta = I64(sptr_plus_delta) : int in
        if canonical_address_p(sptr_plus_delta) then {
            (None() : option(string), sptr_plus_delta)
        } else {
            (Some(":NON-CANONICAL-STACK-ADDRESS"), 0 : int)
        },
      1 => let ss_limit = seg_hidden_limiti(2, 0) : int in
        let ss_attr = seg_hidden_attri(2, 0) : int in
          let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : int in
            let ss_e = data_segment_descriptor_attributesbits_get_e(ss_attr) : int in
              let ss_lower = (if ss_e == 1 then {
                  1 + ss_limit
              } else 0) : int in
                let ss_upper = (if ss_e == 1 then {
                    if ss_b == 1 then 4294967295 else 65535
                } else {
                    ss_limit
                }) : int in
                  let sptr_plus_delta = (if ss_b == 1 then {
                      n32(sptr_plus_delta)
                  } else {
                      n16(sptr_plus_delta)
                  }) : int in
                    if not_bool(ss_lower <= sptr_plus_delta & sptr_plus_delta <= ss_upper) then {
                        (Some(":OUT-OF-SEGMENT-STACK-ADDRESS"), 0 : int)
                    } else {
                        (None() : option(string), sptr_plus_delta)
                    },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0 : int)
    }) : (option(string), int)
}

val write_sptr : (int, int, int) -> int effect {escape, rreg, wreg}

function write_sptr (proc_mode, sptr, x86) = {
    (match proc_mode {
      0 => write_rgfi(4, sptr, 0),
      1 => let ss_attr = seg_hidden_attri(2, 0) : int in
        let ss_b = data_segment_descriptor_attributesbits_get_d_b(ss_attr) : int in
          if ss_b == 1 then {
              write_rgfi(4, n32(sptr), 0)
          } else {
              let rsp_var = rgfi(4, 0) : int;
              let rsp_new = changeBits(rsp_var, 0, 16, n16(sptr)) : int;
              write_rgfi(4, rsp_new, 0)
          },
      _ => 0
    }) : int
}

val select_address_size : (int, bool, int) -> int effect {escape, rreg}

function select_address_size (proc_mode, p4?, x86) = {
    (match proc_mode {
      0 => if p4? then 4 else 8,
      _ => let cs_attr = seg_hidden_attri(1, 0) : int in
        let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : int in
          if cs_d == 1 then {
              if p4? then 2 else 4
          } else if p4? then 4 else 2
    }) : int
}

val x86_effective_addr_from_sib : (int, int, int, int, int, int) -> (option(string), int, int, int, int) effect {escape, rmem, rreg}

function x86_effective_addr_from_sib (proc_mode, temp_rip, rex_byte, mod_var, sib, x86) = {
    let b = sib_get_base(sib) : int;
    let check_alignment? = false : bool;
    let (flg, base, displacement, nrip_bytes, x86) = (match mod_var {
      0 => if b == 5 then {
          let (flg0, dword, x86) = rime_size(proc_mode, 4, temp_rip, 1, ":X", check_alignment?, 0, struct { mem_ptr? = false }) : (option(string), int, int);
          if is_some(flg0) then {
              (flg0, 0 : int, 0 : int, 0 : int, 0 : int)
          } else {
              (None() : option(string), 0 : int, dword, 4 : int, 0 : int)
          }
      } else {
          (None() : option(string), if proc_mode == 0 then {
              rgfi(reg_index(b, rex_byte, 0), 0)
          } else {
              rr32(b, 0)
          }, 0 : int, 0 : int, 0 : int)
      },
      1 => let (flg1, byte, x86) = rime_size(proc_mode, 1, temp_rip, 1, ":X", check_alignment?, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
        if is_some(flg1) then {
            (flg1, 0 : int, 0 : int, 0 : int, 0 : int)
        } else {
            (None() : option(string), if proc_mode == 0 then {
                rgfi(reg_index(b, rex_byte, 0), 0)
            } else {
                rr32(b, 0)
            }, byte, 1 : int, 0 : int)
        },
      2 => let (flg2, dword, x86) = rime_size(proc_mode, 4, temp_rip, 1, ":X", check_alignment?, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
        if is_some(flg2) then {
            (flg2, 0 : int, 0 : int, 0 : int, 0 : int)
        } else {
            (None() : option(string), if proc_mode == 0 then {
                rgfi(reg_index(b, rex_byte, 0), 0)
            } else {
                rr32(b, 0)
            }, dword, 4 : int, 0 : int)
        },
      _ => (Some("mod-can-not-be-anything-other-than-0-1-or-2"), 0 : int, 0 : int, 0 : int, 0 : int)
    }) : (option(string), int, int, int, int) : (option(string), int, int, int, int);
    let ix = reg_index(sib_get_index(sib), rex_byte, 1) : int;
    let index = (match ix {
      4 => 0,
      _ => if proc_mode == 0 then {
          rgfi(ix, 0)
      } else {
          I32(rgfi(ix, 0))
      }
    }) : int : int;
    let scale = sib_get_scale(sib) : int;
    let scaled_index = ash(index, scale) : int;
    let effective_addr = (base + scaled_index) : int;
    (flg, effective_addr, displacement, nrip_bytes, 0 : int)
}

val x86_effective_addr_16_disp : (int, int, int, int) -> (option(string), int, int, int) effect {escape, rmem, rreg}

function x86_effective_addr_16_disp (proc_mode, temp_rip, mod_var, x86) = {
    (match mod_var {
      0 => (None() : option(string), 0 : int, 0 : int, 0 : int),
      1 => let (flg, byte, x86) = rime_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
        if is_some(flg) then {
            (flg, 0 : int, 0 : int, 0 : int)
        } else {
            (None() : option(string), byte, 1 : int, 0 : int)
        },
      2 => let (flg, word, x86) = rime_size(proc_mode, 2, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
        if is_some(flg) then {
            (flg, 0 : int, 0 : int, 0 : int)
        } else {
            (None() : option(string), word, 2 : int, 0 : int)
        },
      _ => (Some("mod-value-wrong"), 0 : int, 0 : int, 0 : int)
    }) : (option(string), int, int, int)
}

val x86_effective_addr_16 : (int, int, int, int, int) -> (option(string), int, int, int) effect {escape, rmem, rreg}

function x86_effective_addr_16 (proc_mode, temp_rip, r_m, mod_var, x86) = {
    (match r_m {
      0 => let bx = rr16(3, 0) : int in
        let si = rr16(6, 0) : int in
          let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), n16(bx + (si + disp)), increment_rip_by, 0 : int)
            },
      1 => let bx = rr16(3, 0) : int in
        let di = rr16(7, 0) : int in
          let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), n16(bx + (di + disp)), increment_rip_by, 0 : int)
            },
      2 => let bp = rr16(5, 0) : int in
        let si = rr16(6, 0) : int in
          let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), n16(bp + (si + disp)), increment_rip_by, 0 : int)
            },
      3 => let bp = rr16(5, 0) : int in
        let di = rr16(7, 0) : int in
          let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), n16(bp + (di + disp)), increment_rip_by, 0 : int)
            },
      4 => let si = rr16(6, 0) : int in
        let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
          if is_some(flg) then {
              (flg, 0 : int, 0 : int, 0 : int)
          } else {
              (None() : option(string), n16(si + disp), increment_rip_by, 0 : int)
          },
      5 => let di = rr16(7, 0) : int in
        let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
          if is_some(flg) then {
              (flg, 0 : int, 0 : int, 0 : int)
          } else {
              (None() : option(string), n16(di + disp), increment_rip_by, 0 : int)
          },
      6 => (match mod_var {
        0 => let (flg, disp, x86) = rime_size(proc_mode, 2, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
          if is_some(flg) then {
              (flg, 0 : int, 0 : int, 0 : int)
          } else {
              (None() : option(string), n16(disp), 2 : int, 0 : int)
          },
        _ => let bp = rr16(5, 0) : int in
          let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), n16(bp + disp), increment_rip_by, 0 : int)
            }
      }) : (option(string), int, int, int),
      7 => let bx = rr16(3, 0) : int in
        let (flg, disp, increment_rip_by, x86) = x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var, 0) : (option(string), int, int, int) in
          if is_some(flg) then {
              (flg, 0 : int, 0 : int, 0 : int)
          } else {
              (None() : option(string), n16(bx + disp), increment_rip_by, 0 : int)
          },
      _ => (Some(":R/M-OUT-OF-RANGE"), 0 : int, 0 : int, 0 : int)
    }) : (option(string), int, int, int)
}

val x86_effective_addr_32_64 : (int, bool, int, int, int, int, int, int, int) -> (option(string), int, int, int) effect {escape, rmem, rreg}

function x86_effective_addr_32_64 (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes, x86) = {
    let (flg, addr, displacement, increment_rip_by, x86) = (match mod_var {
      0 => (match r_m {
        4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib, 0),
        5 => if proc_mode == 0 then {
            let (flg0, dword, x86) = rime_size(0, 4, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            let (flg, next_rip) = add_to_iptr(0, temp_rip, 4 + num_imm_bytes, 0) : (option(string), int);
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int, 0 : int)
            } else {
                (flg0, next_rip, dword, 4 : int, 0 : int)
            }
        } else {
            let (flg, dword, x86) = rime_size(proc_mode, 4, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            if is_some(flg) then {
                (flg, 0 : int, 0 : int, 0 : int, 0 : int)
            } else {
                (None() : option(string), 0 : int, dword, 4 : int, 0 : int)
            }
        },
        _ => (None() : option(string), if proc_mode == 0 then {
            rgfi(reg_index(r_m, rex_byte, 0), 0)
        } else {
            rr32(r_m, 0)
        }, 0 : int, 0 : int, 0 : int)
      }) : (option(string), int, int, int, int),
      1 => (match r_m {
        4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib, 0),
        _ => let (flg2, byte2, x86) = rime_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
          let reg = (if proc_mode == 0 then {
              rgfi(reg_index(r_m, rex_byte, 0), 0)
          } else {
              rr32(r_m, 0)
          }) : int in
            (flg2, reg, byte2, 1 : int, 0 : int)
      }) : (option(string), int, int, int, int),
      2 => (match r_m {
        4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib, 0),
        _ => let (flg1, dword, x86) = rime_size(proc_mode, 4, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int) in
          let reg = (if proc_mode == 0 then {
              rgfi(reg_index(r_m, rex_byte, 0), 0)
          } else {
              rr32(r_m, 0)
          }) : int in
            (flg1, reg, dword, 4 : int, 0 : int)
      }) : (option(string), int, int, int, int),
      _ => (Some("mod-value-wrong"), 0 : int, 0 : int, 0 : int, 0 : int)
    }) : (option(string), int, int, int, int) : (option(string), int, int, int, int);
    let dst_base = (addr + displacement) : int;
    let dst_base = (if proc_mode == 0 then {
        if p4 then {
            n32(dst_base)
        } else {
            n64_to_i64(n64(dst_base))
        }
    } else {
        n32(dst_base)
    }) : int;
    (flg, dst_base, increment_rip_by, 0 : int)
}

val x86_effective_addr : (int, bool, int, int, int, int, int, int, int) -> (option(string), int, int, int) effect {escape, rmem, rreg}

function x86_effective_addr (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes, x86) = {
    if 2 == select_address_size(proc_mode, if p4 then true else false, 0) then {
        x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var, 0)
    } else {
        x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes, 0)
    }
}

val alignment_checking_enabled_p : int -> bool effect {escape, rreg}

function alignment_checking_enabled_p x86 = {
    let cr0 = n32(ctri(0, 0)) : int;
    let am = cr0bits_get_am(cr0) : int;
    let ac = (let rflags_var = r_rflags(0) : int in rflagsbits_get_ac(rflags_var)) : int;
    let cpl = (let x86 = 0 : int in segment_selectorbits_get_rpl(seg_visiblei(1, 0))) : int;
    am == 1 & ac == 1 & cpl == 3
}

val x86_operand_from_modr_m_and_sib_bytes : (int, int, int, bool, bool, int, bool, int, int, int, int, int, int, int) -> (option(string), int, int, int, int) effect {escape, rmem, rreg}

function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes, x86) = {
    let (flg0, addr, increment_rip_by, x86) = (if mod_var == 3 then {
        (None() : option(string), 0 : int, 0 : int, 0 : int)
    } else {
        x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes, 0)
    }) : (option(string), int, int, int);
    if is_some(flg0) then {
        (Some("x86-effective-addr-error"), 0 : int, 0 : int, 0 : int, 0 : int)
    } else {
        let (flg2, operand, x86) = (if mod_var == 3 then {
            if reg_type == 0 then {
                (None() : option(string), rgfi_size(operand_size, reg_index(r_m, rex_byte, 0), rex_byte, 0), 0 : int)
            } else {
                (None() : option(string), xmmi_size(operand_size, reg_index(r_m, rex_byte, 0), 0), 0 : int)
            }
        } else {
            let check_alignment? = (inst_ac? & alignment_checking_enabled_p(0)) : bool;
            rme_size(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, 0, struct { mem_ptr? = memory_ptr? })
        }) : (option(string), int, int);
        if is_some(flg2) then {
            (Some("Rm-Size-Error"), 0 : int, 0 : int, 0 : int, 0 : int)
        } else {
            (None() : option(string), operand, increment_rip_by, addr, 0 : int)
        }
    }
}

val x86_operand_to_reg_mem : (int, int, bool, bool, int, int, int, int, int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv, wreg}

function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, rex_byte, r_m, mod_var, x86) = {
    if mod_var == 3 then {
        let x86 = write_rgfi_size(operand_size, reg_index(r_m, rex_byte, 0), operand, rex_byte, 0) : int;
        (None() : option(string), 0 : int)
    } else {
        let check_alignment? = (inst_ac? & alignment_checking_enabled_p(0)) : bool;
        let (flg, x86) = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, 0, struct { mem_ptr? = memory_ptr? }) : (option(string), int);
        (flg, 0 : int)
    }
}

val x86_operand_to_xmm_mem : (int, int, bool, int, int, int, int, int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv, wreg}

function x86_operand_to_xmm_mem (proc_mode, operand_size, inst_ac?, operand, seg_reg, addr, rex_byte, r_m, mod_var, x86) = {
    if mod_var == 3 then {
        let x86 = write_xmmi_size(operand_size, reg_index(r_m, rex_byte, 0), operand, 0) : int;
        (None() : option(string), 0 : int)
    } else {
        let check_alignment? = (inst_ac? & alignment_checking_enabled_p(0)) : bool;
        let (flg, x86) = wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, 0, struct { mem_ptr? = false }) : (option(string), int);
        (flg, 0 : int)
    }
}

val select_operand_size : (int, bool, int, bool, int, bool, bool, bool, int) -> int effect {escape, rreg}

function select_operand_size (proc_mode, byte_operand?, rex_byte, imm?, prefixes, default64?, ignore_rex?, ignore_p3_64?, x86) = {
    if byte_operand? then 1 else if proc_mode == 0 then {
        if logbitp(3, rex_byte) & not_bool(ignore_rex?) then {
            if imm? then 4 else 8
        } else if 102 == prefixes_get_opr(prefixes) & not_bool(ignore_p3_64?) then 2 else if default64? then 8 else 4
    } else {
        let cs_attr = seg_hidden_attri(1, 0) : int;
        let cs_d = code_segment_descriptor_attributesbits_get_d(cs_attr) : int;
        let p3? = (102 == prefixes_get_opr(prefixes)) : bool;
        if cs_d == 1 then {
            if p3? then 2 else 4
        } else if p3? then 4 else 2
    }
}

val select_segment_register : (int, int, bool, int, int, int, int) -> int effect {escape, rreg}

function select_segment_register (proc_mode, p2, p4?, mod_var, r_m, sib, x86) = {
    (match p2 {
      46 => 1,
      54 => 2,
      62 => 3,
      38 => 0,
      100 => 4,
      101 => 5,
      _ => let addr_size = select_address_size(proc_mode, p4?, 0) : int in
        if addr_size == 2 then {
            if not_bool(mod_var == 3) & (r_m == 2 | r_m == 3) then 2 else 3
        } else if (mod_var == 1 | mod_var == 2) & r_m == 5 | not_bool(mod_var == 3) & r_m == 4 & sib_get_base(sib) == 4 then 2 else 3
    }) : int
}

val check_instruction_length : (int, int, int) -> option(int)

function check_instruction_length (start_rip, temp_rip, delta_rip) = {
    let start_rip = start_rip : int;
    let temp_rip = temp_rip : int;
    let delta_rip = delta_rip : int;
    let end_rip = (temp_rip + delta_rip) : int;
    let length = (end_rip - start_rip) : int;
    if 15 < length then {
        Some(length)
    } else {
        None()
    }
}
