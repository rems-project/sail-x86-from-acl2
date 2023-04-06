
val x86_movs : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movs (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVS";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let group_1_prefix : bits(8) = prefixes[rep];
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let df : bits(1) = {
        let rflags_var : rflagsbits = rflags;
        rflags_var[df]
    };
    let counter_addr_size : address_size = select_address_size(proc_mode, Some(prefixes));
    let select_byte_operand : bool = 0xa4 == opcode;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let counter_addr_size_2_4? : bool = counter_addr_size == 2 | counter_addr_size == 4;
    let src_addr : sbits(65) = {
        if counter_addr_size_2_4? then {
            sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x6, rex_byte), 65)
        } else {
            sail_sign_extend(rgfi(6), 65)
        }
    };
    if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(signed(src_addr))) then x86_model_error(":SRC-ADDR-NOT-CANONICAL");
    let inst_ac? : bool = alignment_checking_enabled_p();
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let src : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, truncate(src_addr, 64), None(), seg_reg, ":R", inst_ac?, false);
    let dst_addr : sbits(65) = {
        if counter_addr_size_2_4? then {
            sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
        } else {
            sail_sign_extend(rgfi(7), 65)
        }
    };
    if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(signed(dst_addr))) then x86_model_error(":DST-ADDR-NOT-CANONICAL");
    let original_dst_addr : sbits(64) = truncate(dst_addr, 64);
    let (src_addr, dst_addr) : (int, int) = {
        match operand_size {
          1 => {
              if df == 0b0 then {
                  (1 + signed(truncate(src_addr, 64)), 1 + signed(truncate(dst_addr, 64)))
              } else {
                  (-1 + signed(truncate(src_addr, 64)), -1 + signed(truncate(dst_addr, 64)))
              }
          },
          2 => {
              if df == 0b0 then {
                  (2 + signed(truncate(src_addr, 64)), 2 + signed(truncate(dst_addr, 64)))
              } else {
                  (-2 + signed(truncate(src_addr, 64)), -2 + signed(truncate(dst_addr, 64)))
              }
          },
          4 => {
              if df == 0b0 then {
                  (4 + signed(truncate(src_addr, 64)), 4 + signed(truncate(dst_addr, 64)))
              } else {
                  (-4 + signed(truncate(src_addr, 64)), -4 + signed(truncate(dst_addr, 64)))
              }
          },
          _ => {
              if df == 0b0 then {
                  (8 + signed(truncate(src_addr, 64)), 8 + signed(truncate(dst_addr, 64)))
              } else {
                  (-8 + signed(truncate(src_addr, 64)), -8 + signed(truncate(dst_addr, 64)))
              }
          }
        }
    };
    let src_addr : sbits(65) = bits_of_int(src_addr, 65);
    let dst_addr : sbits(65) = bits_of_int(dst_addr, 65);
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, original_dst_addr, None(), 0, src, inst_ac?, false);
    (match group_1_prefix {
      243 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b0 then {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          } else {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          }
      },
      242 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b1 then {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          } else {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          }
      },
      _ => write_iptr(proc_mode, temp_rip)
    }) : unit;
    (match counter_addr_size {
      2 => write_rgfi_size(0x2, 0x6, sail_zero_extend(truncate(src_addr, 16), 64), rex_byte),
      4 => write_rgfi_size(0x4, 0x6, sail_zero_extend(truncate(src_addr, 32), 64), rex_byte),
      _ => write_rgfi(6, truncate(src_addr, 64))
    }) : unit;
    (match counter_addr_size {
      2 => write_rgfi_size(0x2, 0x7, sail_zero_extend(truncate(dst_addr, 16), 64), rex_byte),
      4 => write_rgfi_size(0x4, 0x7, sail_zero_extend(truncate(dst_addr, 32), 64), rex_byte),
      _ => write_rgfi(7, truncate(dst_addr, 64))
    }) : unit
}

val x86_cmps : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cmps (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CMPS";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let group_1_prefix : bits(8) = prefixes[rep];
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let df : bits(1) = {
        let rflags_var : rflagsbits = rflags;
        rflags_var[df]
    };
    let counter_addr_size : address_size = select_address_size(proc_mode, Some(prefixes));
    let select_byte_operand : bool = 0xa6 == opcode;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let counter_addr_size_2_4? : bool = counter_addr_size == 2 | counter_addr_size == 4;
    let src_addr : sbits(65) = {
        if counter_addr_size_2_4? then {
            sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x6, rex_byte), 65)
        } else {
            sail_sign_extend(rgfi(6), 65)
        }
    };
    if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(signed(src_addr))) then x86_model_error(":SRC-ADDR-NOT-CANONICAL");
    let inst_ac? : bool = alignment_checking_enabled_p();
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let src : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, truncate(src_addr, 64), None(), seg_reg, ":R", inst_ac?, false);
    let dst_addr : sbits(65) = {
        if counter_addr_size_2_4? then {
            sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
        } else {
            sail_sign_extend(rgfi(7), 65)
        }
    };
    if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(signed(dst_addr))) then x86_model_error(":DST-ADDR-NOT-CANONICAL");
    let dst : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, truncate(dst_addr, 64), None(), 0, ":R", inst_ac?, false);
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(operand_size, 4, truncate(dst, 64), truncate(src, 64), input_rflags);
    write_user_rflags(output_rflags, undefined_flags);
    let (src_addr, dst_addr) : (int, int) = {
        match operand_size {
          1 => {
              if df == 0b0 then {
                  (signed(truncate(src_addr, 64)) + 1, signed(truncate(dst_addr, 64)) + 1)
              } else {
                  (signed(truncate(src_addr, 64)) - 1, signed(truncate(dst_addr, 64)) - 1)
              }
          },
          2 => {
              if df == 0b0 then {
                  (signed(truncate(src_addr, 64)) + 2, signed(truncate(dst_addr, 64)) + 2)
              } else {
                  (signed(truncate(src_addr, 64)) - 2, signed(truncate(dst_addr, 64)) - 2)
              }
          },
          4 => {
              if df == 0b0 then {
                  (signed(truncate(src_addr, 64)) + 4, signed(truncate(dst_addr, 64)) + 4)
              } else {
                  (signed(truncate(src_addr, 64)) - 4, signed(truncate(dst_addr, 64)) - 4)
              }
          },
          _ => {
              if df == 0b0 then {
                  (signed(truncate(src_addr, 64)) + 8, signed(truncate(dst_addr, 64)) + 8)
              } else {
                  (signed(truncate(src_addr, 64)) - 8, signed(truncate(dst_addr, 64)) - 8)
              }
          }
        }
    };
    let src_addr : sbits(65) = bits_of_int(src_addr, 65);
    let dst_addr : sbits(65) = bits_of_int(dst_addr, 65);
    (match group_1_prefix {
      243 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b0 then {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          } else {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          }
      },
      242 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b1 then {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          } else {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          }
      },
      _ => write_iptr(proc_mode, temp_rip)
    }) : unit;
    (match counter_addr_size {
      2 => write_rgfi_size(0x2, 0x6, sail_zero_extend(truncate(src_addr, 16), 64), rex_byte),
      4 => write_rgfi_size(0x4, 0x6, sail_zero_extend(truncate(src_addr, 32), 64), rex_byte),
      _ => write_rgfi(6, truncate(src_addr, 64))
    }) : unit;
    (match counter_addr_size {
      2 => write_rgfi_size(0x2, 0x7, sail_zero_extend(truncate(dst_addr, 16), 64), rex_byte),
      4 => write_rgfi_size(0x4, 0x7, sail_zero_extend(truncate(dst_addr, 32), 64), rex_byte),
      _ => write_rgfi(7, truncate(dst_addr, 64))
    }) : unit
}

val x86_stos : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_stos (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-STOS";
    let group_1_prefix : bits(8) = prefixes[seg];
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p4? : bool = 0x67 == prefixes[adr];
    let df : bits(1) = {
        let rflags_var : rflagsbits = rflags;
        rflags_var[df]
    };
    let counter_addr_size : address_size = select_address_size(proc_mode, Some(prefixes));
    let counter_addr_size_2_4? : bool = counter_addr_size == 2 | counter_addr_size == 4;
    let dst_addr : sbits(65) = {
        if counter_addr_size_2_4? then {
            sail_zero_extend(rgfi_size(bits_of_int(counter_addr_size, 4), 0x7, rex_byte), 65)
        } else {
            sail_sign_extend(rgfi(7), 65)
        }
    };
    if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(signed(dst_addr))) then x86_model_error(":DST-ADDR-NOT-CANONICAL");
    let select_byte_operand : bool = 0xaa == opcode;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let rax_var : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x0, rex_byte);
    let inst_ac? : bool = alignment_checking_enabled_p();
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, truncate(dst_addr, 64), None(), 0, sail_zero_extend(rax_var, 128), inst_ac?, false);
    let dst_addr : sbits(65) = {
        match operand_size {
          1 => {
              if df == 0b0 then {
                  bits_of_int(1 + signed(truncate(dst_addr, 64)), 65)
              } else {
                  bits_of_int(-1 + signed(truncate(dst_addr, 64)), 65)
              }
          },
          2 => {
              if df == 0b0 then {
                  bits_of_int(2 + signed(truncate(dst_addr, 64)), 65)
              } else {
                  bits_of_int(-2 + signed(truncate(dst_addr, 64)), 65)
              }
          },
          4 => {
              if df == 0b0 then {
                  bits_of_int(4 + signed(truncate(dst_addr, 64)), 65)
              } else {
                  bits_of_int(-4 + signed(truncate(dst_addr, 64)), 65)
              }
          },
          _ => {
              if df == 0b0 then {
                  bits_of_int(8 + signed(truncate(dst_addr, 64)), 65)
              } else {
                  bits_of_int(-8 + signed(truncate(dst_addr, 64)), 65)
              }
          }
        }
    };
    (match group_1_prefix {
      243 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b0 then {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          } else {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          }
      },
      242 => {
          let counter : bits(64) = rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, rex_byte);
          let counter : bits(64) = sail_mask(64, trunc(counter_addr_size, bits_of_int(unsigned(counter) - 1, 64)));
          if counter == 0x0000000000000000 | (let rflags_var : rflagsbits = rflags in rflags_var[zf]) == 0b1 then {
              {
                  write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte);
                  write_iptr(proc_mode, temp_rip)
              }
          } else {
              write_rgfi_size(bits_of_int(counter_addr_size, 4), 0x1, counter, rex_byte)
          }
      },
      _ => write_iptr(proc_mode, temp_rip)
    }) : unit;
    (match counter_addr_size {
      2 => write_rgfi_size(0x2, 0x7, sail_zero_extend(truncate(dst_addr, 16), 64), rex_byte),
      4 => write_rgfi_size(0x4, 0x7, sail_zero_extend(truncate(dst_addr, 32), 64), rex_byte),
      _ => write_rgfi(7, truncate(dst_addr, 64))
    }) : unit
}
