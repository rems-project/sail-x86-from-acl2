
val x86_movs : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_movs (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-MOVS" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let group_1_prefix = prefixes_get_rep(prefixes) : bits(8);
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let p2 = prefixes_get_seg(prefixes) : bits(8);
        let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
        let df = the_bits(1, let rflags_var = rflags : bits(32) in the_bits(2, rflagsbits_get_df(rflags_var))) : bits(1);
        let counter_addr_size = the_range(2, 8, select_address_size(proc_mode, p4?)) : {|8, 2, 4|};
        let select_byte_operand = (get_slice_int(8, 164, 0) == opcode) : bool;
        let operand_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
        let counter_addr_size_2_4? = (get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 2, 0) | get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 4, 0)) : bool;
        let src_addr = (if counter_addr_size_2_4? then {
            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 6, 0), rex_byte)
        } else {
            rgfi(6)
        }) : bits(64);
        if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(the_bits(48, src_addr))) then {
            throw(Emsg("Model state error: :SRC-ADDR-NOT-CANONICAL"))
        } else {
            let inst_ac? = alignment_checking_enabled_p() : bool;
            let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
            let (flg0, src) = rme_size(proc_mode, operand_size, src_addr, seg_reg, ":R", inst_ac?, struct { mem_ptr? = false }) : (option(string), int);
            if is_some(flg0) then {
                throw(Emsg("Model state error: :SRC-RME-SIZE-ERROR"))
            } else {
                let dst_addr = (if counter_addr_size_2_4? then {
                    rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 7, 0), rex_byte)
                } else {
                    rgfi(7)
                }) : bits(64);
                if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(the_bits(48, dst_addr))) then {
                    throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
                } else {
                    let original_dst_addr = the_sbits(48, dst_addr) : sbits(48);
                    let (src_addr, dst_addr) = (match operand_size {
                      1 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                          (1 + signed(the_sbits(48, src_addr)), 1 + signed(the_sbits(48, dst_addr)))
                      } else {
                          (-1 + signed(the_sbits(48, src_addr)), -1 + signed(the_sbits(48, dst_addr)))
                      },
                      2 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                          (2 + signed(the_sbits(48, src_addr)), 2 + signed(the_sbits(48, dst_addr)))
                      } else {
                          (-2 + signed(the_sbits(48, src_addr)), -2 + signed(the_sbits(48, dst_addr)))
                      },
                      4 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                          (4 + signed(the_sbits(48, src_addr)), 4 + signed(the_sbits(48, dst_addr)))
                      } else {
                          (-4 + signed(the_sbits(48, src_addr)), -4 + signed(the_sbits(48, dst_addr)))
                      },
                      _ => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                          (8 + signed(the_sbits(48, src_addr)), 8 + signed(the_sbits(48, dst_addr)))
                      } else {
                          (-8 + signed(the_sbits(48, src_addr)), -8 + signed(the_sbits(48, dst_addr)))
                      }
                    }) : (int, int) : (int, int);
                    let src_addr = the_sbits(49, src_addr) : sbits(49);
                    let dst_addr = the_sbits(49, dst_addr) : sbits(49);
                    let flg1 = wme_size(proc_mode, operand_size, the_sbits(64, original_dst_addr), 0, the_nat(src), inst_ac?, struct { mem_ptr? = false }) : option(string);
                    if is_some(flg1) then {
                        throw(Emsg("Model state error: :WME-SIZE-ERROR"))
                    } else {
                        (match group_1_prefix {
                          243 => let counter =
                            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                          in
                          let counter =
                            sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                          in
                            if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                              rflags : bits(32)
                            in
                              the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 0, 0) then {
                                write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                            } else {
                                {
                                    write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                                    write_iptr(proc_mode, temp_rip)
                                }
                            },
                          242 => let counter =
                            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                          in
                          let counter =
                            sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                          in
                            if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                              rflags : bits(32)
                            in
                              the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 1, 0) then {
                                {
                                    write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                                    write_iptr(proc_mode, temp_rip)
                                }
                            } else {
                                write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                            },
                          _ => write_iptr(proc_mode, temp_rip)
                        }) : unit;
                        (match counter_addr_size {
                          2 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 6, 0), unsigned(n16(the_bits(16, src_addr))), rex_byte),
                          4 => write_rgfi_size(get_slice_int(4, 4, 0), get_slice_int(4, 6, 0), unsigned(n32(the_bits(32, src_addr))), rex_byte),
                          _ => write_rgfi(6, the_bits(64, src_addr))
                        }) : unit;
                        (match counter_addr_size {
                          2 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 7, 0), unsigned(n16(the_bits(16, dst_addr))), rex_byte),
                          4 => write_rgfi_size(get_slice_int(4, 4, 0), get_slice_int(4, 7, 0), unsigned(n32(the_bits(32, dst_addr))), rex_byte),
                          _ => write_rgfi(7, the_bits(64, dst_addr))
                        }) : unit
                    }
                }
            }
        }
    }
}

val x86_cmps : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cmps (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-CMPS" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let group_1_prefix = prefixes_get_rep(prefixes) : bits(8);
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let p2 = prefixes_get_seg(prefixes) : bits(8);
        let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
        let df = the_bits(1, let rflags_var = rflags : bits(32) in the_bits(2, rflagsbits_get_df(rflags_var))) : bits(1);
        let counter_addr_size = the_range(2, 8, select_address_size(proc_mode, p4?)) : {|8, 2, 4|};
        let select_byte_operand = (get_slice_int(8, 166, 0) == opcode) : bool;
        let operand_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
        let counter_addr_size_2_4? = (get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 2, 0) | get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 4, 0)) : bool;
        let src_addr = (if counter_addr_size_2_4? then {
            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 6, 0), rex_byte)
        } else {
            rgfi(6)
        }) : bits(64);
        if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(the_bits(48, src_addr))) then {
            throw(Emsg("Model state error: :SRC-ADDR-NOT-CANONICAL"))
        } else {
            let inst_ac? = alignment_checking_enabled_p() : bool;
            let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
            let (flg0, src) = rme_size(proc_mode, operand_size, src_addr, seg_reg, ":R", inst_ac?, struct { mem_ptr? = false }) : (option(string), int);
            if is_some(flg0) then {
                throw(Emsg("Model state error: :SRC-RME-SIZE-ERROR"))
            } else {
                let dst_addr = (if counter_addr_size_2_4? then {
                    rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 7, 0), rex_byte)
                } else {
                    rgfi(7)
                }) : bits(64);
                if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(the_bits(48, dst_addr))) then {
                    throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
                } else {
                    let (flg0, dst) = rme_size(proc_mode, operand_size, dst_addr, 0, ":R", inst_ac?, struct { mem_ptr? = false }) : (option(string), int);
                    if is_some(flg0) then {
                        throw(Emsg("Model state error: :DST-RME-SIZE-ERROR"))
                    } else {
                        let input_rflags = rflags : bits(32);
                        let (result, output_rflags, undefined_flags) = (match operand_size {
                          1 => let (elem0, elem1, elem2) =
                            gpr_arith_logic_spec_1(4, get_slice_int(8, dst, 0), get_slice_int(8, src, 0), input_rflags) : (bits(8), bits(32), bits(32))
                          in
                            (the_bits(64, elem0), elem1, elem2),
                          2 => let (elem0, elem1, elem2) =
                            gpr_arith_logic_spec_2(4, get_slice_int(16, dst, 0), get_slice_int(16, src, 0), input_rflags) : (bits(16), bits(32), bits(32))
                          in
                            (the_bits(64, elem0), elem1, elem2),
                          4 => let (elem0, elem1, elem2) =
                            gpr_arith_logic_spec_4(4, get_slice_int(32, dst, 0), get_slice_int(32, src, 0), input_rflags) : (bits(32), bits(32), bits(32))
                          in
                            (the_bits(64, elem0), elem1, elem2),
                          _ => gpr_arith_logic_spec_8(4, get_slice_int(64, dst, 0), get_slice_int(64, src, 0), input_rflags)
                        }) : (bits(64), bits(32), bits(32)) : (bits(64), bits(32), bits(32));
                        let output_rflags = output_rflags : bits(32);
                        let undefined_flags = undefined_flags : bits(32);
                        write_user_rflags(output_rflags, undefined_flags);
                        let (src_addr, dst_addr) = (match operand_size {
                          1 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                              (signed(the_sbits(48, src_addr)) + 1, signed(the_sbits(48, dst_addr)) + 1)
                          } else {
                              (signed(the_sbits(48, src_addr)) - 1, signed(the_sbits(48, dst_addr)) - 1)
                          },
                          2 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                              (signed(the_sbits(48, src_addr)) + 2, signed(the_sbits(48, dst_addr)) + 2)
                          } else {
                              (signed(the_sbits(48, src_addr)) - 2, signed(the_sbits(48, dst_addr)) - 2)
                          },
                          4 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                              (signed(the_sbits(48, src_addr)) + 4, signed(the_sbits(48, dst_addr)) + 4)
                          } else {
                              (signed(the_sbits(48, src_addr)) - 4, signed(the_sbits(48, dst_addr)) - 4)
                          },
                          _ => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                              (signed(the_sbits(48, src_addr)) + 8, signed(the_sbits(48, dst_addr)) + 8)
                          } else {
                              (signed(the_sbits(48, src_addr)) - 8, signed(the_sbits(48, dst_addr)) - 8)
                          }
                        }) : (int, int) : (int, int);
                        let src_addr = the_sbits(49, src_addr) : sbits(49);
                        let dst_addr = the_sbits(49, dst_addr) : sbits(49);
                        (match group_1_prefix {
                          243 => let counter =
                            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                          in
                          let counter =
                            sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                          in
                            if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                              rflags : bits(32)
                            in
                              the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 0, 0) then {
                                write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                            } else {
                                {
                                    write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                                    write_iptr(proc_mode, temp_rip)
                                }
                            },
                          242 => let counter =
                            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                          in
                          let counter =
                            sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                          in
                            if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                              rflags : bits(32)
                            in
                              the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 1, 0) then {
                                {
                                    write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                                    write_iptr(proc_mode, temp_rip)
                                }
                            } else {
                                write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                            },
                          _ => write_iptr(proc_mode, temp_rip)
                        }) : unit;
                        (match counter_addr_size {
                          2 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 6, 0), unsigned(n16(the_bits(16, src_addr))), rex_byte),
                          4 => write_rgfi_size(get_slice_int(4, 4, 0), get_slice_int(4, 6, 0), unsigned(n32(the_bits(32, src_addr))), rex_byte),
                          _ => write_rgfi(6, the_bits(64, src_addr))
                        }) : unit;
                        (match counter_addr_size {
                          2 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 7, 0), unsigned(n16(the_bits(16, dst_addr))), rex_byte),
                          4 => write_rgfi_size(get_slice_int(4, 4, 0), get_slice_int(4, 7, 0), unsigned(n32(the_bits(32, dst_addr))), rex_byte),
                          _ => write_rgfi(7, the_bits(64, dst_addr))
                        }) : unit
                    }
                }
            }
        }
    }
}

val x86_stos : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_stos (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-STOS" : string;
    let group_1_prefix = prefixes_get_seg(prefixes) : bits(8);
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
        let df = the_bits(1, let rflags_var = rflags : bits(32) in the_bits(2, rflagsbits_get_df(rflags_var))) : bits(1);
        let counter_addr_size = the_range(2, 8, select_address_size(proc_mode, p4?)) : {|8, 2, 4|};
        let counter_addr_size_2_4? = (get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 2, 0) | get_slice_int(8, counter_addr_size, 0) == get_slice_int(8, 4, 0)) : bool;
        let dst_addr = (if counter_addr_size_2_4? then {
            rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 7, 0), rex_byte)
        } else {
            rgfi(7)
        }) : bits(64);
        if not_bool(counter_addr_size_2_4?) & not_bool(canonical_address_p(the_bits(48, dst_addr))) then {
            throw(Emsg("Model state error: :DST-ADDR-NOT-CANONICAL"))
        } else {
            let select_byte_operand = (get_slice_int(8, 170, 0) == opcode) : bool;
            let operand_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
            let rax_var = rgfi_size(get_slice_int(4, operand_size, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
            let inst_ac? = alignment_checking_enabled_p() : bool;
            let flg0 = wme_size(proc_mode, operand_size, dst_addr, 0, unsigned(rax_var), inst_ac?, struct { mem_ptr? = false }) : option(string);
            if is_some(flg0) then {
                throw(Emsg("Model state error: :WME-SIZE-ERROR"))
            } else {
                let dst_addr = the_sbits(49, (match operand_size {
                  1 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                      1 + signed(the_sbits(48, dst_addr))
                  } else {
                      -1 + signed(the_sbits(48, dst_addr))
                  },
                  2 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                      2 + signed(the_sbits(48, dst_addr))
                  } else {
                      -2 + signed(the_sbits(48, dst_addr))
                  },
                  4 => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                      4 + signed(the_sbits(48, dst_addr))
                  } else {
                      -4 + signed(the_sbits(48, dst_addr))
                  },
                  _ => if the_bits(8, df) == get_slice_int(8, 0, 0) then {
                      8 + signed(the_sbits(48, dst_addr))
                  } else {
                      -8 + signed(the_sbits(48, dst_addr))
                  }
                }) : int) : sbits(49);
                (match group_1_prefix {
                  243 => let counter =
                    rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                  in
                  let counter =
                    sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                  in
                    if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                      rflags : bits(32)
                    in
                      the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 0, 0) then {
                        write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                    } else {
                        {
                            write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                            write_iptr(proc_mode, temp_rip)
                        }
                    },
                  242 => let counter =
                    rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), rex_byte) : bits(64)
                  in
                  let counter =
                    sail_mask(64, trunc(counter_addr_size, get_slice_int(64, unsigned(counter) - 1, 0))) : bits(64)
                  in
                    if the_bits(8, counter) == get_slice_int(8, 0, 0) | the_bits(8, the_bits(1, let rflags_var =
                      rflags : bits(32)
                    in
                      the_bits(2, rflagsbits_get_zf(rflags_var)))) == get_slice_int(8, 1, 0) then {
                        {
                            write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
                            write_iptr(proc_mode, temp_rip)
                        }
                    } else {
                        write_rgfi_size(get_slice_int(4, counter_addr_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte)
                    },
                  _ => write_iptr(proc_mode, temp_rip)
                }) : unit;
                (match counter_addr_size {
                  2 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 7, 0), unsigned(n16(the_bits(16, dst_addr))), rex_byte),
                  4 => write_rgfi_size(get_slice_int(4, 4, 0), get_slice_int(4, 7, 0), unsigned(n32(the_bits(32, dst_addr))), rex_byte),
                  _ => write_rgfi(7, the_bits(64, dst_addr))
                }) : unit
            }
        }
    }
}
