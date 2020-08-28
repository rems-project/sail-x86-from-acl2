
val x86_near_jmp_op_en_d : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_near_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-NEAR-JMP-OP/EN-D" : string;
    let byte_operand? = (opcode == 235) : bool;
    let offset_size = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, true, true, 0) : int;
    let (flg, offset, x86) = rime_size(proc_mode, offset_size, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
    if is_some(flg) then {
        throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
    } else {
        let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, offset_size, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, next_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, next_rip, offset, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :VIRTUAL-MEMORY-ERROR"))
                } else {
                    let temp_rip = (if offset_size == 2 then {
                        binary_logand(65535, temp_rip)
                    } else {
                        temp_rip
                    }) : int;
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_near_jmp_op_en_m : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_near_jmp_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-NEAR-JMP-OP/EN-M" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let operand_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg, jmp_addr, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let jmp_addr = (if proc_mode == 0 then {
                    I64(jmp_addr)
                } else {
                    jmp_addr
                }) : int;
                if not_bool(if proc_mode == 0 then {
                    canonical_address_p(jmp_addr)
                } else {
                    let cs_limit = seg_hidden_limiti(1, 0) : int;
                    0 <= jmp_addr & jmp_addr <= cs_limit
                }) then {
                    throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'JMP-ADDR']"))
                } else {
                    let x86 = write_iptr(proc_mode, jmp_addr, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_far_jmp_op_en_d : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_far_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-FAR-JMP-OP/EN-D" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    if mod_var == 3 then {
        throw(Emsg("Model state error: :SOURCE-OPERAND-NOT-MEMORY-LOCATION"))
    } else {
        let p2 = prefixes_get_seg(prefixes) : int;
        let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
        let offset_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0) : int;
        let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
        let inst_ac? = true : bool;
        let (flg, mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, 2 + offset_size, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, increment_rip_by) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let selector = n16(mem) : int;
                let offset = unsigned(get_slice_int(ash(offset_size, 3), mem, 16)) : int;
                let sel_index = segment_selectorbits_get_index(selector) : int;
                let sel_ti = segment_selectorbits_get_ti(selector) : int;
                let sel_rpl = segment_selectorbits_get_rpl(selector) : int;
                if sel_ti == 0 & sel_index == 0 then {
                    throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':GP-NULLSELECTOR', '0']"))
                } else {
                    let (dt_base_addr, dt_limit) = (if sel_ti == 0 then {
                        let gdtr = stri(0, 0) : int;
                        let gdtr_base = (if proc_mode == 0 then {
                            gdtr_idtrbits_get_base_addr(gdtr)
                        } else {
                            n32(gdtr_idtrbits_get_base_addr(gdtr))
                        }) : int;
                        let gdtr_limit = gdtr_idtrbits_get_limit(gdtr) : int;
                        (gdtr_base, gdtr_limit)
                    } else {
                        let ldtr_base = ssr_hidden_basei(0, 0) : int;
                        let ldtr_base = (if proc_mode == 0 then {
                            ldtr_base
                        } else {
                            n32(ldtr_base)
                        }) : int;
                        let ldtr_limit = ssr_hidden_limiti(0, 0) : int;
                        (ldtr_base, ldtr_limit)
                    }) : (int, int);
                    let largest_address = (ash(sel_index, 3) + 7) : int;
                    if dt_limit < largest_address then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'SELECTOR', 'DT-BASE-ADDR', 'DT-LIMIT']]"))
                    } else {
                        let descriptor_addr = (dt_base_addr + ash(sel_index, 3)) : int;
                        if not_bool(canonical_address_p(descriptor_addr)) then {
                            throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
                        } else {
                            let (flg, descriptor, x86) = rml_size(8, descriptor_addr, ":X", 0) : (option(string), int, int);
                            if is_some(flg) then {
                                throw(Emsg("Model state error: :RML-SIZE-ERROR"))
                            } else {
                                let s = code_segment_descriptorbits_get_s(descriptor) : int;
                                if s == 1 then {
                                    let msb_of_type = code_segment_descriptorbits_get_msb_of_type(descriptor) : int;
                                    if not_bool(msb_of_type == 1) then {
                                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':JMP-FAR-DATA-SEGMENT', 'SEL-INDEX']"))
                                    } else {
                                        let d = code_segment_descriptorbits_get_d(descriptor) : int;
                                        if proc_mode == 0 & d == 1 then {
                                            throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':CS.D=1-IN-64-BIT-MODE', 'SEL-INDEX']"))
                                        } else {
                                            let cpl = (let x86 = 0 : int in
                                              segment_selectorbits_get_rpl(seg_visiblei(1, 0))) : int;
                                            let dpl = code_segment_descriptorbits_get_dpl(descriptor) : int;
                                            let c = code_segment_descriptorbits_get_c(descriptor) : int;
                                            let allowed = (if c == 1 then {
                                                dpl <= cpl
                                            } else {
                                                sel_rpl <= cpl & cpl == dpl
                                            }) : bool;
                                            if not_bool(allowed) then {
                                                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':DPL', 'DPL', ['ACONS', ':CPL', 'CPL', ['ACONS', ':RPL', 'SEL-RPL', 'NIL']]]]"))
                                            } else {
                                                let p = code_segment_descriptorbits_get_p(descriptor) : int;
                                                if not_bool(p == 1) then {
                                                    throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':NP', 'SEL-INDEX', ':CODE-SEGMENT-NOT-PRESENT', 'SEL-INDEX']"))
                                                } else {
                                                    let jmp_addr = (match offset_size {
                                                      2 => n16(offset),
                                                      4 => n32(offset),
                                                      _ => I64(offset)
                                                    }) : int : int;
                                                    let jmp_addr_ok = (if proc_mode == 0 then {
                                                        canonical_address_p(jmp_addr)
                                                    } else {
                                                        let limit15_0 = code_segment_descriptorbits_get_limit15_0(descriptor) : int;
                                                        let limit19_16 = code_segment_descriptorbits_get_limit19_16(descriptor) : int;
                                                        let limit = changeBits(ash(limit19_16, 16), 0, 16, limit15_0) : int;
                                                        let g = code_segment_descriptorbits_get_g(descriptor) : int;
                                                        let max_offset : int = (if g == 1 then {
                                                            ash(limit, 12) - 1
                                                        } else {
                                                            limit
                                                        }) : int;
                                                        jmp_addr < max_offset : int
                                                    }) : bool;
                                                    if not_bool(jmp_addr_ok) then {
                                                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':NONCANONICAL-OR-OUTSIDE-SEGMENT-LIMIT', 'JMP-ADDR']"))
                                                    } else {
                                                        let new_cs_visible = set_segment_selectorbits_get_rpl(cpl, selector) : int;
                                                        let x86 = write_seg_visiblei(1, new_cs_visible, 0) : int;
                                                        let x86 = write_seg_hidden_basei(1, dt_base_addr, 0) : int;
                                                        let x86 = write_seg_hidden_limiti(1, dt_limit, 0) : int;
                                                        let x86 = write_seg_hidden_attri(1, make_code_segment_attr_field(descriptor), 0) : int;
                                                        let x86 = write_iptr(proc_mode, jmp_addr, 0) : int;
                                                        0
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else if not_bool(proc_mode == 0) then {
                                    throw(Emsg("Model state error: :FAR-JMP-SYSTEM-UNIMPLEMENTED-IN-32-BIT-MODE"))
                                } else {
                                    let largest_address = (ash(sel_index, 3) + 15) : int;
                                    if dt_limit < largest_address then {
                                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'SELECTOR', 'DT-BASE-ADDR', 'DT-LIMIT']]"))
                                    } else {
                                        let (flg, descriptor, x86) = rml_size(16, descriptor_addr, ":X", 0) : (option(string), int, int);
                                        if is_some(flg) then {
                                            throw(Emsg("Model state error: :RML-SIZE-ERROR"))
                                        } else {
                                            let (call_gate_desc?, reason2) = ia32e_valid_call_gate_segment_descriptor_p(descriptor) : (bool, (string, int));
                                            if call_gate_desc? then {
                                                let cpl = (let x86 = 0 : int in
                                                  segment_selectorbits_get_rpl(seg_visiblei(1, 0))) : int;
                                                let dpl = call_gate_descriptorbits_get_dpl(descriptor) : int;
                                                if not_bool(cpl <= dpl & sel_rpl <= dpl) then {
                                                    throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':DPL', 'DPL', ['ACONS', ':CPL', 'CPL', ['ACONS', ':RPL', 'SEL-RPL', 'NIL']]]]"))
                                                } else {
                                                    let cs_selector = call_gate_descriptorbits_get_selector(descriptor) : int;
                                                    let cs_sel_index = segment_selectorbits_get_index(cs_selector) : int;
                                                    let cs_sel_ti = segment_selectorbits_get_ti(cs_selector) : int;
                                                    let cs_sel_rpl = segment_selectorbits_get_rpl(cs_selector) : int;
                                                    if cs_sel_ti == 0 & cs_sel_index == 0 then {
                                                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':CALL-GATE-CODE-SEGMENT-NULLSELECTOR', '0']"))
                                                    } else {
                                                        let (cs_dt_base_addr, cs_dt_limit) = (if sel_ti == 0 then {
                                                            let gdtr = stri(0, 0) : int;
                                                            let gdtr_base = gdtr_idtrbits_get_base_addr(gdtr) : int;
                                                            let gdtr_base = (if proc_mode == 0 then {
                                                                gdtr_base
                                                            } else {
                                                                n32(gdtr_base)
                                                            }) : int;
                                                            let gdtr_limit = gdtr_idtrbits_get_limit(gdtr) : int;
                                                            (gdtr_base, gdtr_limit)
                                                        } else {
                                                            let ldtr_base = ssr_hidden_basei(0, 0) : int;
                                                            let ldtr_base = (if proc_mode == 0 then {
                                                                ldtr_base
                                                            } else {
                                                                n32(ldtr_base)
                                                            }) : int;
                                                            let ldtr_limit = ssr_hidden_limiti(0, 0) : int;
                                                            (ldtr_base, ldtr_limit)
                                                        }) : (int, int);
                                                        let largest_address = (ash(cs_sel_index, 3) + 7) : int;
                                                        if cs_dt_limit < largest_address then {
                                                            throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'CS-SELECTOR', 'CS-DT-BASE-ADDR', 'CS-DT-LIMIT']]"))
                                                        } else {
                                                            let cs_descriptor_addr = (cs_dt_base_addr + ash(cs_sel_index, 3)) : int;
                                                            if not_bool(canonical_address_p(cs_descriptor_addr)) then {
                                                                throw(Emsg("Model state error: :CS-DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
                                                            } else {
                                                                let (flg, cs_descriptor, x86) = rml_size(8, cs_descriptor_addr, ":X", 0) : (option(string), int, int);
                                                                if is_some(flg) then {
                                                                    throw(Emsg("Model state error: :RML-SIZE-ERROR"))
                                                                } else {
                                                                    let (valid?, reason) = ia32e_valid_code_segment_descriptor_p(cs_descriptor) : (bool, (string, int));
                                                                    if not_bool(valid?) then {
                                                                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':CALL-GATE-CODE-SEGMENT-DESCRIPTOR-INVALID', ['CONS', 'REASON', 'CS-DESCRIPTOR']]"))
                                                                    } else {
                                                                        let cs_dpl = code_segment_descriptorbits_get_dpl(cs_descriptor) : int;
                                                                        let c_bit = code_segment_descriptorbits_get_c(cs_descriptor) : int;
                                                                        if c_bit == 1 & not_bool(cs_dpl <= cpl) | c_bit == 0 & not_bool(cs_dpl == cpl) then {
                                                                            throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':C-BIT', 'C-BIT', ['ACONS', ':CPL', 'CPL', ['ACONS', ':CS-DPL', 'CS-DPL', 'NIL']]]]"))
                                                                        } else {
                                                                            let call_gate_offset15_0 = call_gate_descriptorbits_get_offset15_0(descriptor) : int;
                                                                            let call_gate_offset31_16 = call_gate_descriptorbits_get_offset31_16(descriptor) : int;
                                                                            let call_gate_offset63_32 = call_gate_descriptorbits_get_offset63_32(descriptor) : int;
                                                                            let call_gate_offset31_0 = changeBits(ash(call_gate_offset31_16, 16), 0, 16, call_gate_offset15_0) : int;
                                                                            let call_gate_offset = changeBits(ash(call_gate_offset63_32, 32), 0, 32, call_gate_offset31_0) : int;
                                                                            let jmp_addr = (match offset_size {
                                                                              2 => n16(call_gate_offset),
                                                                              4 => n32(call_gate_offset),
                                                                              _ => call_gate_offset
                                                                            }) : int : int;
                                                                            if not_bool(canonical_address_p(jmp_addr)) then {
                                                                                throw(Emsg("Model state error: :TARGET-OFFSET-VIRTUAL-MEMORY-ERROR"))
                                                                            } else {
                                                                                let new_cs_visible = set_segment_selectorbits_get_rpl(cpl, cs_selector) : int;
                                                                                let x86 = write_seg_visiblei(1, new_cs_visible, 0) : int;
                                                                                let x86 = write_seg_hidden_basei(1, cs_dt_base_addr, 0) : int;
                                                                                let x86 = write_seg_hidden_limiti(1, cs_dt_limit, 0) : int;
                                                                                let x86 = write_seg_hidden_attri(1, make_code_segment_attr_field(cs_descriptor), 0) : int;
                                                                                let x86 = write_iptr(proc_mode, jmp_addr, 0) : int;
                                                                                0
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':EITHER-BOTH-CODE-SEGMENT-OR-CALL-GATE-ARE-ABSENT-OR-SOME-OTHER-DESCRIPTOR-IS-PRESENT', ['CONS', 'REASON2', 'DESCRIPTOR']]"))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

val x86_loop : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_loop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-LOOP" : string;
    let badlength? = check_instruction_length(start_rip, temp_rip, 1) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
        let counter_size = select_address_size(proc_mode, p4?, 0) : int;
        let counter = rgfi_size(counter_size, 1, rex_byte, 0) : int;
        let counter = trunc(counter_size, counter - 1) : int;
        let zf = (let rflags_var = r_rflags(0) : int in rflagsbits_get_zf(rflags_var)) : int;
        let branch_cond = (if opcode == 226 then {
            not_bool(counter == 0)
        } else if opcode == 225 then {
            zf == 1 & not_bool(counter == 0)
        } else {
            zf == 0 & not_bool(counter == 0)
        }) : bool;
        if branch_cond then {
            let (flg, rel8, x86) = rime_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
            } else {
                let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, 1 + rel8, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let x86 = write_rgfi_size(counter_size, 1, counter, rex_byte, 0) : int;
                    let x86 = write_iptr(proc_mode, next_rip, 0) : int;
                    0
                }
            }
        } else {
            let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, 1, 0) : (option(string), int);
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
            } else {
                let x86 = write_rgfi_size(counter_size, 1, counter, rex_byte, 0) : int;
                let x86 = write_iptr(proc_mode, next_rip, 0) : int;
                0
            }
        }
    }
}
