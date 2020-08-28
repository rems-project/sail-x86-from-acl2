
val x86_call_e8_op_en_m : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_call_e8_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-CALL-E8-OP/EN-M" : string;
    let offset_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true, 0) : int;
    let (flg0, rel16_32, x86) = rime_size(proc_mode, offset_size, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :RIME-SIZE-OPT-ERROR"))
    } else {
        let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, offset_size, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, next_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let (flg, call_rip) = add_to_iptr(proc_mode, next_rip, rel16_32, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :CALL-RIP-INVALID"))
                } else {
                    let rsp_var = read_sptr(proc_mode, 0) : int;
                    let addr_size = select_address_size(proc_mode, false, 0) : int;
                    let (flg, new_rsp) = add_to_sptr(proc_mode, rsp_var, negate(addr_size), 0) : (option(string), int);
                    if is_some(flg) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"))
                    } else {
                        let (flg, x86) = (if proc_mode == 0 then {
                            wime_size(0, addr_size, new_rsp, 2, next_rip, alignment_checking_enabled_p(0), 0, struct { mem_ptr? = false })
                        } else {
                            wme_size(proc_mode, addr_size, new_rsp, 2, if addr_size == 2 then {
                                n16(next_rip)
                            } else {
                                n32(next_rip)
                            }, alignment_checking_enabled_p(0), 0, struct { mem_ptr? = false })
                        }) : (option(string), int);
                        if is_some(flg) then {
                            throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
                        } else {
                            let x86 = write_iptr(proc_mode, call_rip, 0) : int;
                            let x86 = write_sptr(proc_mode, new_rsp, 0) : int;
                            0
                        }
                    }
                }
            }
        }
    }
}

val x86_call_ff_2_op_en_m : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_call_ff_2_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-CALL-FF/2-OP/EN-M" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let operand_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, call_rip, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, next_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let call_rip = (if proc_mode == 0 then {
                    I64(call_rip)
                } else {
                    call_rip
                }) : int;
                if not_bool(if proc_mode == 0 then {
                    canonical_address_p(call_rip)
                } else {
                    0 <= call_rip & call_rip <= seg_hidden_limiti(1, 0)
                }) then {
                    throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'CALL-RIP']"))
                } else {
                    let rsp_var = read_sptr(proc_mode, 0) : int;
                    let addr_size = select_address_size(proc_mode, false, 0) : int;
                    let (flg, new_rsp) = add_to_sptr(proc_mode, rsp_var, negate(addr_size), 0) : (option(string), int);
                    if is_some(flg) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"))
                    } else {
                        let check_alignment? = alignment_checking_enabled_p(0) : bool;
                        let (flg, x86) = (if operand_size == 8 then {
                            wime_size(proc_mode, operand_size, rsp_var, 2, call_rip, check_alignment?, 0, struct { mem_ptr? = false })
                        } else {
                            wme_size(proc_mode, operand_size, rsp_var, 2, call_rip, check_alignment?, 0, struct { mem_ptr? = false })
                        }) : (option(string), int);
                        if is_some(flg) then {
                            throw(Emsg("Model state error: :STACK-WRITING-ERROR"))
                        } else {
                            let x86 = write_iptr(proc_mode, call_rip, 0) : int;
                            let x86 = write_sptr(proc_mode, new_rsp, 0) : int;
                            0
                        }
                    }
                }
            }
        }
    }
}

val x86_ret : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_ret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-RET" : string;
    let rsp_var = read_sptr(proc_mode, 0) : int;
    let operand_size = select_operand_size(proc_mode, false, rex_byte, false, 0, true, true, true, 0) : int;
    let (flg, new_rsp, x86) = (if opcode == 195 then {
        let (flg1, new_rsp) = add_to_sptr(proc_mode, rsp_var, operand_size, 0) : (option(string), int);
        if is_some(flg1) then {
            (flg1, 0 : int, 0 : int)
        } else {
            (None() : option(string), new_rsp, 0 : int)
        }
    } else {
        let (flg1, imm16, x86) = rme16(proc_mode, temp_rip, 1, ":X", false, 0) : (option(string), int, int);
        if is_some(flg1) then {
            (flg1, 0 : int, 0 : int)
        } else {
            let (flg1, new_rsp) = add_to_sptr(proc_mode, rsp_var, operand_size + imm16, 0) : (option(string), int);
            if is_some(flg1) then {
                (flg1, 0 : int, 0 : int)
            } else {
                (None() : option(string), new_rsp, 0 : int)
            }
        }
    }) : (option(string), int, int);
    if is_some(flg) then {
        throw(Emsg("Model state error: :IMM-RML16-ERROR"))
    } else {
        let badlength? = (opcode == 194 & is_some(check_instruction_length(start_rip, temp_rip, 2))) : bool;
        if badlength? then {
            throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
        } else {
            let check_alignment? = alignment_checking_enabled_p(0) : bool;
            let (flg, tos, x86) = (if operand_size == 8 then {
                rime_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, 0, struct { mem_ptr? = false })
            } else {
                rme_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, 0, struct { mem_ptr? = false })
            }) : (option(string), int, int);
            if is_some(flg) then {
                throw(Emsg("This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`"))
            } else if not_bool(if proc_mode == 0 then {
                canonical_address_p(tos)
            } else {
                let cs_limit = seg_hidden_limiti(1, 0) : int;
                0 <= tos & tos <= cs_limit
            }) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'TOS']"))
            } else {
                let x86 = write_sptr(proc_mode, new_rsp, 0) : int;
                let x86 = write_rip(tos, 0) : int;
                0
            }
        }
    }
}

val x86_leave : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_leave (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-LEAVE" : string;
    let operand_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false, 0) : int;
    let rbp_ebp_bp = rgfi_size(operand_size, 5, 0, 0) : int;
    let inst_ac? = alignment_checking_enabled_p(0) : bool;
    let (flg, sailval, x86) = rme_size(proc_mode, operand_size, I64(rbp_ebp_bp), 2, ":R", inst_ac?, 0, struct { mem_ptr? = false }) : (option(string), int, int);
    if is_some(flg) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':POP-ERROR', 'FLG']"))
    } else {
        let (flg, new_rsp) = add_to_sptr(proc_mode, I64(rbp_ebp_bp), operand_size, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :INVALID-RSP"))
        } else {
            let x86 = write_rgfi_size(operand_size, 5, sailval, rex_byte, 0) : int;
            let x86 = write_sptr(proc_mode, new_rsp, 0) : int;
            let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
            0
        }
    }
}
