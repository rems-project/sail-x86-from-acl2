
val x86_movss_movsd_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_movss_movsd_op_en_rm (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVSS/MOVSD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let operand_size = (if sp_dp == 1 then 8 else 4) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let operand_size = (if mod_var == 3 then {
                    operand_size
                } else 16) : int;
                let x86 = write_xmmi_size(operand_size, xmm_index, xmm_mem, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_movss_movsd_op_en_mr : (int, int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_movss_movsd_op_en_mr (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVSS/MOVSD-OP/EN-MR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let operand_size = (if sp_dp == 1 then 8 else 4) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(operand_size, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let (flg0, addr, increment_rip_by, x86) = (if mod_var == 3 then {
        (None() : option(string), 0 : int, 0 : int, 0 : int)
    } else {
        x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)
    }) : (option(string), int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let inst_ac? = true : bool;
                let (flg1, x86) = x86_operand_to_xmm_mem(proc_mode, operand_size, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_movaps_movapd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_movaps_movapd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVAPS/MOVAPD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let x86 = write_xmmi_size(16, xmm_index, xmm_mem, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_movaps_movapd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_movaps_movapd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVAPS/MOVAPD-OP/EN-MR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let (flg0, addr, increment_rip_by, x86) = (if mod_var == 3 then {
        (None() : option(string), 0 : int, 0 : int, 0 : int)
    } else {
        x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)
    }) : (option(string), int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let inst_ac? = true : bool;
                let (flg1, x86) = x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_movups_movupd_movdqu_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_movups_movupd_movdqu_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let x86 = write_xmmi_size(16, xmm_index, xmm_mem, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_movups_movupd_movdqu_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_movups_movupd_movdqu_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-MR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let (flg0, addr, increment_rip_by, x86) = (if mod_var == 3 then {
        (None() : option(string), 0 : int, 0 : int, 0 : int)
    } else {
        x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)
    }) : (option(string), int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let inst_ac? = true : bool;
                let (flg1, x86) = x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_movlps_movlpd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_movlps_movlpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVLPS/MOVLPD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let x86 = write_xmmi_size(8, xmm_index, mem, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_movlps_movlpd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_movlps_movlpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVLPS/MOVLPD-OP/EN-MR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(8, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let (flg0, addr, increment_rip_by, x86) = x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let inst_ac? = true : bool;
                let (flg1, x86) = x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_movhps_movhpd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_movhps_movhpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVHPS/MOVHPD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, mem, increment_rip_by, v_addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let low_qword = xmmi_size(8, xmm_index, 0) : int;
                let result = merge_2_u64s(mem, low_qword) : int;
                let x86 = write_xmmi_size(16, xmm_index, result, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_movhps_movhpd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}

function x86_movhps_movhpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-MOVHPS/MOVHPD-OP/EN-MR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
    let high_qword = unsigned(get_slice_int(127 - 64, xmm, 64)) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let (flg0, addr, increment_rip_by, x86) = x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let inst_ac? = true : bool;
                let (flg1, x86) = x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, high_qword, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}
