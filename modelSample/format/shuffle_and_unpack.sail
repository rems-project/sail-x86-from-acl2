
val extract_32_bits : (int, int) -> int

function extract_32_bits (x, n) = {
    (match n {
      0 => unsigned(get_slice_int(31 - 0, x, 0)),
      1 => unsigned(get_slice_int(63 - 32, x, 32)),
      2 => unsigned(get_slice_int(95 - 64, x, 64)),
      _ => unsigned(get_slice_int(127 - 96, x, 96))
    }) : int
}

val extract_64_bits : (int, int) -> int

function extract_64_bits (x, n) = {
    (match n {
      0 => unsigned(get_slice_int(63 - 0, x, 0)),
      _ => unsigned(get_slice_int(127 - 64, x, 64))
    }) : int
}

val x86_shufps_op_en_rmi : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-SHUFPS-OP/EN-RMI" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm, x86) = rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, 1, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let imm0 = unsigned(get_slice_int(1 - 0, imm, 0)) : int;
                        let imm1 = unsigned(get_slice_int(3 - 2, imm, 2)) : int;
                        let imm2 = unsigned(get_slice_int(5 - 4, imm, 4)) : int;
                        let imm3 = unsigned(get_slice_int(7 - 6, imm, 6)) : int;
                        let dword0 = extract_32_bits(xmm, imm0) : int;
                        let dword1 = extract_32_bits(xmm, imm1) : int;
                        let dword2 = extract_32_bits(xmm_mem, imm2) : int;
                        let dword3 = extract_32_bits(xmm_mem, imm3) : int;
                        let result = merge_4_u32s(dword3, dword2, dword1, dword0) : int;
                        let x86 = write_xmmi_size(16, xmm_index, result, 0) : int;
                        let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                        0
                    }
                }
            }
        }
    }
}

val x86_shufpd_op_en_rmi : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-SHUFPD-OP/EN-RMI" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm, x86) = rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, 1, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let imm0 = logbit(0, imm) : int;
                        let imm1 = logbit(1, imm) : int;
                        let qword0 = extract_64_bits(xmm, imm0) : int;
                        let qword1 = extract_64_bits(xmm_mem, imm1) : int;
                        let result = merge_2_u64s(qword1, qword0) : int;
                        let x86 = write_xmmi_size(16, xmm_index, result, 0) : int;
                        let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                        0
                    }
                }
            }
        }
    }
}

val x86_unpck?ps_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-UNPCK?PS-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
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
                let dword0 = (if high_low == 1 then {
                    unsigned(get_slice_int(95 - 64, xmm, 64))
                } else {
                    unsigned(get_slice_int(31 - 0, xmm, 0))
                }) : int;
                let dword1 = (if high_low == 1 then {
                    unsigned(get_slice_int(95 - 64, xmm_mem, 64))
                } else {
                    unsigned(get_slice_int(31 - 0, xmm_mem, 0))
                }) : int;
                let dword2 = (if high_low == 1 then {
                    unsigned(get_slice_int(127 - 96, xmm, 96))
                } else {
                    unsigned(get_slice_int(63 - 32, xmm, 32))
                }) : int;
                let dword3 = (if high_low == 1 then {
                    unsigned(get_slice_int(127 - 96, xmm_mem, 96))
                } else {
                    unsigned(get_slice_int(63 - 32, xmm_mem, 32))
                }) : int;
                let result = merge_4_u32s(dword3, dword2, dword1, dword0) : int;
                let x86 = write_xmmi_size(16, xmm_index, result, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_unpck?pd_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}

function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-UNPCK?PD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let xmm_index = reg_index(reg, rex_byte, 2) : int;
    let xmm = xmmi_size(16, xmm_index, 0) : int;
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
                let qword0 = (if high_low == 1 then {
                    unsigned(get_slice_int(127 - 64, xmm, 64))
                } else {
                    unsigned(get_slice_int(63 - 0, xmm, 0))
                }) : int;
                let qword1 = (if high_low == 1 then {
                    unsigned(get_slice_int(127 - 64, xmm_mem, 64))
                } else {
                    unsigned(get_slice_int(63 - 0, xmm_mem, 0))
                }) : int;
                let result = merge_2_u64s(qword1, qword0) : int;
                let x86 = write_xmmi_size(16, xmm_index, result, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}
