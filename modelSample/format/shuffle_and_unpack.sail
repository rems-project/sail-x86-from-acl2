
val extract_32_bits : (bits(128), bits(2)) -> bits(32)

function extract_32_bits (x, n) = {
    (match n {
      0 => slice(x, 0, 32),
      1 => slice(x, 32, 32),
      2 => slice(x, 64, 32),
      _ => slice(x, 96, 32)
    }) : bits(32)
}

val extract_64_bits : (bits(128), range(0, 1)) -> bits(64)

function extract_64_bits (x, n) = {
    (match n {
      0 => slice(x, 0, 64),
      _ => slice(x, 64, 64)
    }) : bits(64)
}

val x86_shufps_op_en_rmi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-SHUFPS-OP/EN-RMI" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let xmm_index = reg_index(reg, rex_byte, get_slice_int(2, 2, 0)) : bits(4);
    let xmm = xmmi_size(get_slice_int(5, 16, 0), xmm_index) : bits(128);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 1, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let xmm_mem = the_bits(128, xmm_mem) : bits(128);
    let increment_rip_by = the_range(0, 4, increment_rip_by) : {|0, 1, 2, 4|};
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm) = rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), int);
            let imm = the_bits(8, imm) : bits(8);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52));
                let temp_rip = the_sbits(48, temp_rip) : sbits(48);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let imm0 = slice(imm, 0, 2) : bits(2);
                        let imm1 = slice(imm, 2, 2) : bits(2);
                        let imm2 = slice(imm, 4, 2) : bits(2);
                        let imm3 = slice(imm, 6, 2) : bits(2);
                        let dword0 = extract_32_bits(xmm, imm0) : bits(32);
                        let dword1 = extract_32_bits(xmm, imm1) : bits(32);
                        let dword2 = extract_32_bits(xmm_mem, imm2) : bits(32);
                        let dword3 = extract_32_bits(xmm_mem, imm3) : bits(32);
                        let result = merge_4_u32s(dword3, dword2, dword1, dword0) : bits(128);
                        write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(result));
                        write_iptr(proc_mode, temp_rip)
                    }
                }
            }
        }
    }
}

val x86_shufpd_op_en_rmi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-SHUFPD-OP/EN-RMI" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let xmm_index = reg_index(reg, rex_byte, get_slice_int(2, 2, 0)) : bits(4);
    let xmm = xmmi_size(get_slice_int(5, 16, 0), xmm_index) : bits(128);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 1, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let xmm_mem = the_bits(128, xmm_mem) : bits(128);
    let increment_rip_by = the_range(0, 4, increment_rip_by) : {|0, 1, 2, 4|};
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm) = rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), int);
            let imm = the_bits(8, imm) : bits(8);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52));
                let temp_rip = the_sbits(48, temp_rip) : sbits(48);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let imm0 = logbit(0, imm) : bits(1);
                        let imm1 = logbit(1, imm) : bits(1);
                        let qword0 = extract_64_bits(xmm, unsigned(imm0)) : bits(64);
                        let qword1 = extract_64_bits(xmm_mem, unsigned(imm1)) : bits(64);
                        let result = merge_2_u64s(qword1, qword0) : bits(128);
                        write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(result));
                        write_iptr(proc_mode, temp_rip)
                    }
                }
            }
        }
    }
}

val x86_unpck?ps_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-UNPCK?PS-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let xmm_index = reg_index(reg, rex_byte, get_slice_int(2, 2, 0)) : bits(4);
    let xmm = xmmi_size(get_slice_int(5, 16, 0), xmm_index) : bits(128);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let xmm_mem = the_bits(128, xmm_mem) : bits(128);
    let increment_rip_by = the_range(0, 4, increment_rip_by) : {|0, 1, 2, 4|};
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let dword0 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm, 64, 32)
                } else {
                    slice(xmm, 0, 32)
                }) : bits(32);
                let dword1 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm_mem, 64, 32)
                } else {
                    slice(xmm_mem, 0, 32)
                }) : bits(32);
                let dword2 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm, 96, 32)
                } else {
                    slice(xmm, 32, 32)
                }) : bits(32);
                let dword3 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm_mem, 96, 32)
                } else {
                    slice(xmm_mem, 32, 32)
                }) : bits(32);
                let result = merge_4_u32s(dword3, dword2, dword1, dword0) : bits(128);
                write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(result));
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}

val x86_unpck?pd_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-UNPCK?PD-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let xmm_index = reg_index(reg, rex_byte, get_slice_int(2, 2, 0)) : bits(4);
    let xmm = xmmi_size(get_slice_int(5, 16, 0), xmm_index) : bits(128);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, xmm_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let xmm_mem = the_bits(128, xmm_mem) : bits(128);
    let increment_rip_by = the_range(0, 4, increment_rip_by) : {|0, 1, 2, 4|};
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let qword0 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm, 64, 64)
                } else {
                    slice(xmm, 0, 64)
                }) : bits(64);
                let qword1 = (if get_slice_int(1, high_low, 0) == get_slice_int(1, 1, 0) then {
                    slice(xmm_mem, 64, 64)
                } else {
                    slice(xmm_mem, 0, 64)
                }) : bits(64);
                let result = merge_2_u64s(qword1, qword0) : bits(128);
                write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(result));
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}
