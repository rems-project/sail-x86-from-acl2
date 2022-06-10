
val jcc_cmovcc_setcc_spec : bits(8) -> bool

function jcc_cmovcc_setcc_spec opcode = {
    let low_nibble = the_bits(4, logand(opcode, get_slice_int(8, 15, 0))) : bits(4);
    (match low_nibble {
      0 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_of(rflags_var)))),
      1 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_of(rflags_var)))),
      2 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_cf(rflags_var)))),
      3 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_cf(rflags_var)))),
      4 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_zf(rflags_var)))),
      5 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_zf(rflags_var)))),
      6 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_cf(rflags_var)))) | get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var =
        rflags : bits(32)
      in
        the_bits(2, rflagsbits_get_zf(rflags_var)))),
      7 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_cf(rflags_var)))) & get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var =
        rflags : bits(32)
      in
        the_bits(2, rflagsbits_get_zf(rflags_var)))),
      8 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_sf(rflags_var)))),
      9 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_sf(rflags_var)))),
      10 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_pf(rflags_var)))),
      11 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_pf(rflags_var)))),
      12 => not_bool(the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_sf(rflags_var)))) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_of(rflags_var))))),
      13 => the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in the_bits(2, rflagsbits_get_sf(rflags_var)))) == the_bits(8, the_bits(1, let rflags_var =
        rflags : bits(32)
      in
        the_bits(2, rflagsbits_get_of(rflags_var)))),
      14 => get_slice_int(8, 1, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_zf(rflags_var)))) | not_bool(the_bits(8, the_bits(1, let rflags_var =
        rflags : bits(32)
      in
        the_bits(2, rflagsbits_get_sf(rflags_var)))) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_of(rflags_var))))),
      15 => get_slice_int(8, 0, 0) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_zf(rflags_var)))) & the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_sf(rflags_var)))) == the_bits(8, the_bits(1, let rflags_var = rflags : bits(32) in
        the_bits(2, rflagsbits_get_of(rflags_var)))),
      _ => false
    }) : bool
}

val x86_one_byte_jcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_one_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-ONE-BYTE-JCC" : string;
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 1, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let branch_cond = jcc_cmovcc_setcc_spec(opcode) : bool;
        if branch_cond then {
            let (flg, rel8) = rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
            } else {
                let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1 + signed(rel8), 0)) : (option(string), sbits(52));
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    write_iptr(proc_mode, the_sbits(48, next_rip))
                }
            }
        } else {
            let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
            } else {
                write_iptr(proc_mode, the_sbits(48, next_rip))
            }
        }
    }
}

val x86_two_byte_jcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_two_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-TWO-BYTE-JCC" : string;
    let offset_size = the_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true)) : {|1, 2, 4|};
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, offset_size, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let branch_cond = jcc_cmovcc_setcc_spec(opcode) : bool;
        if branch_cond then {
            let (flg, offset) = rime_size(proc_mode, offset_size, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
            } else {
                let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, offset_size + signed(offset), 0)) : (option(string), sbits(52));
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    write_iptr(proc_mode, the_sbits(48, next_rip))
                }
            }
        } else {
            let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, offset_size, 0)) : (option(string), sbits(52));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
            } else {
                write_iptr(proc_mode, the_sbits(48, next_rip))
            }
        }
    }
}

val x86_jrcxz : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_jrcxz (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-JRCXZ" : string;
    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 1, 0)) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
        let register_size = select_address_size(proc_mode, p4?) : {|8, 2, 4|};
        let branch_cond = (the_bits(8, rgfi_size(get_slice_int(4, register_size, 0), get_slice_int(4, 1, 0), rex_byte)) == get_slice_int(8, 0, 0)) : bool;
        if branch_cond then {
            let (flg, rel8) = rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), sbits(68));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
            } else {
                let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1 + signed(rel8), 0)) : (option(string), sbits(52));
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    write_iptr(proc_mode, the_sbits(48, next_rip))
                }
            }
        } else {
            let (flg, next_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52));
            if is_some(flg) then {
                throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
            } else {
                write_iptr(proc_mode, the_sbits(48, next_rip))
            }
        }
    }
}

val x86_cmovcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cmovcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-CMOVCC" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let operand_size = the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let branch_cond = jcc_cmovcc_setcc_spec(opcode) : bool;
                if branch_cond then {
                    write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), reg_mem, rex_byte)
                };
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}

val x86_setcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_setcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-SETCC" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let (flg0, addr, increment_rip_by) = (if the_bits(8, mod_var) == get_slice_int(8, 3, 0) then {
        (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
    } else {
        let (elem0, elem1, elem2) = x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), sbits(67), {|0, 1, 2, 4|});
        (elem0, the_sbits(68, elem1), elem2)
    }) : (option(string), sbits(68), {|0, 1, 2, 4|});
    let addr = the_sbits(64, addr) : sbits(64);
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let branch_cond = jcc_cmovcc_setcc_spec(opcode) : bool;
                let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
                let inst_ac? = true : bool;
                let sailval = (if branch_cond then 1 else 0) : {|0, 1|};
                let flg2 = x86_operand_to_reg_mem(proc_mode, 1, inst_ac?, false, sailval, seg_reg, addr, rex_byte, r_m, mod_var) : option(string);
                if is_some(flg2) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                } else {
                    write_iptr(proc_mode, temp_rip)
                }
            }
        }
    }
}
