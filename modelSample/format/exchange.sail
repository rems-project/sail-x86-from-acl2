
val x86_xchg : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_xchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-XCHG" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == get_slice_int(8, 134, 0)) : bool;
    let reg_mem_size = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false) : {|8, 1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, val1, increment_rip_by, addr) = (if get_slice_int(8, ash(unsigned(opcode), -4), 0) == get_slice_int(8, 9, 0) then {
        (None() : option(string), unsigned(rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), rex_byte)), 0 : {|0|}, get_slice_int(67, 0, 0))
    } else {
        let (elem0, elem1, elem2, elem3) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
        (elem0, elem1, elem2, the_sbits(67, elem3))
    }) : (option(string), int, {|0, 1, 2, 4|}, sbits(67));
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
                let rw_rd = the_bits(3, logand(get_slice_int(8, 7, 0), opcode)) : bits(3);
                let val2 = (if get_slice_int(8, ash(unsigned(opcode), -4), 0) == get_slice_int(8, 9, 0) then {
                    rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(rw_rd, rex_byte, get_slice_int(2, 0, 0)), rex_byte)
                } else {
                    rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)
                }) : bits(64);
                let flg2 = (if get_slice_int(8, ash(unsigned(opcode), -4), 0) == get_slice_int(8, 9, 0) then {
                    let x86 = write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), unsigned(val2), rex_byte) : unit;
                    None() : option(string)
                } else {
                    x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, unsigned(val2), seg_reg, addr, rex_byte, r_m, mod_var)
                }) : option(string);
                if is_some(flg2) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM-ERROR"))
                } else {
                    if the_bits(8, the_bits(4, ash(unsigned(opcode), -4))) == get_slice_int(8, 9, 0) then {
                        write_rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(rw_rd, rex_byte, get_slice_int(2, 0, 0)), val1, rex_byte)
                    } else {
                        write_rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), val1, rex_byte)
                    };
                    write_iptr(proc_mode, temp_rip)
                }
            }
        }
    }
}

val x86_cmpxchg : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cmpxchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-CMPXCHG" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == get_slice_int(8, 176, 0)) : bool;
    let reg_mem_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let rax_var = rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
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
                let input_rflags = rflags : bits(32);
                let (result, output_rflags, undefined_flags) = (match reg_mem_size {
                  1 => let (elem0, elem1, elem2) =
                    gpr_arith_logic_spec_1(8, get_slice_int(8, reg_mem, 0), the_bits(8, rax_var), input_rflags) : (bits(8), bits(32), bits(32))
                  in
                    (the_bits(64, elem0), elem1, elem2),
                  2 => let (elem0, elem1, elem2) =
                    gpr_arith_logic_spec_2(8, get_slice_int(16, reg_mem, 0), the_bits(16, rax_var), input_rflags) : (bits(16), bits(32), bits(32))
                  in
                    (the_bits(64, elem0), elem1, elem2),
                  4 => let (elem0, elem1, elem2) =
                    gpr_arith_logic_spec_4(8, get_slice_int(32, reg_mem, 0), the_bits(32, rax_var), input_rflags) : (bits(32), bits(32), bits(32))
                  in
                    (the_bits(64, elem0), elem1, elem2),
                  _ => gpr_arith_logic_spec_8(8, get_slice_int(64, reg_mem, 0), rax_var, input_rflags)
                }) : (bits(64), bits(32), bits(32)) : (bits(64), bits(32), bits(32));
                let output_rflags = output_rflags : bits(32);
                let undefined_flags = undefined_flags : bits(32);
                write_user_rflags(output_rflags, undefined_flags);
                let flg1 = (if the_bits(8, result) == get_slice_int(8, 0, 0) then {
                    let register_var = rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte) : bits(64);
                    x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, unsigned(register_var), seg_reg, addr, rex_byte, r_m, mod_var)
                } else {
                    let x86 = write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), reg_mem, rex_byte) : unit;
                    None() : option(string)
                }) : option(string);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM-ERROR"))
                } else {
                    write_iptr(proc_mode, temp_rip)
                }
            }
        }
    }
}

val x86_two_byte_nop : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_two_byte_nop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-TWO-BYTE-NOP" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
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
        throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :NEXT-RIP-INVALID"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}
