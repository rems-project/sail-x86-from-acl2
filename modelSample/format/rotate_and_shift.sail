$include "./shifts_spec.sail"
$include "./rotates_spec.sail"

val x86_sal_sar_shl_shr_rcl_rcr_rol_ror : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_sal_sar_shl_shr_rcl_rcr_rol_ror (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let byte_operand? = (opcode == get_slice_int(8, 192, 0) | opcode == get_slice_int(8, 208, 0) | opcode == get_slice_int(8, 210, 0)) : bool;
    let reg_mem_size = the_range(0, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, if opcode == get_slice_int(8, 192, 0) | opcode == get_slice_int(8, 193, 0) then 1 else 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, shift_rotate_by) = (match opcode {
              208 => (None() : option(string), 1 : {|1|}),
              209 => (None() : option(string), 1 : {|1|}),
              210 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
              211 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
              192 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }),
              193 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }),
              _ => (None() : option(string), 0 : {|0|})
            }) : (option(string), int) : (option(string), int);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let countmask = (if logbitp(3, rex_byte) then 63 else 31) : {|31, 63|};
                let shift_rotate_by = logand(countmask, shift_rotate_by) : int;
                let (flg, temp_rip) = (if opcode == get_slice_int(8, 192, 0) | opcode == get_slice_int(8, 193, 0) then {
                    add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))
                } else {
                    (None() : option(string), the_sbits(52, temp_rip))
                }) : (option(string), sbits(52));
                let temp_rip = the_sbits(49, temp_rip) : sbits(49);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0)) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let input_rflags = rflags : bits(32);
                        let (result, output_rflags, undefined_flags) = (match reg {
                          0 => rol_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          1 => ror_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          2 => rcl_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          3 => rcr_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          4 => sal_shl_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          5 => shr_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          7 => sar_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
                          _ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
                        }) : (bits(64), bits(32), bits(32)) : (bits(64), bits(32), bits(32));
                        let output_rflags = output_rflags : bits(32);
                        let undefined_flags = undefined_flags : bits(32);
                        write_user_rflags(output_rflags, undefined_flags);
                        let flg2 = x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, unsigned(sail_mask(64, trunc(reg_mem_size, result))), seg_reg, the_sbits(64, addr), rex_byte, r_m, mod_var) : option(string);
                        if is_some(flg2) then {
                            throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                        } else {
                            write_iptr(proc_mode, the_sbits(48, temp_rip))
                        }
                    }
                }
            }
        }
    }
}

val x86_shld_shrd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shld_shrd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-SHLD/SHRD" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let operand_size = the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg, dst_value, increment_rip_by, dst_addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 1, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    if is_some(flg) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let src_value = rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte) : bits(64);
            let (flg, count) = (match opcode {
              164 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }),
              172 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }),
              165 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
              173 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
              _ => (None() : option(string), 0 : {|0|})
            }) : (option(string), int) : (option(string), int);
            if is_some(flg) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = (match opcode {
                  164 => let (elem0, elem1) =
                    add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52))
                  in
                    (elem0, the_sbits(53, elem1)),
                  172 => let (elem0, elem1) =
                    add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0)) : (option(string), sbits(52))
                  in
                    (elem0, the_sbits(53, elem1)),
                  165 => (None() : option(string), the_sbits(53, temp_rip)),
                  173 => (None() : option(string), the_sbits(53, temp_rip)),
                  _ => (None() : option(string), get_slice_int(53, 0, 0))
                }) : (option(string), sbits(53)) : (option(string), sbits(53));
                let temp_rip = the_sbits(48, temp_rip) : sbits(48);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let count_mask = (if logbitp(3, rex_byte) then 63 else 31) : {|31, 63|};
                        let count = logand(count_mask, count) : int;
                        let input_rflags = rflags : bits(32);
                        let (result, result_undefined?, output_rflags, undefined_flags) = (match opcode {
                          164 => shld_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
                          165 => shld_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
                          172 => shrd_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
                          173 => shrd_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
                          _ => (get_slice_int(64, 0, 0), false, get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
                        }) : (bits(64), bool, bits(32), bits(32)) : (bits(64), bool, bits(32), bits(32));
                        let output_rflags = output_rflags : bits(32);
                        let undefined_flags = undefined_flags : bits(32);
                        let result = (if result_undefined? then {
                            undef_read()
                        } else {
                            unsigned(result)
                        }) : int;
                        write_user_rflags(output_rflags, undefined_flags);
                        let flg = x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(sail_mask(64, trunc(operand_size, get_slice_int(64, result, 0)))), seg_reg, the_sbits(64, dst_addr), rex_byte, r_m, mod_var) : option(string);
                        if is_some(flg) then {
                            throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                        } else {
                            write_iptr(proc_mode, temp_rip)
                        }
                    }
                }
            }
        }
    }
}
