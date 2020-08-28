$include "./shifts_spec.sail"
$include "./rotates_spec.sail"

val x86_sal_sar_shl_shr_rcl_rcr_rol_ror : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_sal_sar_shl_shr_rcl_rcr_rol_ror (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let byte_operand? = (opcode == 192 | opcode == 208 | opcode == 210) : bool;
    let reg_mem_size = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, if opcode == 192 | opcode == 193 then 1 else 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, shift_rotate_by, x86) = (match opcode {
              208 => (None() : option(string), 1 : int, 0 : int),
              209 => (None() : option(string), 1 : int, 0 : int),
              210 => (None() : option(string), rr08(1, rex_byte, 0), 0 : int),
              211 => (None() : option(string), rr08(1, rex_byte, 0), 0 : int),
              192 => rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }),
              193 => rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }),
              _ => (None() : option(string), 0 : int, 0 : int)
            }) : (option(string), int, int) : (option(string), int, int);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let countmask = (if logbitp(3, rex_byte) then 63 else 31) : int;
                let shift_rotate_by = binary_logand(countmask, shift_rotate_by) : int;
                let (flg, temp_rip) = (if opcode == 192 | opcode == 193 then {
                    add_to_iptr(proc_mode, temp_rip, 1, 0)
                } else {
                    (None() : option(string), temp_rip)
                }) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let input_rflags = r_rflags(0) : int;
                        let (result, output_rflags, undefined_flags) = (match reg {
                          0 => rol_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          1 => ror_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          2 => rcl_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          3 => rcr_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          4 => sal_shl_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          5 => shr_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          7 => sar_spec(reg_mem_size, reg_mem, shift_rotate_by, input_rflags),
                          _ => (0 : int, 0 : int, 0 : int)
                        }) : (int, int, int) : (int, int, int);
                        let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                        let (flg2, x86) = x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, trunc(reg_mem_size, result), seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                        if is_some(flg2) then {
                            throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                        } else {
                            let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                            0
                        }
                    }
                }
            }
        }
    }
}

val x86_shld_shrd : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_shld_shrd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-SHLD/SHRD" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let operand_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg, dst_value, increment_rip_by, dst_addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0) : (option(string), int, int, int, int);
    if is_some(flg) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let src_value = rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0) : int;
            let (flg, count, x86) = (match opcode {
              164 => rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }),
              172 => rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }),
              165 => (None() : option(string), rr08(1, rex_byte, 0), 0 : int),
              173 => (None() : option(string), rr08(1, rex_byte, 0), 0 : int),
              _ => (None() : option(string), 0 : int, 0 : int)
            }) : (option(string), int, int) : (option(string), int, int);
            if is_some(flg) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = (match opcode {
                  164 => add_to_iptr(proc_mode, temp_rip, 1, 0),
                  172 => add_to_iptr(proc_mode, temp_rip, 1, 0),
                  165 => (None() : option(string), temp_rip),
                  173 => (None() : option(string), temp_rip),
                  _ => (None() : option(string), 0 : int)
                }) : (option(string), int) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let count_mask = (if logbitp(3, rex_byte) then 63 else 31) : int;
                        let count = binary_logand(count_mask, count) : int;
                        let input_rflags = r_rflags(0) : int;
                        let (result, result_undefined?, output_rflags, undefined_flags) = (match opcode {
                          164 => shld_spec(operand_size, dst_value, src_value, count, input_rflags),
                          165 => shld_spec(operand_size, dst_value, src_value, count, input_rflags),
                          172 => shrd_spec(operand_size, dst_value, src_value, count, input_rflags),
                          173 => shrd_spec(operand_size, dst_value, src_value, count, input_rflags),
                          _ => (0 : int, false, 0 : int, 0 : int)
                        }) : (int, bool, int, int) : (int, bool, int, int);
                        let (result, x86) = (if result_undefined? then {
                            undef_read(0)
                        } else {
                            (result, 0 : int)
                        }) : (int, int);
                        let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                        let (flg, x86) = x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, trunc(operand_size, result), seg_reg, dst_addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                        if is_some(flg) then {
                            throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                        } else {
                            let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                            0
                        }
                    }
                }
            }
        }
    }
}
