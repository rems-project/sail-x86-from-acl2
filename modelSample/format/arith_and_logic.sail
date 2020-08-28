$include "./arith_and_logic_spec.sail"
$include "./decoding_and_spec_utils.sail"

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g : (int, int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let byte_operand? = (0 == binary_logand(1, opcode)) : bool;
    let operand_size = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0) : int;
    let g = rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, e, increment_rip_by, e_addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
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
                let input_rflags = r_rflags(0) : int;
                let (result, output_rflags, undefined_flags) = (match operand_size {
                  1 => gpr_arith_logic_spec_1(operation, e, g, input_rflags),
                  2 => gpr_arith_logic_spec_2(operation, e, g, input_rflags),
                  4 => gpr_arith_logic_spec_4(operation, e, g, input_rflags),
                  _ => gpr_arith_logic_spec_8(operation, e, g, input_rflags)
                }) : (int, int, int) : (int, int, int);
                let (flg1, x86) = (if operation == 8 | operation == 7 then {
                    (None() : option(string), 0 : int)
                } else {
                    x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, result, seg_reg, e_addr, rex_byte, r_m, mod_var, 0)
                }) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                } else {
                    let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_g_e : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, undef, wreg}

function x86_add_adc_sub_sbb_or_and_xor_cmp_g_e (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let byte_operand? = (0 == binary_logand(1, opcode)) : bool;
    let operand_size = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0) : int;
    let g = rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, e, increment_rip_by, e_addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
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
                let input_rflags = r_rflags(0) : int;
                let (result, output_rflags, undefined_flags) = (match operand_size {
                  1 => gpr_arith_logic_spec_1(operation, g, e, input_rflags),
                  2 => gpr_arith_logic_spec_2(operation, g, e, input_rflags),
                  4 => gpr_arith_logic_spec_4(operation, g, e, input_rflags),
                  _ => gpr_arith_logic_spec_8(operation, g, e, input_rflags)
                }) : (int, int, int) : (int, int, int);
                let x86 = (if operation == 8 then 0 else {
                    write_rgfi_size(operand_size, reg_index(reg, rex_byte, 2), result, rex_byte, 0)
                }) : int;
                let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i : (int, int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let e_byte_operand? = (opcode == 128 | opcode == 130 | opcode == 246) : bool;
    let e_size = select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false, 0) : int;
    let imm_byte_operand? = (opcode == 128 | opcode == 130 | opcode == 131 | opcode == 246) : bool;
    let imm_size = select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, e, increment_rip_by, e_addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, e_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, imm_size, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm, x86) = rme_size(proc_mode, imm_size, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let imm = loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), imm)) : int;
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, imm_size, 0) : (option(string), int);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let input_rflags = r_rflags(0) : int;
                        let (result, output_rflags, undefined_flags) = (match e_size {
                          1 => gpr_arith_logic_spec_1(operation, e, imm, input_rflags),
                          2 => gpr_arith_logic_spec_2(operation, e, imm, input_rflags),
                          4 => gpr_arith_logic_spec_4(operation, e, imm, input_rflags),
                          _ => gpr_arith_logic_spec_8(operation, e, imm, input_rflags)
                        }) : (int, int, int) : (int, int, int);
                        let (flg1, x86) = (if operation == 8 | operation == 7 then {
                            (None() : option(string), 0 : int)
                        } else {
                            x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, result, seg_reg, e_addr, rex_byte, r_m, mod_var, 0)
                        }) : (option(string), int);
                        if is_some(flg1) then {
                            throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                        } else {
                            let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                            let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                            0
                        }
                    }
                }
            }
        }
    }
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, undef, wreg}

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I" : string;
    let byte_operand? = (0 == binary_logand(1, opcode)) : bool;
    let operand_size = select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false, 0) : int;
    let rax_size = (if logbitp(3, rex_byte) then 8 else {
        operand_size
    }) : int;
    let rax_var = rgfi_size(rax_size, 0, rex_byte, 0) : int;
    let (flg, imm, x86) = rme_size(proc_mode, operand_size, temp_rip, 1, ":X", false, 0, struct { mem_ptr? = false }) : (option(string), int, int);
    if is_some(flg) then {
        throw(Emsg("Model state error: :RME-SIZE-ERROR"))
    } else {
        let imm = (if not_bool(byte_operand?) & rax_size == 8 then {
            n64(n32_to_i32(imm))
        } else {
            imm
        }) : int;
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, operand_size, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let input_rflags = r_rflags(0) : int;
                let (result, output_rflags, undefined_flags) = (match rax_size {
                  1 => gpr_arith_logic_spec_1(operation, rax_var, imm, input_rflags),
                  2 => gpr_arith_logic_spec_2(operation, rax_var, imm, input_rflags),
                  4 => gpr_arith_logic_spec_4(operation, rax_var, imm, input_rflags),
                  _ => gpr_arith_logic_spec_8(operation, rax_var, imm, input_rflags)
                }) : (int, int, int) : (int, int, int);
                let x86 = (if operation == 8 | operation == 7 then 0 else {
                    write_rgfi_size(rax_size, 0, result, rex_byte, 0)
                }) : int;
                let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                0
            }
        }
    }
}

val x86_inc_dec_fe_ff : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-INC/DEC-FE-FF" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (0 == binary_logand(1, opcode)) : bool;
    let r_mem_size = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, r_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
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
                let input_rflags = r_rflags(0) : int;
                let old_cf = rflagsbits_get_cf(input_rflags) : int;
                let (result, output_rflags, undefined_flags) = (match r_mem_size {
                  1 => gpr_arith_logic_spec_1(if reg == 0 then 0 else 4, r_mem, 1, input_rflags),
                  2 => gpr_arith_logic_spec_2(if reg == 0 then 0 else 4, r_mem, 1, input_rflags),
                  4 => gpr_arith_logic_spec_4(if reg == 0 then 0 else 4, r_mem, 1, input_rflags),
                  _ => gpr_arith_logic_spec_8(if reg == 0 then 0 else 4, r_mem, 1, input_rflags)
                }) : (int, int, int) : (int, int, int);
                let output_rflags = set_rflagsbits_get_cf(old_cf, output_rflags) : int;
                let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                let (flg1, x86) = x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, result, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_inc_dec_4x : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rreg, undef, wreg}

function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-INC/DEC-4X" : string;
    let operand_size = select_operand_size(proc_mode, false, 0, false, prefixes, false, false, false, 0) : int;
    let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
    if is_some(badlength?) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
    } else {
        let reg = binary_logand(7, opcode) : int;
        let operand = rgfi_size(operand_size, reg, 0, 0) : int;
        let input_rflags = r_rflags(0) : int;
        let old_cf = rflagsbits_get_cf(input_rflags) : int;
        let (result, output_rflags, undefined_flags) = (match operand_size {
          1 => gpr_arith_logic_spec_1(if logbitp(3, opcode) then 4 else 0, operand, 1, input_rflags),
          2 => gpr_arith_logic_spec_2(if logbitp(3, opcode) then 4 else 0, operand, 1, input_rflags),
          4 => gpr_arith_logic_spec_4(if logbitp(3, opcode) then 4 else 0, operand, 1, input_rflags),
          _ => gpr_arith_logic_spec_8(if logbitp(3, opcode) then 4 else 0, operand, 1, input_rflags)
        }) : (int, int, int) : (int, int, int);
        let output_rflags = set_rflagsbits_get_cf(old_cf, output_rflags) : int;
        let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
        let x86 = write_rgfi_size(operand_size, reg, result, 0, 0) : int;
        let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
        0
    }
}

val x86_not_neg_f6_f7 : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}

function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-NOT/NEG-F6-F7" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (0 == binary_logand(1, opcode)) : bool;
    let r_mem_size = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, r_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
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
                let input_rflags = r_rflags(0) : int;
                let (result, output_rflags, undefined_flags) = (match reg {
                  3 => (match r_mem_size {
                    1 => gpr_arith_logic_spec_1(4, 0, r_mem, input_rflags),
                    2 => gpr_arith_logic_spec_2(4, 0, r_mem, input_rflags),
                    4 => gpr_arith_logic_spec_4(4, 0, r_mem, input_rflags),
                    _ => gpr_arith_logic_spec_8(4, 0, r_mem, input_rflags)
                  }) : (int, int, int),
                  _ => (trunc(r_mem_size, lognot(r_mem)), 0 : int, 0 : int)
                }) : (int, int, int) : (int, int, int);
                let x86 = (if reg == 3 then {
                    let cf = (if 0 == r_mem then 0 else 1) : int;
                    let output_rflags = set_rflagsbits_get_cf(cf, output_rflags) : int;
                    let x86 = write_user_rflags(output_rflags, undefined_flags, 0) : int;
                    0
                } else 0) : int;
                let (flg1, x86) = x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, result, seg_reg, addr, rex_byte, r_m, mod_var, 0) : (option(string), int);
                if is_some(flg1) then {
                    throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
                } else {
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}
