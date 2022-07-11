$include "./shifts_spec.sail"
$include "./rotates_spec.sail"

val x86_sal_sar_shl_shr_rcl_rcr_rol_ror : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_sal_sar_shl_shr_rcl_rcr_rol_ror (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let byte_operand? : bool = opcode == 0xc0 | opcode == 0xd0 | opcode == 0xd2;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(if opcode == 0xc0 | opcode == 0xc1 then 1 else 0, 3));
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let (flg1, shift_rotate_by) : (option(string), bits(128)) = {
        match opcode {
          208 => (None() : option(string), 0x00000000000000000000000000000001),
          209 => (None() : option(string), 0x00000000000000000000000000000001),
          210 => (None() : option(string), sail_zero_extend(rr08(0x1, rex_byte), 128)),
          211 => (None() : option(string), sail_zero_extend(rr08(0x1, rex_byte), 128)),
          192 => rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false }),
          193 => rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false }),
          _ => (None() : option(string), 0x00000000000000000000000000000000)
        }
    };
    if is_some(flg1) then throw(Emsg("Model state error: :RME-SIZE-ERROR"));
    let countmask : {|31, 63|} = {
        if logbitp(3, rex_byte) then 63 else 31
    };
    let shift_rotate_by : bits(128) = logand(bits_of_int(countmask, 128), shift_rotate_by);
    let (flg, temp_rip) : (option(string), sbits(49)) = {
        if opcode == 0xc0 | opcode == 0xc1 then {
            add_to_iptr(proc_mode, temp_rip, 0x000000000001)
        } else {
            (None() : option(string), sail_sign_extend(temp_rip, 49))
        }
    };
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = {
        match reg {
          0 => rol_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          1 => ror_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          2 => rcl_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          3 => rcr_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          4 => sal_shl_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          5 => shr_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          7 => sar_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          _ => (0x0000000000000000, 0x00000000, 0x00000000)
        }
    };
    write_user_rflags(output_rflags, undefined_flags);
    let flg2 : option(string) = x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, unsigned(sail_mask(64, trunc(reg_mem_size, result))), seg_reg, addr, rex_byte, r_m, mod_var);
    if is_some(flg2) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

val x86_shld_shrd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shld_shrd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHLD/SHRD";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false));
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg, dst_value, increment_rip_by, dst_addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    if is_some(flg) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let src_value : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let (flg, count) : (option(string), bits(128)) = {
        match opcode {
          164 => rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false }),
          172 => rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false }),
          165 => (None() : option(string), sail_zero_extend(rr08(0x1, rex_byte), 128)),
          173 => (None() : option(string), sail_zero_extend(rr08(0x1, rex_byte), 128)),
          _ => (None() : option(string), 0x00000000000000000000000000000000)
        }
    };
    if is_some(flg) then throw(Emsg("Model state error: :RME-SIZE-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = {
        match opcode {
          164 => add_to_iptr(proc_mode, temp_rip, 0x000000000001),
          172 => add_to_iptr(proc_mode, temp_rip, 0x000000000001),
          165 => (None() : option(string), sail_sign_extend(temp_rip, 49)),
          173 => (None() : option(string), sail_sign_extend(temp_rip, 49)),
          _ => (None() : option(string), 0b0000000000000000000000000000000000000000000000000)
        }
    };
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let count_mask : {|31, 63|} = {
        if logbitp(3, rex_byte) then 63 else 31
    };
    let count : bits(128) = logand(bits_of_int(count_mask, 128), count);
    let input_rflags : bits(32) = rflags;
    let (result, result_undefined?, output_rflags, undefined_flags) : (bits(64), bool, bits(32), bits(32)) = {
        match opcode {
          164 => shld_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          165 => shld_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          172 => shrd_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          173 => shrd_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          _ => (0x0000000000000000, false, 0x00000000, 0x00000000)
        }
    };
    let result : int = {
        if result_undefined? then {
            undef_read()
        } else {
            unsigned(result)
        }
    };
    write_user_rflags(output_rflags, undefined_flags);
    let flg : option(string) = x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(sail_mask(64, trunc(operand_size, bits_of_int(result, 64)))), seg_reg, dst_addr, rex_byte, r_m, mod_var);
    if is_some(flg) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_iptr(proc_mode, temp_rip)
}
