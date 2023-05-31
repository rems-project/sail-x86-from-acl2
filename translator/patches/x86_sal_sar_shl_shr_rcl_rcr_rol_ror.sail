// Countmask was wrong for byte operations

val x86_sal_sar_shl_shr_rcl_rcr_rol_ror : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_sal_sar_shl_shr_rcl_rcr_rol_ror (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0xc0 | opcode == 0xd0 | opcode == 0xd2;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, if opcode == 0xc0 | opcode == 0xc1 then 0b001 else 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let shift_rotate_by : bits(128) = {
        match opcode {
          208 => 0x00000000000000000000000000000001,
          209 => 0x00000000000000000000000000000001,
          210 => sail_zero_extend(rr08(0x1, rex_byte), 128),
          211 => sail_zero_extend(rr08(0x1, rex_byte), 128),
          192 => rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false),
          193 => rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false),
          _ => 0x00000000000000000000000000000000
        }
    };
    let countmask : {|31, 63|} = {
        if logbitp(3, rex_byte) & not_bool(byte_operand?) then 63 else 31
    };
    let shift_rotate_by : bits(128) = logand(bits_of_int(countmask, 128), shift_rotate_by);
    let temp_rip : sbits(49) = {
        if opcode == 0xc0 | opcode == 0xc1 then {
            sail_sign_extend(add_to_iptr(proc_mode, temp_rip, 0x000000000001), 49)
        } else {
            sail_sign_extend(temp_rip, 49)
        }
    };
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = {
        match reg {
          0 => rol_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          1 => ror_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          2 => rcl_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          3 => rcr_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          4 => sal_shl_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          5 => shr_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          7 => sar_spec(reg_mem_size, truncate(reg_mem, 64), truncate(shift_rotate_by, 6), input_rflags),
          _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
        }
    };
    write_user_rflags(output_rflags, undefined_flags);
    x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, sail_zero_extend(sail_mask(64, trunc(reg_mem_size, result)), 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}
