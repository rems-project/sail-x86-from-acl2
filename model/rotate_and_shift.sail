$include "./shifts_spec.sail"
$include "./rotates_spec.sail"

/*! <p> 
 Op/En: MI<br/> 
 C0/0: ROL r/m8, imm8<br/> 
 C0/1: ROR r/m8, imm8<br/> 
 C0/2: RCL r/m8, imm8<br/> 
 C0/3: RCR r/m8, imm8<br/> 
 C0/4: SAL/SHL r/m8, imm8<br/> 
 C0/5: SHR r/m8, imm8<br/> 
 C0/7: SAR r/m8, imm8<br/> 
 C1/0: ROL r/m16/32/64, imm8<br/> 
 C1/1: ROR r/m16/32/64, imm8<br/> 
 C1/2: RCL r/m16/32/64, imm8<br/> 
 C1/3: RCR r/m16/32/64, imm8<br/> 
 C1/4: SAL/SHL r/m16/32/64, imm8<br/> 
 C1/5: SHR r/m16/32/64, imm8<br/> 
 C1/7: SAR r/m16/32/64. imm8<br/> 
 </p> 
 
 <p> 
 Op/En: M1<br/> 
 D0/0: ROL r/m8, 1<br/> 
 D0/1: ROR r/m8, 1<br/> 
 D0/2: RCL r/m8, 1<br/> 
 D0/3: RCR r/m8, 1<br/> 
 D0/4: SAL/SHL r/m8, 1<br/> 
 D0/5: SHR r/m8, 1<br/> 
 D0/7: SAR r/m8, 1<br/> 
 D1/0: ROL r/m16/32/64, 1<br/> 
 D1/1: ROR r/m16/32/64, 1<br/> 
 D1/2: RCL r/m16/32/64, 1<br/> 
 D1/3: RCR r/m16/32/64, 1<br/> 
 D1/4: SAL/SHL r/m16/32/64, 1<br/> 
 D1/5: SHR r/m16/32/64, 1<br/> 
 D1/7: SAR r/m16/32/64, 1<br/> 
 </p> 
 
 <p> 
 Op/En: MC<br/> 
 D2/0: ROL r/m8, CL<br/> 
 D2/1: ROR r/m8, CL<br/> 
 D2/2: RCL r/m8, CL<br/> 
 D2/3: RCR r/m8, CL<br/> 
 D2/4: SAL/SHL r/m8, CL<br/> 
 D2/5: SHR r/m8, CL<br/> 
 D2/7: SAR r/m8, CL<br/> 
 D3/0: ROL r/m16/32/64, CL<br/> 
 D3/1: ROR r/m16/32/64, CL<br/> 
 D3/2: RCL r/m16/32/64, CL<br/> 
 D3/3: RCR r/m16/32/64, CL<br/> 
 D3/4: SAL/SHL r/m16/32/64, CL<br/> 
 D3/5: SHR r/m16/32/64, CL<br/> 
 D3/7: SAR r/m16/32/64, CL<br/> 
 </p> */
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

/*! Double-precision shift left or right.
<p> 
 Op/En: MRI<br/> 
 0F A4: SHLD r/m16, r16, imm8<br/> 
 0F A4: SHLD r/m32, r32, imm8<br/> 
 0F A4: SHLD r/m64, r64, imm8<br/> 
 </p> 
 
 <p> 
 Op/En: MRC<br/> 
 0F A5: SHLD r/m16, r16, CL<br/> 
 0F A5: SHLD r/m32, r32, CL<br/> 
 0F A5: SHLD r/m64, r64, CL<br/> 
 </p> 
 
 <p> 
 Op/En: MRI<br/> 
 0F AC: SHRD r/m16, r16, imm8<br/> 
 0F AC: SHRD r/m32, r32, imm8<br/> 
 0F AC: SHRD r/m64, r64, imm8<br/> 
 </p> 
 
 <p> 
 Op/En: MRC<br/> 
 0F AD: SHRD r/m16, r16, CL<br/> 
 0F AD: SHRD r/m32, r32, CL<br/> 
 0F AD: SHRD r/m64, r64, CL<br/> 
 </p> */
val x86_shld_shrd : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_shld_shrd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHLD/SHRD";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false));
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (dst_value, increment_rip_by, dst_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let src_value : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let count : bits(128) = {
        match opcode {
          164 => rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false),
          172 => rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false),
          165 => sail_zero_extend(rr08(0x1, rex_byte), 128),
          173 => sail_zero_extend(rr08(0x1, rex_byte), 128),
          _ => 0x00000000000000000000000000000000
        }
    };
    let temp_rip : sbits(48) = {
        match opcode {
          164 => add_to_iptr(proc_mode, temp_rip, 0x000000000001),
          172 => add_to_iptr(proc_mode, temp_rip, 0x000000000001),
          165 => temp_rip,
          173 => temp_rip,
          _ => 0x000000000000
        }
    };
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let count_mask : {|31, 63|} = {
        if logbitp(3, rex_byte) then 63 else 31
    };
    let count : bits(128) = logand(bits_of_int(count_mask, 128), count);
    let input_rflags : rflagsbits = rflags;
    let (result, result_undefined?, output_rflags, undefined_flags) : (bits(64), bool, rflagsbits, rflagsbits) = {
        match opcode {
          164 => shld_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          165 => shld_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          172 => shrd_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          173 => shrd_spec(operand_size, truncate(dst_value, 64), src_value, truncate(count, 6), input_rflags),
          _ => (0x0000000000000000, false, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
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
    x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(sail_mask(64, trunc(operand_size, bits_of_int(result, 64))), 128), seg_reg, dst_addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}
