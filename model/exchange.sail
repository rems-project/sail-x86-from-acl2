
val x86_xchg : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_xchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-XCHG";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = opcode == 0x86;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (val1, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = {
        if ash(unsigned(opcode), -4) == 9 then {
            (sail_zero_extend(rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, rex_byte), 128), 0, 0x0000000000000000)
        } else {
            x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let rw_rd : bits(3) = truncate(logand(0x07, opcode), 3);
    let val2 : bits(64) = {
        if ash(unsigned(opcode), -4) == 9 then {
            rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(rw_rd, rex_byte, 0b00), rex_byte)
        } else {
            rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)
        }
    };
    if ash(unsigned(opcode), -4) == 9 then {
        write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, val2, rex_byte)
    } else {
        x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, sail_zero_extend(val2, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib)
    };
    if bits_of_int(ash(unsigned(opcode), -4), 4) == 0x9 then {
        write_rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(rw_rd, rex_byte, 0b00), truncate(val1, 64), rex_byte)
    } else {
        write_rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), truncate(val1, 64), rex_byte)
    };
    write_iptr(proc_mode, temp_rip)
}

val x86_cmpxchg : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cmpxchg (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CMPXCHG";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = opcode == 0xb0;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let rax_var : bits(64) = rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, rex_byte);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(reg_mem_size, 8, truncate(reg_mem, 64), rax_var, input_rflags);
    write_user_rflags(output_rflags, undefined_flags);
    if result == 0x0000000000000000 then {
        let register_var : bits(64) = rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
        x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, sail_zero_extend(register_var, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib)
    } else {
        write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, truncate(reg_mem, 64), rex_byte)
    };
    write_iptr(proc_mode, temp_rip)
}

val x86_two_byte_nop : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_two_byte_nop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-TWO-BYTE-NOP";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p4? : bool = 0x67 == prefixes[adr];
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_iptr(proc_mode, temp_rip)
}
