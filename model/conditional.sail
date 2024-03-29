
val jcc_cmovcc_setcc_spec : bits(8) -> bool

function jcc_cmovcc_setcc_spec opcode = {
    let low_nibble : bits(4) = truncate(logand(opcode, 0x0f), 4);
    (match low_nibble {
      0 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[of]),
      1 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[of]),
      2 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[cf]),
      3 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[cf]),
      4 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[zf]),
      5 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[zf]),
      6 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[cf]) | 0b1 == (let rflags_var : rflagsbits =
        rflags
      in
        rflags_var[zf]),
      7 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[cf]) & 0b0 == (let rflags_var : rflagsbits =
        rflags
      in
        rflags_var[zf]),
      8 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[sf]),
      9 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[sf]),
      10 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[pf]),
      11 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[pf]),
      12 => not_bool((let rflags_var : rflagsbits = rflags in rflags_var[sf]) == (let rflags_var : rflagsbits = rflags
      in
        rflags_var[of])),
      13 => (let rflags_var : rflagsbits = rflags in rflags_var[sf]) == (let rflags_var : rflagsbits = rflags in
        rflags_var[of]),
      14 => 0b1 == (let rflags_var : rflagsbits = rflags in rflags_var[zf]) | not_bool((let rflags_var : rflagsbits =
        rflags
      in
        rflags_var[sf]) == (let rflags_var : rflagsbits = rflags in rflags_var[of])),
      15 => 0b0 == (let rflags_var : rflagsbits = rflags in rflags_var[zf]) & (let rflags_var : rflagsbits = rflags in
        rflags_var[sf]) == (let rflags_var : rflagsbits = rflags in rflags_var[of]),
      _ => false
    }) : bool
}

val x86_one_byte_jcc : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_one_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ONE-BYTE-JCC";
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b001);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        let rel8 : sbits(64) = rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(1 + signed(rel8), 48));
        write_iptr(proc_mode, next_rip)
    } else {
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
        write_iptr(proc_mode, next_rip)
    }
}

val x86_two_byte_jcc : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_two_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-TWO-BYTE-JCC";
    let offset_size : {|1, 2, 4|} = check_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, bits_of_int(offset_size, 3));
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        let offset : sbits(64) = rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size + signed(offset), 48));
        write_iptr(proc_mode, next_rip)
    } else {
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
        write_iptr(proc_mode, next_rip)
    }
}

val x86_jrcxz : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_jrcxz (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-JRCXZ";
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b001);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p4? : bool = 0x67 == prefixes[adr];
    let register_size : address_size = select_address_size(proc_mode, Some(prefixes));
    let branch_cond : bool = rgfi_size(bits_of_int(register_size, 4), 0x1, rex_byte) == 0x0000000000000000;
    if branch_cond then {
        let rel8 : sbits(64) = rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(1 + signed(rel8), 48));
        write_iptr(proc_mode, next_rip)
    } else {
        let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
        write_iptr(proc_mode, next_rip)
    }
}

val x86_cmovcc : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cmovcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CMOVCC";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), truncate(reg_mem, 64), rex_byte)
    } else if operand_size == 4 & (in_64bit_mode(proc_mode) | in_compatibility_mode(proc_mode)) then {
        wr32(reg_index(reg, rex_byte, 0b10), rr32(reg_index(reg, rex_byte, 0b10)))
    } else ();
    write_iptr(proc_mode, temp_rip)
}

val x86_setcc : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_setcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SETCC";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let sailval : {|0, 1|} = {
        if branch_cond then 1 else 0
    };
    x86_operand_to_reg_mem(proc_mode, 1, inst_ac?, false, bits_of_int(sailval, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}
