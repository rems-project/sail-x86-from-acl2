
val jcc_cmovcc_setcc_spec : bits(8) -> bool

function jcc_cmovcc_setcc_spec opcode = {
    let low_nibble : bits(4) = truncate(logand(opcode, 0x0f), 4);
    (match low_nibble {
      0 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1),
      1 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1),
      2 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_cf(rflags_var), 2), 1),
      3 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_cf(rflags_var), 2), 1),
      4 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1),
      5 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1),
      6 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_cf(rflags_var), 2), 1) | 0b1 == truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1),
      7 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_cf(rflags_var), 2), 1) & 0b0 == truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1),
      8 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1),
      9 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1),
      10 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_pf(rflags_var), 2), 1),
      11 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_pf(rflags_var), 2), 1),
      12 => not_bool(truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1) == truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1)),
      13 => truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1) == truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1),
      14 => 0b1 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1) | not_bool(truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1) == truncate(let rflags_var : bits(32) = rflags in
        sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1)),
      15 => 0b0 == truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(rflagsbits_get_zf(rflags_var), 2), 1) & truncate(let rflags_var : bits(32) =
        rflags
      in
        sail_zero_extend(rflagsbits_get_sf(rflags_var), 2), 1) == truncate(let rflags_var : bits(32) = rflags in
        sail_zero_extend(rflagsbits_get_of(rflags_var), 2), 1),
      _ => false
    }) : bool
}

val x86_one_byte_jcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_one_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ONE-BYTE-JCC";
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b001);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        let (flg, rel8) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
        if is_some(flg) then throw(Emsg("Model state error: :RIME-SIZE-ERROR"));
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(1 + signed(rel8), 48));
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    } else {
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    }
}

val x86_two_byte_jcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_two_byte_jcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-TWO-BYTE-JCC";
    let offset_size : {|1, 2, 4|} = check_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, bits_of_int(offset_size, 3));
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        let (flg, offset) : (option(string), sbits(64)) = rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
        if is_some(flg) then throw(Emsg("Model state error: :RIME-SIZE-ERROR"));
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size + signed(offset), 48));
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    } else {
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    }
}

val x86_jrcxz : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_jrcxz (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-JRCXZ";
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b001);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let register_size : {|2, 4, 8|} = select_address_size(proc_mode, p4?);
    let branch_cond : bool = rgfi_size(bits_of_int(register_size, 4), 0x1, rex_byte) == 0x0000000000000000;
    if branch_cond then {
        let (flg, rel8) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
        if is_some(flg) then throw(Emsg("Model state error: :RIME-SIZE-ERROR"));
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(1 + signed(rel8), 48));
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    } else {
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_iptr(proc_mode, truncate(next_rip, 48))
    }
}

val x86_cmovcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cmovcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CMOVCC";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, reg_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    if branch_cond then {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), reg_mem, rex_byte)
    };
    write_iptr(proc_mode, temp_rip)
}

val x86_setcc : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_setcc (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SETCC";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (None() : option(string), 0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let branch_cond : bool = jcc_cmovcc_setcc_spec(opcode);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let sailval : {|0, 1|} = {
        if branch_cond then 1 else 0
    };
    let flg2 : option(string) = x86_operand_to_reg_mem(proc_mode, 1, inst_ac?, false, sailval, seg_reg, addr, rex_byte, r_m, mod_var);
    if is_some(flg2) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_iptr(proc_mode, temp_rip)
}
