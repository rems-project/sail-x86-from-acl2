
val x86_call_e8_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_call_e8_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CALL-E8-OP/EN-M";
    let offset_size : {|1, 2, 4|} = check_range(0, 4, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, true, true));
    let (flg0, rel16_32) : (option(string), sbits(64)) = rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    let rel16_32 : sbits(32) = truncate(rel16_32, 32);
    if is_some(flg0) then throw(Emsg("Model state error: :RIME-SIZE-OPT-ERROR"));
    let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(next_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let (flg, call_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(next_rip, 48), sail_sign_extend(rel16_32, 48));
    let call_rip : sbits(48) = truncate(call_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :CALL-RIP-INVALID"));
    let rsp_var : bits(64) = read_sptr(proc_mode);
    let addr_size : {|2, 4, 8|} = select_address_size(proc_mode, false);
    let (flg, new_rsp) : (option(string), sbits(64)) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(addr_size), 64));
    if is_some(flg) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"));
    let flg : option(string) = if proc_mode == 0 then {
        wime_size(0, addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), 2, signed(next_rip), alignment_checking_enabled_p(), struct { mem_ptr? = false })
    } else {
        wme_size(proc_mode, addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), 2, unsigned(if addr_size == 2 then {
            sail_zero_extend(n16(truncate(next_rip, 16)), 32)
        } else {
            n32(truncate(next_rip, 32))
        }), alignment_checking_enabled_p(), struct { mem_ptr? = false })
    };
    if is_some(flg) then throw(Emsg("Model state error: :STACK-WRITING-ERROR"));
    write_iptr(proc_mode, call_rip);
    write_sptr(proc_mode, new_rsp)
}

val x86_call_ff_2_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_call_ff_2_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CALL-FF/2-OP/EN-M";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true));
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, call_rip, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48));
    let next_rip : sbits(48) = truncate(next_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, next_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let call_rip : int = if proc_mode == 0 then {
        signed(I64(bits_of_int(call_rip, 64)))
    } else {
        call_rip
    };
    if not_bool(if proc_mode == 0 then {
        canonical_address_p(call_rip)
    } else {
        0 <= call_rip & call_rip <= unsigned(seg_hidden_limits[1])
    }) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'CALL-RIP']"));
    let rsp_var : bits(64) = read_sptr(proc_mode);
    let addr_size : {|2, 4, 8|} = select_address_size(proc_mode, false);
    let (flg, new_rsp) : (option(string), sbits(64)) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(addr_size), 64));
    if is_some(flg) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':CALL', 'FLG']"));
    let check_alignment? : bool = alignment_checking_enabled_p();
    let flg : option(string) = if operand_size == 8 then {
        wime_size(proc_mode, operand_size, rsp_var, 2, call_rip, check_alignment?, struct { mem_ptr? = false })
    } else {
        wme_size(proc_mode, operand_size, rsp_var, 2, nat_of_int(call_rip), check_alignment?, struct { mem_ptr? = false })
    };
    if is_some(flg) then throw(Emsg("Model state error: :STACK-WRITING-ERROR"));
    write_iptr(proc_mode, bits_of_int(call_rip, 48));
    write_sptr(proc_mode, new_rsp)
}

val x86_ret : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_ret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-RET";
    let rsp_var : bits(64) = read_sptr(proc_mode);
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, 0x0000000000000, true, true, true));
    let (flg, new_rsp) : (option(string), sbits(64)) = if opcode == 0xc3 then {
        let (flg1, new_rsp) : (option(string), sbits(64)) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
        if is_some(flg1) then {
            (flg1, 0x0000000000000000)
        } else {
            (None() : option(string), new_rsp)
        }
    } else {
        let (flg1, imm16) : (option(string), int) = rme16(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X", false);
        let imm16 : bits(16) = bits_of_int(imm16, 16);
        if is_some(flg1) then {
            (flg1, 0x0000000000000000)
        } else {
            let (flg1, new_rsp) : (option(string), sbits(64)) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size + unsigned(imm16), 64));
            if is_some(flg1) then {
                (flg1, 0x0000000000000000)
            } else {
                (None() : option(string), new_rsp)
            }
        }
    };
    let new_rsp : sbits(48) = truncate(new_rsp, 48);
    if is_some(flg) then throw(Emsg("Model state error: :IMM-RML16-ERROR"));
    let badlength? : bool = opcode == 0xc2 & is_some(check_instruction_length(start_rip, temp_rip, 0b010));
    if badlength? then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let check_alignment? : bool = alignment_checking_enabled_p();
    let (flg, tos) : (option(string), int) = if operand_size == 8 then {
        let (elem0, elem1) : (option(string), sbits(64)) = rime_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, struct { mem_ptr? = false });
        (elem0, signed(elem1))
    } else {
        rme_size(proc_mode, operand_size, rsp_var, 2, ":R", check_alignment?, struct { mem_ptr? = false })
    };
    let tos : sbits(64) = bits_of_int(tos, 64);
    if is_some(flg) then throw(Emsg("This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`"));
    if not_bool(if proc_mode == 0 then {
        canonical_address_p(signed(tos))
    } else {
        let cs_limit : bits(32) = seg_hidden_limits[1];
        0 <= signed(tos) & signed(tos) <= unsigned(cs_limit)
    }) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'TOS']"));
    write_sptr(proc_mode, sail_sign_extend(new_rsp, 64));
    rip = sail_zero_extend(truncate(tos, 48), 64)
}

val x86_leave : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_leave (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LEAVE";
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false));
    let rbp_ebp_bp : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x5, 0x00);
    let inst_ac? : bool = alignment_checking_enabled_p();
    let (flg, sailval) : (option(string), int) = rme_size(proc_mode, operand_size, I64(rbp_ebp_bp), 2, ":R", inst_ac?, struct { mem_ptr? = false });
    if is_some(flg) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':SS', '0', ':POP-ERROR', 'FLG']"));
    let (flg, new_rsp) : (option(string), sbits(64)) = add_to_sptr(proc_mode, I64(rbp_ebp_bp), bits_of_int(operand_size, 64));
    if is_some(flg) then throw(Emsg("Model state error: :INVALID-RSP"));
    write_rgfi_size(bits_of_int(operand_size, 4), 0x5, sailval, rex_byte);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}
