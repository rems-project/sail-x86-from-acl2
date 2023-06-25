
val x86_call_ff_2_op_en_m : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_call_ff_2_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CALL-FF/2-OP/EN-M";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true));
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (call_rip, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let call_rip : sbits(64) = truncate(call_rip, 64);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let next_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, next_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let call_rip : sbits(64) = {
        if in_64bit_mode(proc_mode) then {
            call_rip
        } else {
            call_rip
        }
    };
    if not_bool(if in_64bit_mode(proc_mode) then {
        canonical_address_p(signed(call_rip))
    } else {
        0 <= signed(call_rip) & signed(call_rip) <= unsigned(seg_hidden_limits[1])
    }) then x86_fault("(!!FAULT-FRESH :GP 0 :BAD-RETURN-ADDRESS CALL-RIP)");
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let addr_size : address_size = select_address_size(proc_mode, None());
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(addr_size), 64));
    let check_alignment? : bool = alignment_checking_enabled_p();
    if in_64bit_mode(proc_mode) then {
        wime_size(Mode_64bit, select_address_size(proc_mode, Some(prefixes)), addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_sign_extend(next_rip, 128), alignment_checking_enabled_p(), false)
    } else {
        wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), addr_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, if addr_size == 2 then {
            sail_zero_extend(sail_zero_extend(truncate(next_rip, 16), 32), 128)
        } else {
            sail_zero_extend(truncate(next_rip, 32), 128)
        }, alignment_checking_enabled_p(), false)
    };
    write_iptr(proc_mode, truncate(call_rip, 48));
    write_sptr(proc_mode, new_rsp)
}
