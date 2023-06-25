// Previously ignored REX.W

val x86_push_i : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_i (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-I";
    let byte_imm? : bool = opcode == 0x6a;
    let imm_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_imm?, rex_byte, true, prefixes, false, false, false);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, false, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let imm : sbits(32) = truncate(rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), imm_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false), 32);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, new_rsp, None(), 2, bits_of_int(loghead(ash(operand_size, 3), signed(imm)), 128), alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}
