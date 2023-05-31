// Ignored REX.W

val x86_push_general_register : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_general_register (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-GENERAL-REGISTER";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, false, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let reg : bits(3) = loghead(3, opcode);
    let sailval : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b00), rex_byte);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_zero_extend(sailval, 128), alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}
