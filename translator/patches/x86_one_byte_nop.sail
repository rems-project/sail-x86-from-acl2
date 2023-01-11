val x86_one_byte_nop : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_one_byte_nop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_iptr(proc_mode, temp_rip)
}
