// Corrects operand size calculation

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (range(0, 8), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I";
    let byte_operand? : bool = 0x00 == logand(0x01, opcode);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false);
    let rax_size : {|1, 2, 4, 8|} = {
        if logbitp(3, rex_byte) then 8 else {
            operand_size
        }
    };
    let rax_var : bits(64) = rgfi_size(bits_of_int(rax_size, 4), 0x0, rex_byte);
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let (final_operand_size, imm) : ({|1, 2, 4, 8|}, bits(64)) = {
        if not_bool(byte_operand?) & rax_size == 8 then {
            (8, sail_sign_extend(truncate(imm, 32), 64))
        } else {
            (operand_size, sail_zero_extend(truncate(imm, 32), 64))
        }
    };
    let temp_rip : sbits(49) = sail_sign_extend(add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48)), 49);
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let input_rflags : rflagsbits = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), rflagsbits, rflagsbits) = gpr_arith_logic_spec(rax_size, operation, rax_var, imm, input_rflags);
    if operation == 8 | operation == 7 then () else {
        write_rgfi_size(bits_of_int(final_operand_size, 4), 0x0, result, rex_byte)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}
