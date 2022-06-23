$include "./arith_and_logic_spec.sail"
$include "./decoding_and_spec_utils.sail"

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP/TEST-E-G";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let byte_operand? : bool = 0b0 == truncate(logand(0x01, opcode), 1);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let g : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match operand_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(operation, bits_of_int(e, 8), truncate(g, 8), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(operation, bits_of_int(e, 16), truncate(g, 16), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(operation, bits_of_int(e, 32), truncate(g, 32), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(operation, bits_of_int(e, 64), g, input_rflags)
    };
    let flg1 : option(string) = if operation == 8 | operation == 7 then {
        None() : option(string)
    } else {
        x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)
    };
    if is_some(flg1) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, temp_rip)
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_g_e : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_g_e (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-G-E";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let byte_operand? : bool = 0b0 == truncate(logand(0x01, opcode), 1);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let g : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match operand_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(operation, truncate(g, 8), bits_of_int(e, 8), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(operation, truncate(g, 16), bits_of_int(e, 16), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(operation, truncate(g, 32), bits_of_int(e, 32), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(operation, g, bits_of_int(e, 64), input_rflags)
    };
    if operation == 8 then () else {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(result), rex_byte)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, temp_rip)
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-E-I";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let e_byte_operand? : bool = opcode == 0x80 | opcode == 0x82 | opcode == 0xf6;
    let e_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, e_byte_operand?, rex_byte, false, prefixes, false, false, false);
    let imm_byte_operand? : bool = opcode == 0x80 | opcode == 0x82 | opcode == 0x83 | opcode == 0xf6;
    let imm_size : {|1, 2, 4|} = check_range(1, 4, select_operand_size(proc_mode, imm_byte_operand?, rex_byte, true, prefixes, false, false, false));
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, e, increment_rip_by, e_addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, e_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3));
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let (flg1, imm) : (option(string), int) = rme_size(proc_mode, imm_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    let imm : bits(32) = bits_of_int(imm, 32);
    if is_some(flg1) then throw(Emsg("Model state error: :RME-SIZE-ERROR"));
    let imm : int = loghead(ash(e_size, 3), binary_logext(ash(imm_size, 3), unsigned(imm)));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48));
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match e_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(operation, bits_of_int(e, 8), bits_of_int(imm, 8), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(operation, bits_of_int(e, 16), bits_of_int(imm, 16), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(operation, bits_of_int(e, 32), bits_of_int(imm, 32), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(operation, bits_of_int(e, 64), bits_of_int(imm, 64), input_rflags)
    };
    let flg1 : option(string) = if operation == 8 | operation == 7 then {
        None() : option(string)
    } else {
        x86_operand_to_reg_mem(proc_mode, e_size, inst_ac?, false, unsigned(result), seg_reg, e_addr, rex_byte, r_m, mod_var)
    };
    if is_some(flg1) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

val x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i : (range(0, 8), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i (operation, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-ADD/ADC/SUB/SBB/OR/AND/XOR/CMP-TEST-RAX-I";
    let byte_operand? : bool = 0x00 == logand(0x01, opcode);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false);
    let rax_size : {|1, 2, 4, 8|} = if logbitp(3, rex_byte) then 8 else {
        operand_size
    };
    let rax_var : bits(64) = rgfi_size(bits_of_int(rax_size, 4), 0x0, rex_byte);
    let (flg, imm) : (option(string), int) = rme_size(proc_mode, operand_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    if is_some(flg) then throw(Emsg("Model state error: :RME-SIZE-ERROR"));
    let imm : bits(64) = if not_bool(byte_operand?) & rax_size == 8 then {
        sail_sign_extend(bits_of_int(imm, 32), 64)
    } else {
        sail_zero_extend(bits_of_int(imm, 32), 64)
    };
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48));
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match rax_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(operation, truncate(rax_var, 8), truncate(imm, 8), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(operation, truncate(rax_var, 16), truncate(imm, 16), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(operation, truncate(rax_var, 32), truncate(imm, 32), input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(operation, rax_var, imm, input_rflags)
    };
    if operation == 8 | operation == 7 then () else {
        write_rgfi_size(bits_of_int(rax_size, 4), 0x0, unsigned(result), rex_byte)
    };
    write_user_rflags(output_rflags, undefined_flags);
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

val x86_inc_dec_fe_ff : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_inc_dec_fe_ff (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-INC/DEC-FE-FF";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let select_byte_operand : bool = 0x00 == logand(0x01, opcode);
    let r_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, r_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match r_mem_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(if reg == 0b000 then 0 else 4, bits_of_int(r_mem, 8), 0x01, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(if reg == 0b000 then 0 else 4, bits_of_int(r_mem, 16), 0x0001, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(if reg == 0b000 then 0 else 4, bits_of_int(r_mem, 32), 0x00000001, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(if reg == 0b000 then 0 else 4, bits_of_int(r_mem, 64), 0x0000000000000001, input_rflags)
    };
    let output_rflags : bits(32) = set_rflagsbits_get_cf(old_cf, output_rflags);
    write_user_rflags(output_rflags, undefined_flags);
    let flg1 : option(string) = x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var);
    if is_some(flg1) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_iptr(proc_mode, temp_rip)
}

val x86_inc_dec_4x : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_inc_dec_4x (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-INC/DEC-4X";
    let operand_size : {|2, 4|} = check_range(2, 4, select_operand_size(proc_mode, false, 0x00, false, prefixes, false, false, false));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let reg : bits(3) = truncate(logand(0x07, opcode), 3);
    let operand : bits(64) = rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), 0x00);
    let input_rflags : bits(32) = rflags;
    let old_cf : bits(1) = rflagsbits_get_cf(input_rflags);
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match operand_size {
      1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
        gpr_arith_logic_spec_1(if logbitp(3, opcode) then 4 else 0, truncate(operand, 8), 0x01, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
        gpr_arith_logic_spec_2(if logbitp(3, opcode) then 4 else 0, truncate(operand, 16), 0x0001, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
        gpr_arith_logic_spec_4(if logbitp(3, opcode) then 4 else 0, truncate(operand, 32), 0x00000001, input_rflags)
      in
        (sail_zero_extend(elem0, 64), elem1, elem2),
      _ => gpr_arith_logic_spec_8(if logbitp(3, opcode) then 4 else 0, operand, 0x0000000000000001, input_rflags)
    };
    let output_rflags : bits(32) = set_rflagsbits_get_cf(old_cf, output_rflags);
    write_user_rflags(output_rflags, undefined_flags);
    write_rgfi_size(bits_of_int(operand_size, 4), sail_zero_extend(reg, 4), unsigned(result), 0x00);
    write_iptr(proc_mode, temp_rip)
}

val x86_not_neg_f6_f7 : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_not_neg_f6_f7 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-NOT/NEG-F6-F7";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let select_byte_operand : bool = 0x00 == logand(0x01, opcode);
    let r_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, r_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, r_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let input_rflags : bits(32) = rflags;
    let (result, output_rflags, undefined_flags) : (bits(64), bits(32), bits(32)) = match reg {
      3 => (match r_mem_size {
        1 => let (elem0, elem1, elem2) : (bits(8), bits(32), bits(32)) =
          gpr_arith_logic_spec_1(4, 0x00, bits_of_int(r_mem, 8), input_rflags)
        in
          (sail_zero_extend(elem0, 64), elem1, elem2),
        2 => let (elem0, elem1, elem2) : (bits(16), bits(32), bits(32)) =
          gpr_arith_logic_spec_2(4, 0x0000, bits_of_int(r_mem, 16), input_rflags)
        in
          (sail_zero_extend(elem0, 64), elem1, elem2),
        4 => let (elem0, elem1, elem2) : (bits(32), bits(32), bits(32)) =
          gpr_arith_logic_spec_4(4, 0x00000000, bits_of_int(r_mem, 32), input_rflags)
        in
          (sail_zero_extend(elem0, 64), elem1, elem2),
        _ => gpr_arith_logic_spec_8(4, 0x0000000000000000, bits_of_int(r_mem, 64), input_rflags)
      }) : (bits(64), bits(32), bits(32)),
      _ => (sail_mask(64, trunc(r_mem_size, bits_of_int(lognot(r_mem), 64))), 0x00000000, 0x00000000)
    };
    if reg == 0b011 then {
        let cf : bits(1) = bits_of_int(if 0 == r_mem then 0 else 1, 1);
        let output_rflags : bits(32) = set_rflagsbits_get_cf(cf, output_rflags);
        write_user_rflags(output_rflags, undefined_flags)
    };
    let flg1 : option(string) = x86_operand_to_reg_mem(proc_mode, r_mem_size, inst_ac?, false, unsigned(result), seg_reg, addr, rex_byte, r_m, mod_var);
    if is_some(flg1) then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"));
    write_iptr(proc_mode, temp_rip)
}
