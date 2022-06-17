
val extract_32_bits : (bits(128), bits(2)) -> bits(32)

function extract_32_bits (x, n) = {
    (match n {
      0 => slice(x, 0, 32),
      1 => slice(x, 32, 32),
      2 => slice(x, 64, 32),
      _ => slice(x, 96, 32)
    }) : bits(32)
}

val extract_64_bits : (bits(128), range(0, 1)) -> bits(64)

function extract_64_bits (x, n) = {
    (match n {
      0 => slice(x, 0, 64),
      _ => slice(x, 64, 64)
    }) : bits(64)
}

val x86_shufps_op_en_rmi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHUFPS-OP/EN-RMI";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let xmm_mem : bits(128) = bits_of_int(xmm_mem, 128);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let (flg1, imm) : (option(string), int) = rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    let imm : bits(8) = bits_of_int(imm, 8);
    if is_some(flg1) then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let imm0 : bits(2) = slice(imm, 0, 2);
    let imm1 : bits(2) = slice(imm, 2, 2);
    let imm2 : bits(2) = slice(imm, 4, 2);
    let imm3 : bits(2) = slice(imm, 6, 2);
    let dword0 : bits(32) = extract_32_bits(xmm, imm0);
    let dword1 : bits(32) = extract_32_bits(xmm, imm1);
    let dword2 : bits(32) = extract_32_bits(xmm_mem, imm2);
    let dword3 : bits(32) = extract_32_bits(xmm_mem, imm3);
    let result : bits(128) = merge_4_u32s(dword3, dword2, dword1, dword0);
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

val x86_shufpd_op_en_rmi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHUFPD-OP/EN-RMI";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let xmm_mem : bits(128) = bits_of_int(xmm_mem, 128);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let (flg1, imm) : (option(string), int) = rme_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    let imm : bits(8) = bits_of_int(imm, 8);
    if is_some(flg1) then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let imm0 : bits(1) = logbit(0, imm);
    let imm1 : bits(1) = logbit(1, imm);
    let qword0 : bits(64) = extract_64_bits(xmm, unsigned(imm0));
    let qword1 : bits(64) = extract_64_bits(xmm_mem, unsigned(imm1));
    let result : bits(128) = merge_2_u64s(qword1, qword0);
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

val x86_unpck?ps_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-UNPCK?PS-OP/EN-RM";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let xmm_mem : bits(128) = bits_of_int(xmm_mem, 128);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let dword0 : bits(32) = if high_low == 1 then {
        slice(xmm, 64, 32)
    } else {
        slice(xmm, 0, 32)
    };
    let dword1 : bits(32) = if high_low == 1 then {
        slice(xmm_mem, 64, 32)
    } else {
        slice(xmm_mem, 0, 32)
    };
    let dword2 : bits(32) = if high_low == 1 then {
        slice(xmm, 96, 32)
    } else {
        slice(xmm, 32, 32)
    };
    let dword3 : bits(32) = if high_low == 1 then {
        slice(xmm_mem, 96, 32)
    } else {
        slice(xmm_mem, 32, 32)
    };
    let result : bits(128) = merge_4_u32s(dword3, dword2, dword1, dword0);
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

val x86_unpck?pd_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-UNPCK?PD-OP/EN-RM";
    let r_m : bits(3) = modr_m_get_r_m(modr_m);
    let mod_var : bits(2) = modr_m_get_mod(modr_m);
    let reg : bits(3) = modr_m_get_reg(modr_m);
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes_get_seg(prefixes);
    let p4? : bool = 0x67 == prefixes_get_adr(prefixes);
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let xmm_mem : bits(128) = bits_of_int(xmm_mem, 128);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"));
    let qword0 : bits(64) = if high_low == 1 then {
        slice(xmm, 64, 64)
    } else {
        slice(xmm, 0, 64)
    };
    let qword1 : bits(64) = if high_low == 1 then {
        slice(xmm_mem, 64, 64)
    } else {
        slice(xmm_mem, 0, 64)
    };
    let result : bits(128) = merge_2_u64s(qword1, qword0);
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}
