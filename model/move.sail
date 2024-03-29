
val x86_mov_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0x88;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let register_var : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(register_var, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_rm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0x8a;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), truncate(reg_mem, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_fd : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_fd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-FD";
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0xa0;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let offset_size : moffset_size = select_moffset_size(proc_mode, Some(prefixes));
    let offset : sbits(64) = rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, 0b00, 0b000, sib);
    let inst_ac? : bool = alignment_checking_enabled_p();
    let data : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, offset, None(), seg_reg, ":R", inst_ac?, false);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x0, truncate(data, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_td : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_td (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-TD";
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0xa2;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let offset_size : moffset_size = select_moffset_size(proc_mode, Some(prefixes));
    let offset : sbits(64) = rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, 0b00, 0b000, sib);
    let data : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x0, rex_byte);
    let inst_ac? : bool = true;
    x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(data, 128), seg_reg, offset, prefixes, rex_byte, 0b000, 0b00, sib);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_oi : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_oi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-OI";
    let byte_operand? : bool = 176 <= unsigned(opcode) & unsigned(opcode) <= 183;
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false);
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let reg : bits(3) = truncate(logand(0x07, opcode), 3);
    write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b00), truncate(imm, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_mi : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_op_en_mi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-OP/EN-MI";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let byte_operand? : bool = opcode == 0xc6;
    let imm_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false);
    let reg_mem_size : {|1, 2, 4, 8|} = {
        if opcode == 0xc7 & logbitp(3, rex_byte) then 8 else {
            imm_size
        }
    };
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3))
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), imm_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let imm : bits(128) = {
        if reg_mem_size == 8 then {
            sail_zero_extend(sail_sign_extend(truncate(imm, 32), 64), 128)
        } else {
            imm
        }
    };
    let inst_ac? : bool = true;
    x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, imm, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

val x86_lea : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lea (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LEA";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p4? : bool = 0x67 == prefixes[adr];
    let register_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false));
    let (m, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let m : bits(64) = sail_mask(64, trunc_signed(register_size, m));
    write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), m, rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_movsx : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movsx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVSX";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, true, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let register_size : {|1, 2, 4, 8|} = {
        if logbitp(3, rex_byte) then 8 else {
            reg_mem_size
        }
    };
    let reg_mem : bits(128) = {
        if register_size == 8 then {
            sail_zero_extend(sail_sign_extend(truncate(reg_mem, 32), 64), 128)
        } else {
            reg_mem
        }
    };
    write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), truncate(reg_mem, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_movsxd : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movsxd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVSXD";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let reg_mem_size : {|1, 2|} = {
        if opcode == 0xbe then 1 else 2
    };
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let register_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let reg_mem : bits(128) = {
        match reg_mem_size {
          1 => sail_zero_extend(sail_mask(64, signed_bitslice(truncate(reg_mem, 8), 0, ash(register_size, 3))), 128),
          2 => {
              (match register_size {
                2 => reg_mem,
                _ => sail_zero_extend(sail_mask(64, signed_bitslice(truncate(reg_mem, 16), 0, ash(register_size, 3))), 128)
              }) : bits(128)
          }
        }
    };
    write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), truncate(reg_mem, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_movzx : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movzx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVZX";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let reg_mem_size : {|1, 2|} = {
        if opcode == 0xb6 then 1 else 2
    };
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let register_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), truncate(reg_mem, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_mov_control_regs_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_mov_control_regs_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOV-CONTROL-REGS-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let ctr_index : bits(4) = {
        if logbitp(2, rex_byte) & reg == 0b000 then 0x8 else {
            sail_zero_extend(reg, 4)
        }
    };
    let operand_size : {|4, 8|} = {
        if in_64bit_mode(proc_mode) then 8 else 4
    };
    let ctr_val : bits(64) = ctrs[unsigned(ctr_index)];
    let ctr_val : bits(64) = {
        if operand_size == 4 then {
            sail_zero_extend(truncate(ctr_val, 32), 64)
        } else {
            ctr_val
        }
    };
    write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), ctr_val, rex_byte);
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with cf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with pf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with af = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with zf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with sf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with of = sailval] in rflags = new_rflags;
    write_iptr(proc_mode, temp_rip)
}

val x86_movd_movq_to_xmm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movd_movq_to_xmm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVD/MOVQ-TO-XMM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|4, 8|} = {
        if logbitp(3, rex_byte) then 8 else 4
    };
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_xmmi_size(bits_of_int(operand_size, 5), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem));
    write_iptr(proc_mode, temp_rip)
}

val x86_movd_movq_from_xmm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movd_movq_from_xmm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVD/MOVQ-FROM-XMM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|4, 8|} = {
        if logbitp(3, rex_byte) then 8 else 4
    };
    let register_var : bits(128) = xmmi_size(bits_of_int(operand_size, 5), reg_index(reg, rex_byte, 0b10));
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, register_var, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}
