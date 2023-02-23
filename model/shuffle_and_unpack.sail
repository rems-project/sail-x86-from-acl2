
val extract_32_bits : (bits(128), bits(2)) -> bits(32)

function extract_32_bits (x, n) = {
    (match n {
      0 => x[31 .. 0],
      1 => x[63 .. 32],
      2 => x[95 .. 64],
      _ => x[127 .. 96]
    }) : bits(32)
}

val extract_64_bits : (bits(128), range(0, 1)) -> bits(64)

function extract_64_bits (x, n) = {
    (match n {
      0 => x[63 .. 0],
      _ => x[127 .. 64]
    }) : bits(64)
}

/*! Shuffle packed single-precision floating-point values
<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 0F C6: SHUFPS xmm1, xmm2/m128, imm8<br/> */
val x86_shufps_op_en_rmi : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHUFPS-OP/EN-RMI";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let imm : bits(8) = truncate(imm, 8);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let imm0 : bits(2) = imm[1 .. 0];
    let imm1 : bits(2) = imm[3 .. 2];
    let imm2 : bits(2) = imm[5 .. 4];
    let imm3 : bits(2) = imm[7 .. 6];
    let dword0 : bits(32) = extract_32_bits(xmm, imm0);
    let dword1 : bits(32) = extract_32_bits(xmm, imm1);
    let dword2 : bits(32) = extract_32_bits(xmm_mem, imm2);
    let dword3 : bits(32) = extract_32_bits(xmm_mem, imm3);
    let result : bits(128) = dword3 @ (dword2 @ (dword1 @ dword0));
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

/*! Shuffle packed double-precision floating-point values
<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 66 0F C6: SHUFPD xmm1, xmm2/m128, imm8<br/> */
val x86_shufpd_op_en_rmi : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SHUFPD-OP/EN-RMI";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let imm : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false);
    let imm : bits(8) = truncate(imm, 8);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let imm0 : bits(1) = logbit(0, imm);
    let imm1 : bits(1) = logbit(1, imm);
    let qword0 : bits(64) = extract_64_bits(xmm, unsigned(imm0));
    let qword1 : bits(64) = extract_64_bits(xmm_mem, unsigned(imm1));
    let result : bits(128) = qword1 @ qword0;
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

/*! Unpack and interleave low/high packed single-precision floating-point 
 values
<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 14: UNPCKLPS xmm1, xmm2/m128<br/> 
 0F 15: UNPCKHPS xmm1, xmm2/m128<br/> */
val x86_unpck?ps_op_en_rm : (range(0, 1), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-UNPCK?PS-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let dword0 : bits(32) = {
        if high_low == 1 then {
            xmm[95 .. 64]
        } else {
            xmm[31 .. 0]
        }
    };
    let dword1 : bits(32) = {
        if high_low == 1 then {
            xmm_mem[95 .. 64]
        } else {
            xmm_mem[31 .. 0]
        }
    };
    let dword2 : bits(32) = {
        if high_low == 1 then {
            xmm[127 .. 96]
        } else {
            xmm[63 .. 32]
        }
    };
    let dword3 : bits(32) = {
        if high_low == 1 then {
            xmm_mem[127 .. 96]
        } else {
            xmm_mem[63 .. 32]
        }
    };
    let result : bits(128) = dword3 @ (dword2 @ (dword1 @ dword0));
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

/*! Unpack and interleave low/high packed double-precision floating-point 
 values
<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 66 0F 14: UNPCKLPD xmm1, xmm2/m128<br/> 
 66 0F 15: UNPCKHPD xmm1, xmm2/m128<br/> */
val x86_unpck?pd_op_en_rm : (range(0, 1), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-UNPCK?PD-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let qword0 : bits(64) = {
        if high_low == 1 then {
            xmm[127 .. 64]
        } else {
            xmm[63 .. 0]
        }
    };
    let qword1 : bits(64) = {
        if high_low == 1 then {
            xmm_mem[127 .. 64]
        } else {
            xmm_mem[63 .. 0]
        }
    };
    let result : bits(128) = qword1 @ qword0;
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}
