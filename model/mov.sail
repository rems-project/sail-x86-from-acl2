
/*! Move scalar single/double precision floating-point values
<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 F3 0F 10: MOVSS xmm1, xmm2/m32<br/> 
 F2 0F 10: MOVSD xmm1, xmm2/m64<br/> */
val x86_movss_movsd_op_en_rm : (range(0, 1), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movss_movsd_op_en_rm (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVSS/MOVSD-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let operand_size : {|4, 8|} = {
        if sp_dp == 1 then 8 else 4
    };
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let operand_size : {|4, 8, 16|} = {
        if mod_var == 0b11 then {
            operand_size
        } else 16
    };
    write_xmmi_size(bits_of_int(operand_size, 5), xmm_index, unsigned(xmm_mem));
    write_iptr(proc_mode, temp_rip)
}

/*! Move scalar single/double precision floating-point values
<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 F3 0F 11: MOVSS xmm2/m32, xmm1<br/> 
 F2 0F 11: MOVSD xmm2/m64, xmm1<br/> */
val x86_movss_movsd_op_en_mr : (range(0, 1), proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movss_movsd_op_en_mr (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVSS/MOVSD-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let operand_size : {|4, 8|} = {
        if sp_dp == 1 then 8 else 4
    };
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(bits_of_int(operand_size, 5), xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_xmm_mem(proc_mode, operand_size, inst_ac?, xmm, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

/*! Move aligned packed single/double precision floating-point values
<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 28: MOVAPS xmm1, xmm2/m128<br/> 
 66 0F 28: MOVAPD xmm1, xmm2/m128<br/> */
val x86_movaps_movapd_op_en_rm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movaps_movapd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVAPS/MOVAPD-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_xmmi_size(0b10000, xmm_index, unsigned(xmm_mem));
    write_iptr(proc_mode, temp_rip)
}

/*! Move aligned packed single/double precision floating-point values
<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 29: MOVAPS xmm2/m128, xmm1<br/> 
 66 0F 29: MOVAPD xmm2/m128, xmm1<br/> */
val x86_movaps_movapd_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movaps_movapd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVAPS/MOVAPD-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

/*! <h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 10: MOVUPS xmm1, xmm2/m128<br/> 
 66 0F 10: MOVUPD xmm1, xmm2/m128<br/> 
 F3 0F 6F: MOVDQU xmm1, xmm2/m128<br/> 
 
 <p>Note: The MOVDQU, MOVUPS, and MOVUPD instructions perform 128-bit 
 unaligned loads or stores. They do not generate general-protection 
 exceptions ( #GP ) when operands are not aligned on a 16-byte 
 boundary. If alignment checking is enabled, alignment-check 
 exceptions ( #AC ) may or may not be generated depending on processor 
 implementation when data addresses are not aligned on an 8-byte 
 boundary.</p> */
val x86_movups_movupd_movdqu_op_en_rm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movups_movupd_movdqu_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (xmm_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_xmmi_size(0b10000, xmm_index, unsigned(xmm_mem));
    write_iptr(proc_mode, temp_rip)
}

/*! <h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 11: MOVUPS xmm2/m128, xmm1<br/> 
 66 0F 11: MOVUPD xmm2/m128, xmm1<br/> 
 F3 0F 7F: MOVDQU xmm2/m128, xmm1<br/> */
val x86_movups_movupd_movdqu_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movups_movupd_movdqu_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

/*! Move low packed single/double precision floating-point values
<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 12: MOVLPS xmm, m64<br/> 
 66 0F 12: MOVLPD xmm, m64<br/> */
val x86_movlps_movlpd_op_en_rm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movlps_movlpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVLPS/MOVLPD-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 8, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let mem : bits(64) = truncate(mem, 64);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_xmmi_size(0b01000, xmm_index, unsigned(mem));
    write_iptr(proc_mode, temp_rip)
}

/*! Move low packed single/double precision floating-point values
<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 13: MOVLPS m64, xmm<br/> 
 66 0F 13: MOVLPD m64, xmm<br/> */
val x86_movlps_movlpd_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movlps_movlpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVLPS/MOVLPD-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(64) = truncate(xmmi_size(0b01000, xmm_index), 64);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, sail_zero_extend(xmm, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

/*! Move high packed single/double precision floating-point values
<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 16: MOVHPS xmm, m64<br/> 
 66 0F 16: MOVHPD xmm, m64<br/> */
val x86_movhps_movhpd_op_en_rm : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movhps_movhpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVHPS/MOVHPD-OP/EN-RM";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (mem, increment_rip_by, v_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 8, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let mem : bits(64) = truncate(mem, 64);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let low_qword : bits(64) = truncate(xmmi_size(0b01000, xmm_index), 64);
    let result : bits(128) = mem @ low_qword;
    write_xmmi_size(0b10000, xmm_index, unsigned(result));
    write_iptr(proc_mode, temp_rip)
}

/*! Move high packed single/double precision floating-point values
<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 17: MOVHPS m64, xmm<br/> 
 66 0F 17: MOVHPD m64, xmm<br/> */
val x86_movhps_movhpd_op_en_mr : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_movhps_movhpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-MOVHPS/MOVHPD-OP/EN-MR";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let xmm_index : bits(4) = reg_index(reg, rex_byte, 0b10);
    let xmm : bits(128) = xmmi_size(0b10000, xmm_index);
    let high_qword : bits(64) = xmm[127 .. 64];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(49) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let inst_ac? : bool = true;
    x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, sail_zero_extend(high_qword, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}
