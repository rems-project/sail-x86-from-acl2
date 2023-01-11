
/*! PUSH: 50+rw/rd
<p>Op/En: O</p> 
 <p><tt>50+rw/rd r16/r32/r64</tt></p> 
 <p>Note that <tt>50+rd r32</tt> is N.E. in 64-bit mode 
 and that <tt>50+rd r64</tt> is N.E. in 32-bit mode.</p> 
 
 <p>PUSH does not have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled with the execution in 
 this case.</p> */
val x86_push_general_register : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_general_register (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-GENERAL-REGISTER";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
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

/*! PUSH: FF /6 r/m
<p>Op/En: M</p> 
 <p><tt>FF /6 r/m16/32/64</tt></p> 
 <p>Note that <tt>FF/6 r/m32</tt> is N.E. in 64-bit mode 
 and that <tt>FF/6 r/m64</tt> is N.E. in 32-bit mode.</p> 
 
 <p>PUSH does not have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled with the execution in 
 this case.</p> 
 
 <p>This opcode belongs to Group 5, and it has an opcode 
 extension ( ModR/m.reg = 6 ) .</p> */
val x86_push_ev : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_ev (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-EV";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let (e, increment_rip_by, e_addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, true, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, e, alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}

/*! PUSH: 6A/68 ib/iw/id
<p>Op/En: I</p> 
 <p><tt>6A ib</tt>: PUSH imm8</p> 
 <p><tt>68 iw</tt>: PUSH imm16</p> 
 <p><tt>68 id</tt>: PUSH imm32</p> 
 
 <p>From the description of the PUSH instruction ( Intel Manual, Vol. 2, 
 Section 4.2 ) :</p> 
 
 <p><i> If the source operand is an immediate of size less than the operand 
 size, a sign-extended value is pushed on the stack.</i></p> 
 
 <p>PUSH doesn ' t have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled the decoding with the 
 execution in this case.</p> */
val x86_push_i : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_i (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-I";
    let byte_imm? : bool = opcode == 0x6a;
    let imm_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, byte_imm?, rex_byte, true, prefixes, false, false, false);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let imm : sbits(32) = truncate(rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), imm_size, temp_rip, None(), 1, ":X", false, false), 32);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, new_rsp, None(), 2, bits_of_int(loghead(ash(operand_size, 3), signed(imm)), 128), alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}

/*! PUSH Segment Register
<p>Note that PUSH CS/SS/DS/ES are invalid in 64-bit mode. Only PUSH FS/GS 
 are valid in 64-bit mode.</p> 
 
 <p><tt>0E</tt>: [PUSH CS]</p> 
 <p><tt>16</tt>: [PUSH SS]</p> 
 <p><tt>1E</tt>: [PUSH DS]</p> 
 <p><tt>06</tt>: [PUSH ES]</p> 
 <p><tt>0F A0</tt>: [PUSH FS]</p> 
 <p><tt>0F A8</tt>: [PUSH GS]</p> 
 
 <p>If the source operand is a segment register ( 16 bits ) and the operand 
 size is 64-bits, a zero-extended value is pushed on the stack; if the 
 operand size is 32-bits, either a zero-extended value is pushed on the stack 
 or the segment selector is written on the stack using a 16-bit move. For the 
 last case, all recent Core and Atom processors perform a 16-bit move, 
 leaving the upper portion of the stack location unmodified.</p> 
 
 <p>For now, our model handles the last case described above by doing a 
 16-bit move. This should be how all modern processor work. In the future, we 
 might parameterize the model on a flag that says how this case is handled 
 ( modern or legacy ) .</p> 
 
 <p>PUSH doesn ' t have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled with the execution in 
 this case.</p> */
val x86_push_segment_register : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_push_segment_register (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSH-SEGMENT-REGISTER";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let sailval : bits(16) = seg_visibles[(match opcode {
      14 => 1,
      22 => 2,
      30 => 3,
      6 => 0,
      160 => 4,
      _ => 5
    }) : {|0, 1, 2, 3, 4, 5|}];
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), if operand_size == 4 then 2 else {
        operand_size
    }, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_zero_extend(sailval, 128), alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}

/*! POP: 58+rw/rd
<p>Op/En: O</p> 
 <p><tt>58+rw/rd r16/r32/r64</tt></p> 
 <p>Note that <tt>58+rd r32</tt> is N.E. in the 64-bit mode 
 and that <tt>58+rd r64</tt> is N.E. in 32-bit mode.</p> 
 
 <p>POP does not have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled with the execution in 
 this case.</p> */
val x86_pop_general_register : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_pop_general_register (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-POP-GENERAL-REGISTER";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let sailval : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", alignment_checking_enabled_p(), false);
    let reg : bits(8) = logand(opcode, 0x07);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_sptr(proc_mode, new_rsp);
    write_rgfi_size(bits_of_int(operand_size, 4), reg_index(truncate(reg, 3), rex_byte, 0b00), truncate(sailval, 64), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

/*! POP: 8F/0 r/m
<p>Op/En: M</p> 
 <p><tt>8F/0 r/m16/32/64</tt></p> 
 <p>Note that <tt>8F/0 r/m32</tt> is N.E. in 64-bit mode 
 and that <tt>8F/0 r/m64</tt> is N.E. in 32-bit mode.</p> 
 
 <p>POP does not have a separate instruction semantic function, unlike other 
 opcodes like ADD, SUB, etc. The decoding is coupled with the execution in 
 this case.</p> 
 
 <p>This opcode belongs to Group 1A, and it has an opcode 
 extension ( ModR/m.reg = 0 ) .</p> */
val x86_pop_ev : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_pop_ev (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-POP-EV";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let check_alignment? : bool = alignment_checking_enabled_p();
    let sailval : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    write_sptr(proc_mode, new_rsp);
    x86_operand_to_reg_mem(proc_mode, operand_size, check_alignment?, false, sailval, seg_reg, addr, prefixes, rex_byte, r_m, mod_var, sib);
    write_iptr(proc_mode, temp_rip)
}

val x86_pushf : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_pushf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSHF";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    let eflags : rflagsbits = rflags;
    let eflags : bits(32) = {
        match operand_size {
          2 => bits_of_int(logand(65535, unsigned(eflags.bits)), 32),
          _ => bits_of_int(logand(3997695, unsigned(eflags.bits)), 32)
        }
    };
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(truncate(new_rsp, 48), 64), None(), 2, sail_zero_extend(eflags, 128), alignment_checking_enabled_p(), false);
    write_sptr(proc_mode, new_rsp);
    write_iptr(proc_mode, temp_rip)
}

val x86_popf : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_popf (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-POPF";
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, false);
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let new_rsp : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let sailval : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", alignment_checking_enabled_p(), false);
    let sailval : bits(32) = logior(0x00000002, truncate(logand(0x000000000000000000000000003f7fd7, sailval), 32));
    write_sptr(proc_mode, new_rsp);
    (match operand_size {
      2 => rflags = Mk_rflagsbits(sailval),
      _ => {
          let rf : bits(1) = {
              let rflags_var : rflagsbits = rflags;
              rflags_var[rf]
          };
          let vm : bits(1) = {
              let rflags_var : rflagsbits = rflags;
              rflags_var[vm]
          };
          rflags = Mk_rflagsbits(sailval);
          let rflags_var : rflagsbits = rflags in
          let new_rflags : rflagsbits = [rflags_var with rf] in rflags = new_rflags;
          let rflags_var : rflagsbits = rflags in
          let new_rflags : rflagsbits = [rflags_var with vm] in rflags = new_rflags;
          let rflags_var : rflagsbits = rflags in
          let new_rflags : rflagsbits = [rflags_var with vip = 0b0] in rflags = new_rflags;
          let rflags_var : rflagsbits = rflags;
          let new_rflags : rflagsbits = [rflags_var with vif = 0b0];
          rflags = new_rflags
      }
    }) : unit;
    write_iptr(proc_mode, temp_rip)
}

/*! PUSHA/PUSHAD: 60
<p> 
 In 64-bit mode, this instruction is invalid; it throws a #UD exception. 
 </p> 
 <p> 
 Note that the stack pointer is read twice: 
 via @ ( tsee read-*sp ) and via @ ( tsee rgfi-size ) . 
 The former is used as the address to write into the stack, 
 while the latter is used as ( part of ) the data to write into the stack. 
 In principle, the sizes of these two stack pointers may differ: 
 the former ' s size is determined solely by CS.D, 
 while the latter ' s size is also influenced 
 by the operand size override prefix. 
 It seems odd that the two sizes would differ, though. 
 </p> 
 <p> 
 We use some simple and repetitive code to write the registers into the stack. 
 It may be possible to optimize it by pushing all the registers in one shot. 
 </p> */
val x86_pusha : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_pusha (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-PUSHA";
    let operand_size : {|2, 4|} = check_range(2, 4, select_operand_size(proc_mode, false, 0x00, false, prefixes, false, false, false));
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let eax_ax : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x0, 0x00);
    let ecx_cx : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x1, 0x00);
    let edx_dx : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x2, 0x00);
    let ebx_bx : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x3, 0x00);
    let esp_sp : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x4, 0x00);
    let ebp_bp : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x5, 0x00);
    let esi_si : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x6, 0x00);
    let edi_di : bits(64) = rgfi_size(bits_of_int(operand_size, 4), 0x7, 0x00);
    let check_alignment? : bool = alignment_checking_enabled_p();
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(eax_ax, 128), check_alignment?, false);
    let check_alignment? : bool = false;
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(ecx_cx, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(edx_dx, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(ebx_bx, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(esp_sp, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(ebp_bp, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(esi_si, 128), check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(negate(operand_size), 64));
    wme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, sail_zero_extend(edi_di, 128), check_alignment?, false);
    write_sptr(proc_mode, rsp_var);
    write_iptr(proc_mode, temp_rip)
}

/*! POPA/POPD: 61
<p> 
 In 64-bit mode, this instruction is invalid; it throws a #UD exception. 
 </p> 
 <p> 
 We use some simple and repetitive code to read the registers from the stack. 
 It may be possible to optimize it by popping all the registers in one shot. 
 </p> */
val x86_popa : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_popa (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-POPA";
    let operand_size : {|2, 4|} = check_range(2, 4, select_operand_size(proc_mode, false, 0x00, false, prefixes, false, false, false));
    let rsp_var : sbits(64) = read_sptr(proc_mode);
    let check_alignment? : bool = alignment_checking_enabled_p();
    let edi_di : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let check_alignment? : bool = false;
    let esi_si : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let ebp_bp : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let ebx_bx : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let edx_dx : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let ecx_cx : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    let eax_ax : bits(128) = rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, rsp_var, None(), 2, ":R", check_alignment?, false);
    let rsp_var : sbits(64) = add_to_sptr(proc_mode, rsp_var, bits_of_int(operand_size, 64));
    write_rgfi_size(bits_of_int(operand_size, 4), 0x7, truncate(edi_di, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x6, truncate(esi_si, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x5, truncate(ebp_bp, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x3, truncate(ebx_bx, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x2, truncate(edx_dx, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x1, truncate(ecx_cx, 64), 0x00);
    write_rgfi_size(bits_of_int(operand_size, 4), 0x0, truncate(eax_ax, 64), 0x00);
    write_sptr(proc_mode, rsp_var);
    write_iptr(proc_mode, temp_rip)
}
