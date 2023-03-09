$include "./divide_spec.sail"

/*! <h4>Op/En: M</h4> 
 
 <p>F6/6:<br/> 
 DIV r/m8: ( AX div r/m8 ) , AH := Remainder, AL := Quotient<br/><br/> 
 F7/6:<br/> 
 DIV r/m16: ( DX:AX div r/m16 ) , DX := Remainder, AX := Quotient<br/> 
 DIV r/m32: ( EDX:EAX div r/m8 ) , EDX := Remainder, EAX := Quotient<br/> 
 DIV r/m64: ( RDX:RAX div r/m8 ) , RDX := Remainder, RAX := Quotient<br/></p> */
val x86_div : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_div (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-DIV";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = opcode == 0xf6;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if reg_mem == 0x00000000000000000000000000000000 then x86_fault("(!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)");
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let rax_var : bits(64) = rgfi_size(if select_byte_operand then 0x2 else {
        bits_of_int(reg_mem_size, 4)
    }, 0x0, rex_byte);
    let rdx_var : bits(64) = {
        if select_byte_operand then 0x0000000000000000 else {
            rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, rex_byte)
        }
    };
    let dividend : int = {
        if select_byte_operand then {
            unsigned(rax_var)
        } else {
            changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))
        }
    };
    let (overflow?, quotient, remainder) : (bool, bits(128), bits(64)) = div_spec(reg_mem_size, bits_of_int(dividend, 128), truncate(reg_mem, 64));
    if overflow? then x86_model_error(":UNSIGNED-DIVIDE-ERROR-OVERFLOW");
    (match reg_mem_size {
      1 => {
          let result : bits(128) = changeSlice(quotient, 8, 8, truncate(remainder, 8));
          write_rgfi_size(0x2, 0x0, truncate(result, 64), rex_byte)
      },
      _ => {
          write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, truncate(quotient, 64), rex_byte);
          write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, remainder, rex_byte)
      }
    }) : unit;
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

/*! <h4>Op/En: M</h4> 
 
 <p>F6/7:<br/> 
 IDIV r/m8: ( AX div r/m8 ) , AH := Remainder, AL := Quotient<br/><br/> 
 
 F7/7:<br/> 
 IDIV r/m16: ( DX:AX div r/m16 ) , DX := Remainder, AX := Quotient <br/> 
 IDIV r/m32: ( EDX:EAX div r/m8 ) , EDX := Remainder, EAX := Quotient <br/> 
 IDIV r/m64: ( RDX:RAX div r/m8 ) , RDX := Remainder, RAX := Quotient</p> */
val x86_idiv : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_idiv (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-IDIV";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let select_byte_operand : bool = opcode == 0xf6;
    let reg_mem_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false);
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (reg_mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if reg_mem == 0x00000000000000000000000000000000 then x86_fault("(!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)");
    let temp_rip : sbits(49) = sail_sign_extend(add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48)), 49);
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let rax_var : bits(64) = rgfi_size(if select_byte_operand then 0x2 else {
        bits_of_int(reg_mem_size, 4)
    }, 0x0, rex_byte);
    let rdx_var : bits(64) = {
        if select_byte_operand then 0x0000000000000000 else {
            rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, rex_byte)
        }
    };
    let dividend : int = {
        if select_byte_operand then {
            unsigned(rax_var)
        } else {
            changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))
        }
    };
    let (overflow?, quotient, remainder) : (bool, bits(64), bits(64)) = idiv_spec(reg_mem_size, bits_of_int(dividend, 128), truncate(reg_mem, 64));
    if overflow? then x86_model_error(":UNSIGNED-DIVIDE-ERROR-OVERFLOW");
    (match reg_mem_size {
      1 => {
          let result : bits(64) = changeSlice(quotient, 8, 8, truncate(remainder, 8));
          write_rgfi_size(0x2, 0x0, result, rex_byte)
      },
      _ => {
          write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, quotient, rex_byte);
          write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, remainder, rex_byte)
      }
    }) : unit;
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
    write_iptr(proc_mode, truncate(temp_rip, 48))
}
