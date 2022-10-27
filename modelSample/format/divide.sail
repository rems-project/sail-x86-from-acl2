$include "./divide_spec.sail"

val x86_div : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

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
    let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    if reg_mem == 0x00000000000000000000000000000000 then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT--ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let rax_var : bits(64) = rgfi_size(bits_of_int(if select_byte_operand then 2 else {
        reg_mem_size
    }, 4), 0x0, rex_byte);
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
    if overflow? then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"));
    (match reg_mem_size {
      1 => {
          let result : bits(128) = changeSlice(quotient, 8, 8, truncate(remainder, 8));
          write_rgfi_size(0x2, 0x0, unsigned(result), rex_byte)
      },
      _ => {
          {
              write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(quotient), rex_byte);
              write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(remainder), rex_byte)
          }
      }
    }) : unit;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with cf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with pf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with af = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with zf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with sf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with of = sailval] in rflags = new_rflags.bits;
    write_iptr(proc_mode, temp_rip)
}

val x86_idiv : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

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
    let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg0) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"));
    if reg_mem == 0x00000000000000000000000000000000 then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let rax_var : bits(64) = rgfi_size(bits_of_int(if select_byte_operand then 2 else {
        reg_mem_size
    }, 4), 0x0, rex_byte);
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
    if overflow? then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"));
    (match reg_mem_size {
      1 => {
          let result : bits(64) = changeSlice(quotient, 8, 8, truncate(remainder, 8));
          write_rgfi_size(0x2, 0x0, unsigned(result), rex_byte)
      },
      _ => {
          {
              write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(quotient), rex_byte);
              write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(remainder), rex_byte)
          }
      }
    }) : unit;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with cf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with pf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with af = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with zf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with sf = sailval] in rflags = new_rflags.bits;
    let rflags_var : bits(32) = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [Mk_rflagsbits(rflags_var) with of = sailval] in rflags = new_rflags.bits;
    write_iptr(proc_mode, truncate(temp_rip, 48))
}
