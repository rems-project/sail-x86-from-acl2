
/*! <h3>Op/En = M: [OP m16@ ( ' & ' ) m32]</h3> 
 <h3>Op/En = M: [OP m16@ ( ' & ' ) m64]</h3> 
 
 <p>In 64-bit mode, the instruction ' s operand size is fixed at 8+2 
 bytes ( an 8-byte base and a 2-byte limit ) .</p> 
 
 <p>[OP M]<br/> 
 0F 01/2: LGDT m16@ ( ' & ' ) 32<br/> 
 0F 01/2: LGDT m16@ ( ' & ' ) 64</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p> */
val x86_lgdt : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lgdt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LGDT";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    if app_view then x86_model_error(":LGDT-UNIMPLEMENTED-IN-APP-VIEW");
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let base_size : {|4, 8|} = {
        if n64_bit_modep() then 8 else 4
    };
    let base_size_plus_2 : {|6, 10|} = 2 + base_size;
    let inst_ac? : bool = false;
    let (mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, base_size_plus_2, inst_ac?, true, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p3? : bool = 0x66 == prefixes[opr];
    let operand_size : {|2, 4, 8|} = {
        if base_size == 8 then 8 else {
            let cs_attr : bits(16) = seg_hidden_attrs[1];
            let cs_d : bits(1) = Mk_code_segment_descriptor_attributesbits(cs_attr)[d];
            if cs_d == 0b1 then {
                if p3? then 2 else 4
            } else if p3? then 4 else 2
        }
    };
    let base_bits : {|24, 32, 64|} = {
        match operand_size {
          8 => 64,
          4 => 32,
          _ => 24
        }
    };
    let gdtr_limit : gdtr_idtrbits = [Mk_gdtr_idtrbits(0x00000000000000000000) with limit = mem[15 .. 0]];
    let gdtr : gdtr_idtrbits = [gdtr_limit with base_addr = sail_mask(64, bitslice(mem, 16, base_bits))];
    strs[0] = gdtr.bits;
    write_iptr(proc_mode, temp_rip)
}

/*! <h3>Op/En = M: [OP m16@ ( ' & ' ) m32]</h3> 
 <h3>Op/En = M: [OP m16@ ( ' & ' ) m64]</h3> 
 
 <p>In 64-bit mode, the instruction ' s operand size is fixed at 8+2 
 bytes ( an 8-byte base and a 2-byte limit ) .</p> 
 
 <p>[OP M]<br/> 
 0F 01/3: LIDT m16@ ( ' & ' ) 32<br/> 
 0F 01/3: LIDT m16@ ( ' & ' ) 64</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p> */
val x86_lidt : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lidt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LIDT";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    if app_view then x86_model_error(":LIDT-UNIMPLEMENTED");
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let base_size : {|4, 8|} = {
        if n64_bit_modep() then 8 else 4
    };
    let base_size_plus_2 : {|6, 10|} = 2 + base_size;
    let inst_ac? : bool = false;
    let (mem, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, base_size_plus_2, inst_ac?, true, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let p3? : bool = 0x66 == prefixes[opr];
    let operand_size : {|2, 4, 8|} = {
        if base_size == 8 then 8 else {
            let cs_attr : bits(16) = seg_hidden_attrs[1];
            let cs_d : bits(1) = Mk_code_segment_descriptor_attributesbits(cs_attr)[d];
            if cs_d == 0b1 then {
                if p3? then 2 else 4
            } else if p3? then 4 else 2
        }
    };
    let base_bits : {|24, 32, 64|} = {
        match operand_size {
          8 => 64,
          4 => 32,
          _ => 24
        }
    };
    let idtr_limit : gdtr_idtrbits = [Mk_gdtr_idtrbits(0x00000000000000000000) with limit = mem[15 .. 0]];
    let idtr : gdtr_idtrbits = [idtr_limit with base_addr = sail_mask(64, bitslice(mem, 16, base_bits))];
    strs[1] = idtr.bits;
    write_iptr(proc_mode, temp_rip)
}

/*! <h3>Op/En = M: [OP r/m16]</h3> 
 [OP M]<br/> 
 0F 00/2: LLDT r/m16<br/> 
 
 <p>If bits 2-15 of the source operand are 0, LDTR is marked invalid 
 and the LLDT instruction completes silently. However, all subsequent 
 references to descriptors in the LDT ( except by the LAR, VERR, VERW or 
 LSL instructions ) cause a general protection exception.</p> 
 
 <p>The operand-size attribute has no effect on this instruction. In 
 64-bit mode, the operand size is fixed at 16 bits.</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p> */
val x86_lldt : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_lldt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LLDT";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    if app_view then x86_model_error(":LLDT-UNIMPLEMENTED");
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = false;
    let (selector, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, 2, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let sel_index : bits(13) = Mk_segment_selectorbits(truncate(selector, 16))[index];
    let sel_ti : bits(1) = Mk_segment_selectorbits(truncate(selector, 16))[ti];
    let sel_rpl : bits(2) = Mk_segment_selectorbits(truncate(selector, 16))[rpl];
    if sel_ti == 0b1 then x86_model_error(":GP-SELECTOR-DOES-NOT-POINT-TO-GDT");
    let gdtr : bits(80) = strs[0];
    let gdtr_base : bits(64) = {
        if in_64bit_mode(proc_mode) then {
            Mk_gdtr_idtrbits(gdtr)[base_addr]
        } else {
            sail_zero_extend(truncate(Mk_gdtr_idtrbits(gdtr)[base_addr], 32), 64)
        }
    };
    let gdtr_limit : bits(16) = Mk_gdtr_idtrbits(gdtr)[limit];
    let largest_address : int = ash(unsigned(sel_index), 3) + (if in_64bit_mode(proc_mode) then 15 else 7);
    if unsigned(gdtr_limit) < largest_address then x86_model_error(":GP-SELECTOR-LIMIT-CHECK-FAILED");
    if sel_index == 0b0000000000000 then {
        ssr_visibles[0] = truncate(selector, 16)
    } else {
        let descriptor_addr : int = unsigned(gdtr_base) + unsigned(bits_of_int(ash(unsigned(sel_index), 3), 16));
        if not_bool(canonical_address_p(descriptor_addr)) then x86_model_error(":DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR");
        let descriptor_size : {|8, 16|} = {
            if in_64bit_mode(proc_mode) then 16 else 8
        };
        let descriptor : bits(128) = rml_size(descriptor_size, bits_of_int(descriptor_addr, 64), ":X");
        let (descriptor_valid?, reason) : (bool, (string, bits(128))) = ia32e_valid_ldt_segment_descriptor_p(descriptor);
        if not_bool(descriptor_valid?) then x86_model_error(":INVALID-SEGMENT-DESCRIPTOR");
        let ldtr_base15_0 : bits(16) = Mk_system_segment_descriptorbits(descriptor)[base15_0];
        let ldtr_base23_16 : bits(8) = Mk_system_segment_descriptorbits(descriptor)[base23_16];
        let ldtr_base31_24 : bits(8) = Mk_system_segment_descriptorbits(descriptor)[base31_24];
        let ldtr_base63_32 : bits(32) = Mk_system_segment_descriptorbits(descriptor)[base63_32];
        let ldtr_base63_24 : bits(40) = bits_of_int(changeSlice(ash(unsigned(ldtr_base63_32), 8), 0, 8, unsigned(ldtr_base31_24)), 40);
        let ldtr_base23_0 : bits(24) = bits_of_int(changeSlice(ash(unsigned(ldtr_base23_16), 16), 0, 16, unsigned(ldtr_base15_0)), 24);
        let ldtr_base : bits(64) = bits_of_int(changeSlice(ash(unsigned(ldtr_base63_24), 24), 0, 24, unsigned(ldtr_base23_0)), 64);
        let ldtr_limit15_0 : bits(16) = Mk_system_segment_descriptorbits(descriptor)[limit15_0];
        let ldtr_limit19_16 : bits(4) = Mk_system_segment_descriptorbits(descriptor)[limit19_16];
        let ldtr_limit : bits(32) = bits_of_int(changeSlice(ash(unsigned(ldtr_limit19_16), 16), 0, 16, unsigned(ldtr_limit15_0)), 32);
        let ldtr_attr : system_segment_descriptor_attributesbits = make_system_segment_attr_field(descriptor);
        ssr_visibles[0] = truncate(selector, 16);
        ssr_hidden_bases[0] = ldtr_base;
        ssr_hidden_limits[0] = ldtr_limit;
        ssr_hidden_attrs[0] = ldtr_attr.bits;
        write_iptr(proc_mode, temp_rip)
    }
}
