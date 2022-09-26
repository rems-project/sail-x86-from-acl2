
val x86_near_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_near_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-NEAR-JMP-OP/EN-D";
    let byte_operand? : bool = opcode == 0xeb;
    let offset_size : {|1, 2, 4|} = check_range(0, 4, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, true, true));
    let (flg, offset) : (option(string), sbits(64)) = rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
    let offset : sbits(32) = truncate(offset, 32);
    if is_some(flg) then throw(Emsg("Model state error: :RIME-SIZE-ERROR"));
    let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48));
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, truncate(next_rip, 48), 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(next_rip, 48), sail_sign_extend(offset, 48));
    if is_some(flg) then throw(Emsg("Model state error: :VIRTUAL-MEMORY-ERROR"));
    let temp_rip : sbits(49) = {
        if offset_size == 2 then {
            logand(0b0000000000000000000000000000000001111111111111111, temp_rip)
        } else {
            temp_rip
        }
    };
    write_iptr(proc_mode, truncate(temp_rip, 48))
}

val x86_near_jmp_op_en_m : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_near_jmp_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-NEAR-JMP-OP/EN-M";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let operand_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true));
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg, jmp_addr, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48));
    let temp_rip : sbits(48) = truncate(temp_rip, 48);
    if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let jmp_addr : sbits(130) = {
        if proc_mode == 0 then {
            sail_sign_extend(truncate(jmp_addr, 64), 130)
        } else {
            sail_zero_extend(jmp_addr, 130)
        }
    };
    if not_bool(if proc_mode == 0 then {
        canonical_address_p(signed(jmp_addr))
    } else {
        let cs_limit : bits(32) = seg_hidden_limits[1];
        0 <= signed(jmp_addr) & signed(jmp_addr) <= unsigned(cs_limit)
    }) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :BAD-RETURN-ADDRESS JMP-ADDR)"));
    write_iptr(proc_mode, truncate(jmp_addr, 48))
}

val x86_far_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_far_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-FAR-JMP-OP/EN-D";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    if mod_var == 0b11 then throw(Emsg("Model state error: :SOURCE-OPERAND-NOT-MEMORY-LOCATION"));
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let offset_size : {|2, 4, 8|} = check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false));
    let seg_reg : {|0, 1, 2, 3, 4, 5|} = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (flg, mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, 2 + offset_size, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    if is_some(flg) then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, increment_rip_by);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let selector : bits(16) = truncate(mem, 16);
    let offset : bits(64) = sail_mask(64, bitslice(mem, 16, ash(offset_size, 3)));
    let sel_index : bits(13) = Mk_segment_selectorbits(selector)[index];
    let sel_ti : bits(1) = Mk_segment_selectorbits(selector)[ti];
    let sel_rpl : bits(2) = Mk_segment_selectorbits(selector)[rpl];
    if sel_ti == 0b0 & sel_index == 0b0000000000000 then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :GP-NULLSELECTOR 0)"));
    let (dt_base_addr, dt_limit) : (bits(64), bits(32)) = {
        if sel_ti == 0b0 then {
            let (elem0, elem1) : (bits(64), bits(16)) = {
                let gdtr : bits(80) = strs[0];
                let gdtr_base : bits(64) = {
                    if proc_mode == 0 then {
                        Mk_gdtr_idtrbits(gdtr)[base_addr]
                    } else {
                        sail_zero_extend(truncate(Mk_gdtr_idtrbits(gdtr)[base_addr], 32), 64)
                    }
                };
                let gdtr_limit : bits(16) = Mk_gdtr_idtrbits(gdtr)[limit];
                (gdtr_base, gdtr_limit)
            };
            (elem0, sail_zero_extend(elem1, 32))
        } else {
            let ldtr_base : bits(64) = ssr_hidden_bases[0];
            let ldtr_base : bits(64) = {
                if proc_mode == 0 then {
                    ldtr_base
                } else {
                    sail_zero_extend(truncate(ldtr_base, 32), 64)
                }
            };
            let ldtr_limit : bits(32) = ssr_hidden_limits[0];
            (ldtr_base, ldtr_limit)
        }
    };
    let largest_address : int = ash(unsigned(sel_index), 3) + 7;
    if unsigned(dt_limit) < largest_address then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST SELECTOR DT-BASE-ADDR DT-LIMIT))"));
    let descriptor_addr : int = unsigned(dt_base_addr) + unsigned(bits_of_int(ash(unsigned(sel_index), 3), 16));
    if not_bool(canonical_address_p(descriptor_addr)) then throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"));
    let (flg, descriptor) : (option(string), bits(128)) = rml_size(8, bits_of_int(descriptor_addr, 48), ":X");
    let descriptor : bits(64) = truncate(descriptor, 64);
    if is_some(flg) then throw(Emsg("Model state error: :RML-SIZE-ERROR"));
    let s : bits(1) = Mk_code_segment_descriptorbits(descriptor)[s];
    if s == 0b1 then {
        let msb_of_type : bits(1) = Mk_code_segment_descriptorbits(descriptor)[msb_of_type];
        if not_bool(msb_of_type == 0b1) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :JMP-FAR-DATA-SEGMENT SEL-INDEX)"));
        let d : bits(1) = Mk_code_segment_descriptorbits(descriptor)[d];
        if proc_mode == 0 & d == 0b1 then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :CS.D=1-IN-64-BIT-MODE SEL-INDEX)"));
        let cpl : bits(2) = {
            {
                ();
                Mk_segment_selectorbits(seg_visibles[1])[rpl]
            }
        };
        let dpl : bits(2) = Mk_code_segment_descriptorbits(descriptor)[dpl];
        let c : bits(1) = Mk_code_segment_descriptorbits(descriptor)[c];
        let allowed : bool = {
            if c == 0b1 then {
                unsigned(dpl) <= unsigned(cpl)
            } else {
                unsigned(sel_rpl) <= unsigned(cpl) & cpl == dpl
            }
        };
        if not_bool(allowed) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :DPL DPL (ACONS :CPL CPL (ACONS :RPL SEL-RPL NIL))))"));
        let p : bits(1) = Mk_code_segment_descriptorbits(descriptor)[p];
        if not_bool(p == 0b1) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :NP SEL-INDEX :CODE-SEGMENT-NOT-PRESENT SEL-INDEX)"));
        let jmp_addr : sbits(64) = {
            match offset_size {
              2 => sail_zero_extend(truncate(offset, 16), 64),
              4 => sail_zero_extend(truncate(offset, 32), 64),
              _ => offset
            }
        };
        let jmp_addr_ok : bool = {
            if proc_mode == 0 then {
                canonical_address_p(signed(jmp_addr))
            } else {
                let limit15_0 : bits(16) = Mk_code_segment_descriptorbits(descriptor)[limit15_0];
                let limit19_16 : bits(4) = Mk_code_segment_descriptorbits(descriptor)[limit19_16];
                let limit : int = changeSlice(ash(unsigned(limit19_16), 16), 0, 16, unsigned(limit15_0));
                let g : bits(1) = Mk_code_segment_descriptorbits(descriptor)[g];
                let max_offset : int : int = {
                    if g == 0b1 then {
                        ash(limit, 12) - 1
                    } else {
                        limit
                    }
                };
                signed(jmp_addr) < max_offset : int
            }
        };
        if not_bool(jmp_addr_ok) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :NONCANONICAL-OR-OUTSIDE-SEGMENT-LIMIT JMP-ADDR)"));
        let new_cs_visible : segment_selectorbits = [Mk_segment_selectorbits(selector) with rpl = cpl];
        seg_visibles[1] = new_cs_visible.bits;
        seg_hidden_bases[1] = dt_base_addr;
        seg_hidden_limits[1] = dt_limit;
        seg_hidden_attrs[1] = make_code_segment_attr_field(descriptor).bits;
        write_iptr(proc_mode, truncate(jmp_addr, 48))
    } else {
        if not_bool(proc_mode == 0) then throw(Emsg("Model state error: :FAR-JMP-SYSTEM-UNIMPLEMENTED-IN-32-BIT-MODE"));
        let largest_address : int = ash(unsigned(sel_index), 3) + 15;
        if unsigned(dt_limit) < largest_address then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST SELECTOR DT-BASE-ADDR DT-LIMIT))"));
        let (flg, descriptor) : (option(string), bits(128)) = rml_size(16, bits_of_int(descriptor_addr, 48), ":X");
        if is_some(flg) then throw(Emsg("Model state error: :RML-SIZE-ERROR"));
        let (call_gate_desc?, reason2) : (bool, (string, bits(128))) = ia32e_valid_call_gate_segment_descriptor_p(descriptor);
        if call_gate_desc? then {
            let cpl : bits(2) = {
                {
                    ();
                    Mk_segment_selectorbits(seg_visibles[1])[rpl]
                }
            };
            let dpl : bits(2) = Mk_call_gate_descriptorbits(descriptor)[dpl];
            if not_bool(unsigned(cpl) <= unsigned(dpl) & unsigned(sel_rpl) <= unsigned(dpl)) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :DPL DPL (ACONS :CPL CPL (ACONS :RPL SEL-RPL NIL))))"));
            let cs_selector : bits(16) = Mk_call_gate_descriptorbits(descriptor)[selector];
            let cs_sel_index : bits(13) = Mk_segment_selectorbits(cs_selector)[index];
            let cs_sel_ti : bits(1) = Mk_segment_selectorbits(cs_selector)[ti];
            let cs_sel_rpl : bits(2) = Mk_segment_selectorbits(cs_selector)[rpl];
            if cs_sel_ti == 0b0 & cs_sel_index == 0b0000000000000 then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :CALL-GATE-CODE-SEGMENT-NULLSELECTOR 0)"));
            let (cs_dt_base_addr, cs_dt_limit) : (bits(64), bits(32)) = {
                if sel_ti == 0b0 then {
                    let (elem0, elem1) : (bits(64), bits(16)) = {
                        let gdtr : bits(80) = strs[0];
                        let gdtr_base : bits(64) = Mk_gdtr_idtrbits(gdtr)[base_addr];
                        let gdtr_base : bits(64) = {
                            if proc_mode == 0 then {
                                gdtr_base
                            } else {
                                sail_zero_extend(truncate(gdtr_base, 32), 64)
                            }
                        };
                        let gdtr_limit : bits(16) = Mk_gdtr_idtrbits(gdtr)[limit];
                        (gdtr_base, gdtr_limit)
                    };
                    (elem0, sail_zero_extend(elem1, 32))
                } else {
                    let ldtr_base : bits(64) = ssr_hidden_bases[0];
                    let ldtr_base : bits(64) = {
                        if proc_mode == 0 then {
                            ldtr_base
                        } else {
                            sail_zero_extend(truncate(ldtr_base, 32), 64)
                        }
                    };
                    let ldtr_limit : bits(32) = ssr_hidden_limits[0];
                    (ldtr_base, ldtr_limit)
                }
            };
            let largest_address : int = ash(unsigned(cs_sel_index), 3) + 7;
            if unsigned(cs_dt_limit) < largest_address then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST CS-SELECTOR CS-DT-BASE-ADDR CS-DT-LIMIT))"));
            let cs_descriptor_addr : int = unsigned(cs_dt_base_addr) + unsigned(bits_of_int(ash(unsigned(cs_sel_index), 3), 16));
            if not_bool(canonical_address_p(cs_descriptor_addr)) then throw(Emsg("Model state error: :CS-DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"));
            let (flg, cs_descriptor) : (option(string), bits(128)) = rml_size(8, bits_of_int(cs_descriptor_addr, 48), ":X");
            let cs_descriptor : bits(64) = truncate(cs_descriptor, 64);
            if is_some(flg) then throw(Emsg("Model state error: :RML-SIZE-ERROR"));
            let (valid?, reason) : (bool, (string, bits(64))) = ia32e_valid_code_segment_descriptor_p(cs_descriptor);
            if not_bool(valid?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :CALL-GATE-CODE-SEGMENT-DESCRIPTOR-INVALID (CONS REASON CS-DESCRIPTOR))"));
            let cs_dpl : bits(2) = Mk_code_segment_descriptorbits(cs_descriptor)[dpl];
            let c_bit : bits(1) = Mk_code_segment_descriptorbits(cs_descriptor)[c];
            if c_bit == 0b1 & not_bool(unsigned(cs_dpl) <= unsigned(cpl)) | c_bit == 0b0 & not_bool(cs_dpl == cpl) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :C-BIT C-BIT (ACONS :CPL CPL (ACONS :CS-DPL CS-DPL NIL))))"));
            let call_gate_offset15_0 : bits(16) = Mk_call_gate_descriptorbits(descriptor)[offset15_0];
            let call_gate_offset31_16 : bits(16) = Mk_call_gate_descriptorbits(descriptor)[offset31_16];
            let call_gate_offset63_32 : bits(32) = Mk_call_gate_descriptorbits(descriptor)[offset63_32];
            let call_gate_offset31_0 : int = changeSlice(ash(unsigned(call_gate_offset31_16), 16), 0, 16, unsigned(call_gate_offset15_0));
            let call_gate_offset : int = changeSlice(ash(unsigned(call_gate_offset63_32), 32), 0, 32, call_gate_offset31_0);
            let jmp_addr : int = {
                match offset_size {
                  2 => unsigned(bits_of_int(call_gate_offset, 16)),
                  4 => unsigned(bits_of_int(call_gate_offset, 32)),
                  _ => call_gate_offset
                }
            };
            if not_bool(canonical_address_p(jmp_addr)) then throw(Emsg("Model state error: :TARGET-OFFSET-VIRTUAL-MEMORY-ERROR"));
            let new_cs_visible : segment_selectorbits = [Mk_segment_selectorbits(cs_selector) with rpl = cpl];
            seg_visibles[1] = new_cs_visible.bits;
            seg_hidden_bases[1] = cs_dt_base_addr;
            seg_hidden_limits[1] = cs_dt_limit;
            seg_hidden_attrs[1] = make_code_segment_attr_field(cs_descriptor).bits;
            write_iptr(proc_mode, bits_of_int(jmp_addr, 48))
        } else {
            throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :EITHER-BOTH-CODE-SEGMENT-OR-CALL-GATE-ARE-ABSENT-OR-SOME-OTHER-DESCRIPTOR-IS-PRESENT (CONS REASON2 DESCRIPTOR))"))
        }
    }
}

val x86_loop : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_loop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-LOOP";
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b001);
    if is_some(badlength?) then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"));
    let p4? : bool = 0x67 == prefixes[adr];
    let counter_size : {|2, 4, 8|} = select_address_size(proc_mode, p4?);
    let counter : bits(64) = rgfi_size(bits_of_int(counter_size, 4), 0x1, rex_byte);
    let counter : bits(64) = sail_mask(64, trunc(counter_size, bits_of_int(unsigned(counter) - 1, 64)));
    let zf : bits(1) = truncate(let rflags_var : bits(32) = rflags in sail_zero_extend(Mk_rflagsbits(rflags_var)[zf], 2), 1);
    let branch_cond : bool = {
        if opcode == 0xe2 then {
            not_bool(counter == 0x0000000000000000)
        } else if opcode == 0xe1 then {
            zf == 0b1 & not_bool(counter == 0x0000000000000000)
        } else {
            zf == 0b0 & not_bool(counter == 0x0000000000000000)
        }
    };
    if branch_cond then {
        let (flg, rel8) : (option(string), sbits(64)) = rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct { mem_ptr? = false });
        if is_some(flg) then throw(Emsg("Model state error: :RIME-SIZE-ERROR"));
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, bits_of_int(1 + signed(rel8), 48));
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_rgfi_size(bits_of_int(counter_size, 4), 0x1, unsigned(counter), rex_byte);
        write_iptr(proc_mode, truncate(next_rip, 48))
    } else {
        let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
        if is_some(flg) then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"));
        write_rgfi_size(bits_of_int(counter_size, 4), 0x1, unsigned(counter), rex_byte);
        write_iptr(proc_mode, truncate(next_rip, 48))
    }
}
