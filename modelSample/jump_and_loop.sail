$ifndef __X86_JUMP_AND_LOOP
$define __X86_JUMP_AND_LOOP
$include <prelude.sail>
$include <string.sail>

val x86_near_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape}
function x86_near_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-NEAR-JMP-OP/EN-D") in
let byte_operand? : bool = ((opcode) == (0xeb)) in
let offset_size : {|1, 2, 4|} = (check_range(0, 4, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, true, true))) in
let (flg, offset) : (option(string), sbits(64)) = (rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
let offset : sbits(32) = (truncate(offset, 32)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else ();
let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(next_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, truncate(next_rip, 48), sail_sign_extend(offset, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :VIRTUAL-MEMORY-ERROR"))
else ();
let temp_rip : sbits(49) = { (if (offset_size) == (2)
then logand(0b0_0000_0000_0000_0000_0000_0000_0000_0000_1111_1111_1111_1111, temp_rip)
else temp_rip) } in
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_near_jmp_op_en_m : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_near_jmp_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-NEAR-JMP-OP/EN-M") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let operand_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg, jmp_addr, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let jmp_addr : sbits(130) = { (if (proc_mode) == (0)
then sail_sign_extend(truncate(jmp_addr, 64), 130)
else sail_zero_extend(jmp_addr, 130)) } in
{ if not_bool(if (proc_mode) == (0)
then canonical_address_p(signed(jmp_addr))
else let cs_limit : bits(32) = (seg_hidden_limits[1]) in
((0) <= (signed(jmp_addr))) & ((signed(jmp_addr)) <= (unsigned(cs_limit))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :BAD-RETURN-ADDRESS JMP-ADDR)"))
else ();
write_iptr(proc_mode, truncate(jmp_addr, 48))
}
}
}
}

/*Absolute Indirect Jump: Far*/
/*<p>Op/En: D</p> 
 <p><tt>FF/5: JMP m16:16 or m16:32 or m16:64</tt></p> 
 
 <p>Source: Intel Manuals ( Vol. 2A ) Instruction Set Reference: the 
 text below has been edited to contain information only about the 
 64-bit mode.</p> 
 
 <p><i>The JMP instruction cannot be used to perform 
 inter-privilege-level far jumps.</i> The processor always uses the 
 segment selector part of the far address to access the corresponding 
 descriptor in the GDT or LDT. The descriptor type and access rights 
 determine the type of jump to be performed.</p> 
 
 <p><b>Far Jump to a Conforming or Non-Conforming Code Segment:</b> If 
 the selected descriptor is for a code segment, a far jump to a code 
 segment at the same privilege level is performed. If the selected code 
 segment is at a different privilege level and the code segment is 
 non-conforming, a general-protection exception is generated. The 
 target operand specifies an absolute far address indirectly with a 
 memory location ( m16:16 or m16:32 or m16:64 ) . The operand-size 
 attribute and the REX.w bit determine the size of the offset ( 16 or 
 32 or 64 bits ) in the far address. The new code segment selector and 
 its descriptor are loaded into CS register, and the offset from the 
 instruction is loaded into the RIP register.</p> 
 
 <p><b>Far Jump through a Call Gate:</b> When executing a far jump 
 through a call gate, the segment selector specified by the target 
 operand identifies the call gate. The offset part of the target 
 operand is ignored. The processor then jumps to the code segment 
 specified in the call gate descriptor and begins executing the 
 instruction at the offset specified in the call gate. No stack switch 
 occurs. The target operand specifies the far address of the call gate 
 indirectly with a memory location ( m16:16 or m16:32 or m16:64 ) .</p>*/
val x86_far_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_far_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-FAR-JMP-OP/EN-D") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
{ if (mod_var) == (0b11)
then throw(Emsg("Model state error: :SOURCE-OPERAND-NOT-MEMORY-LOCATION"))
else ();
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let offset_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg, mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, (2) + (offset_size), inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, increment_rip_by)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let selector : bits(16) = (truncate(mem, 16)) in
let offset : bits(64) = (sail_mask(64, bitslice(mem, 16, ash(offset_size, 3)))) in
let sel_index : bits(13) = ((Mk_segment_selectorbits(selector))[index]) in
let sel_ti : bits(1) = ((Mk_segment_selectorbits(selector))[ti]) in
let sel_rpl : bits(2) = ((Mk_segment_selectorbits(selector))[rpl]) in
{ if ((sel_ti) == (0b0)) & ((sel_index) == (0b0_0000_0000_0000))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :GP-NULLSELECTOR 0)"))
else ();
let (dt_base_addr, dt_limit) : (bits(64), bits(32)) = { (if (sel_ti) == (0b0)
then let (elem0, elem1) : (bits(64), bits(16)) = { (let gdtr : bits(80) = (strs[0]) in
let gdtr_base : bits(64) = { (if (proc_mode) == (0)
then (Mk_gdtr_idtrbits(gdtr))[base_addr]
else sail_zero_extend(truncate((Mk_gdtr_idtrbits(gdtr))[base_addr], 32), 64)) } in
let gdtr_limit : bits(16) = ((Mk_gdtr_idtrbits(gdtr))[limit]) in
(gdtr_base, gdtr_limit)) } in
(elem0, sail_zero_extend(elem1, 32))
else let ldtr_base : bits(64) = (ssr_hidden_bases[0]) in
let ldtr_base : bits(64) = { (if (proc_mode) == (0)
then ldtr_base
else sail_zero_extend(truncate(ldtr_base, 32), 64)) } in
let ldtr_limit : bits(32) = (ssr_hidden_limits[0]) in
(ldtr_base, ldtr_limit)) } in
let largest_address : int = ((ash(unsigned(sel_index), 3)) + (7)) in
{ if (unsigned(dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST SELECTOR DT-BASE-ADDR DT-LIMIT))"))
else ();
let descriptor_addr : int = ((unsigned(dt_base_addr)) + (unsigned(bits_of_int(ash(unsigned(sel_index), 3), 16)))) in
{ if not_bool(canonical_address_p(descriptor_addr))
then throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else ();
let (flg, descriptor) : (option(string), bits(128)) = (rml_size(8, bits_of_int(descriptor_addr, 48), ":X")) in
let descriptor : bits(64) = (truncate(descriptor, 64)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else ();
let s : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[s]) in
if (s) == (0b1)
then let msb_of_type : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[msb_of_type]) in
{ if not_bool((msb_of_type) == (0b1))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :JMP-FAR-DATA-SEGMENT SEL-INDEX)"))
else ();
let d : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[d]) in
{ if ((proc_mode) == (0)) & ((d) == (0b1))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :CS.D=1-IN-64-BIT-MODE SEL-INDEX)"))
else ();
let cpl : bits(2) = { ({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
}) } in
let dpl : bits(2) = ((Mk_code_segment_descriptorbits(descriptor))[dpl]) in
let c : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[c]) in
let allowed : bool = { (if (c) == (0b1)
then (unsigned(dpl)) <= (unsigned(cpl))
else ((unsigned(sel_rpl)) <= (unsigned(cpl))) & ((cpl) == (dpl))) } in
{ if not_bool(allowed)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :DPL DPL (ACONS :CPL CPL (ACONS :RPL SEL-RPL NIL))))"))
else ();
let p : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[p]) in
{ if not_bool((p) == (0b1))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :NP SEL-INDEX :CODE-SEGMENT-NOT-PRESENT SEL-INDEX)"))
else ();
let jmp_addr : sbits(64) = { (match offset_size {
2 => sail_zero_extend(truncate(offset, 16), 64),
4 => sail_zero_extend(truncate(offset, 32), 64),
_ => offset
}) } in
let jmp_addr_ok : bool = { (if (proc_mode) == (0)
then canonical_address_p(signed(jmp_addr))
else let limit15_0 : bits(16) = ((Mk_code_segment_descriptorbits(descriptor))[limit15_0]) in
let limit19_16 : bits(4) = ((Mk_code_segment_descriptorbits(descriptor))[limit19_16]) in
let limit : int = (changeSlice(ash(unsigned(limit19_16), 16), 0, 16, unsigned(limit15_0))) in
let g : bits(1) = ((Mk_code_segment_descriptorbits(descriptor))[g]) in
let max_offset : int : int = { (if (g) == (0b1)
then (ash(limit, 12)) - (1)
else limit) } in
(signed(jmp_addr)) < (max_offset : int)) } in
{ if not_bool(jmp_addr_ok)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :NONCANONICAL-OR-OUTSIDE-SEGMENT-LIMIT JMP-ADDR)"))
else ();
let new_cs_visible : segment_selectorbits = ([(Mk_segment_selectorbits(selector)) with rpl = (cpl)]) in
{ seg_visibles[1] = (new_cs_visible).bits;
seg_hidden_bases[1] = dt_base_addr;
seg_hidden_limits[1] = dt_limit;
seg_hidden_attrs[1] = (make_code_segment_attr_field(descriptor)).bits;
write_iptr(proc_mode, truncate(jmp_addr, 48))
}
}
}
}
}
}
else { if not_bool((proc_mode) == (0))
then throw(Emsg("Model state error: :FAR-JMP-SYSTEM-UNIMPLEMENTED-IN-32-BIT-MODE"))
else ();
let largest_address : int = ((ash(unsigned(sel_index), 3)) + (15)) in
{ if (unsigned(dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST SELECTOR DT-BASE-ADDR DT-LIMIT))"))
else ();
let (flg, descriptor) : (option(string), bits(128)) = (rml_size(16, bits_of_int(descriptor_addr, 48), ":X")) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else ();
let (call_gate_desc?, reason2) : (bool, (string, (bits(128)))) = (ia32e_valid_call_gate_segment_descriptor_p(descriptor)) in
if call_gate_desc?
then let cpl : bits(2) = { ({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
}) } in
let dpl : bits(2) = ((Mk_call_gate_descriptorbits(descriptor))[dpl]) in
{ if not_bool(((unsigned(cpl)) <= (unsigned(dpl))) & ((unsigned(sel_rpl)) <= (unsigned(dpl))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :DPL DPL (ACONS :CPL CPL (ACONS :RPL SEL-RPL NIL))))"))
else ();
let cs_selector : bits(16) = ((Mk_call_gate_descriptorbits(descriptor))[selector]) in
let cs_sel_index : bits(13) = ((Mk_segment_selectorbits(cs_selector))[index]) in
let cs_sel_ti : bits(1) = ((Mk_segment_selectorbits(cs_selector))[ti]) in
let cs_sel_rpl : bits(2) = ((Mk_segment_selectorbits(cs_selector))[rpl]) in
{ if ((cs_sel_ti) == (0b0)) & ((cs_sel_index) == (0b0_0000_0000_0000))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :CALL-GATE-CODE-SEGMENT-NULLSELECTOR 0)"))
else ();
let (cs_dt_base_addr, cs_dt_limit) : (bits(64), bits(32)) = { (if (sel_ti) == (0b0)
then let (elem0, elem1) : (bits(64), bits(16)) = { (let gdtr : bits(80) = (strs[0]) in
let gdtr_base : bits(64) = ((Mk_gdtr_idtrbits(gdtr))[base_addr]) in
let gdtr_base : bits(64) = { (if (proc_mode) == (0)
then gdtr_base
else sail_zero_extend(truncate(gdtr_base, 32), 64)) } in
let gdtr_limit : bits(16) = ((Mk_gdtr_idtrbits(gdtr))[limit]) in
(gdtr_base, gdtr_limit)) } in
(elem0, sail_zero_extend(elem1, 32))
else let ldtr_base : bits(64) = (ssr_hidden_bases[0]) in
let ldtr_base : bits(64) = { (if (proc_mode) == (0)
then ldtr_base
else sail_zero_extend(truncate(ldtr_base, 32), 64)) } in
let ldtr_limit : bits(32) = (ssr_hidden_limits[0]) in
(ldtr_base, ldtr_limit)) } in
let largest_address : int = ((ash(unsigned(cs_sel_index), 3)) + (7)) in
{ if (unsigned(cs_dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :GP-SELECTOR-LIMIT-CHECK-FAILED (LIST CS-SELECTOR CS-DT-BASE-ADDR CS-DT-LIMIT))"))
else ();
let cs_descriptor_addr : int = ((unsigned(cs_dt_base_addr)) + (unsigned(bits_of_int(ash(unsigned(cs_sel_index), 3), 16)))) in
{ if not_bool(canonical_address_p(cs_descriptor_addr))
then throw(Emsg("Model state error: :CS-DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else ();
let (flg, cs_descriptor) : (option(string), bits(128)) = (rml_size(8, bits_of_int(cs_descriptor_addr, 48), ":X")) in
let cs_descriptor : bits(64) = (truncate(cs_descriptor, 64)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else ();
let (valid?, reason) : (bool, (string, (bits(64)))) = (ia32e_valid_code_segment_descriptor_p(cs_descriptor)) in
{ if not_bool(valid?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :CALL-GATE-CODE-SEGMENT-DESCRIPTOR-INVALID (CONS REASON CS-DESCRIPTOR))"))
else ();
let cs_dpl : bits(2) = ((Mk_code_segment_descriptorbits(cs_descriptor))[dpl]) in
let c_bit : bits(1) = ((Mk_code_segment_descriptorbits(cs_descriptor))[c]) in
{ if (((c_bit) == (0b1)) & (not_bool((unsigned(cs_dpl)) <= (unsigned(cpl))))) | (((c_bit) == (0b0)) & (not_bool((cs_dpl) == (cpl))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP CS-SEL-INDEX :PRIVILEGE-CHECK-FAIL (ACONS :C-BIT C-BIT (ACONS :CPL CPL (ACONS :CS-DPL CS-DPL NIL))))"))
else ();
let call_gate_offset15_0 : bits(16) = ((Mk_call_gate_descriptorbits(descriptor))[offset15_0]) in
let call_gate_offset31_16 : bits(16) = ((Mk_call_gate_descriptorbits(descriptor))[offset31_16]) in
let call_gate_offset63_32 : bits(32) = ((Mk_call_gate_descriptorbits(descriptor))[offset63_32]) in
let call_gate_offset31_0 : int = (changeSlice(ash(unsigned(call_gate_offset31_16), 16), 0, 16, unsigned(call_gate_offset15_0))) in
let call_gate_offset : int = (changeSlice(ash(unsigned(call_gate_offset63_32), 32), 0, 32, call_gate_offset31_0)) in
let jmp_addr : int = { (match offset_size {
2 => unsigned(bits_of_int(call_gate_offset, 16)),
4 => unsigned(bits_of_int(call_gate_offset, 32)),
_ => call_gate_offset
}) } in
{ if not_bool(canonical_address_p(jmp_addr))
then throw(Emsg("Model state error: :TARGET-OFFSET-VIRTUAL-MEMORY-ERROR"))
else ();
let new_cs_visible : segment_selectorbits = ([(Mk_segment_selectorbits(cs_selector)) with rpl = (cpl)]) in
{ seg_visibles[1] = (new_cs_visible).bits;
seg_hidden_bases[1] = cs_dt_base_addr;
seg_hidden_limits[1] = cs_dt_limit;
seg_hidden_attrs[1] = (make_code_segment_attr_field(cs_descriptor)).bits;
write_iptr(proc_mode, bits_of_int(jmp_addr, 48))
}
}
}
}
}
}
}
}
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP SEL-INDEX :EITHER-BOTH-CODE-SEGMENT-OR-CALL-GATE-ARE-ABSENT-OR-SOME-OTHER-DESCRIPTOR-IS-PRESENT (CONS REASON2 DESCRIPTOR))"))
}
}
}
}
}
}
}
}
}
}

val x86_loop : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape, rreg, wreg}
function x86_loop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LOOP") in
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b001)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let counter_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let counter : bits(64) = (rgfi_size(bits_of_int(counter_size, 4), 0x1, rex_byte)) in
let counter : bits(64) = (sail_mask(64, trunc(counter_size, bits_of_int((unsigned(counter)) - (1), 64)))) in
let zf : bits(1) = (truncate(let rflags_var : bits(32) = (rflags) in
sail_zero_extend((Mk_rflagsbits(rflags_var))[zf], 2), 1)) in
let branch_cond : bool = { (if (opcode) == (0xe2)
then not_bool((counter) == (0x0000_0000_0000_0000))
else if (opcode) == (0xe1)
then ((zf) == (0b1)) & (not_bool((counter) == (0x0000_0000_0000_0000)))
else ((zf) == (0b0)) & (not_bool((counter) == (0x0000_0000_0000_0000)))) } in
if branch_cond
then { let (flg, rel8) : (option(string), sbits(64)) = (rime_size(proc_mode, 1, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else ();
let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int((1) + (signed(rel8)), 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
write_rgfi_size(bits_of_int(counter_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, truncate(next_rip, 48))
}
}
}
else { let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
write_rgfi_size(bits_of_int(counter_size, 4), 0x1, unsigned(counter), rex_byte);
write_iptr(proc_mode, truncate(next_rip, 48))
}
}
}

$endif
