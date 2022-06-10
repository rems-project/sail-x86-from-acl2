$include <prelude.sail>
$include <string.sail>

val x86_near_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_near_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-NEAR-JMP-OP/EN-D") : string in
let byte_operand? = ((opcode) == (get_slice_int(8, 235, 0))) : bool in
let offset_size = (the_range(0, 4, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, true, true))) : {|1, 2, 4|} in
let (flg, offset) = (rime_size(proc_mode, offset_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
let offset = (the_sbits(32, offset)) : sbits(32) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, the_range(0, 4, offset_size), 0))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, next_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, the_sbits(48, next_rip), the_sbits(48, offset))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :VIRTUAL-MEMORY-ERROR"))
else let temp_rip = (if (get_slice_int(8, offset_size, 0)) == (get_slice_int(8, 2, 0))
then logand(get_slice_int(53, 65535, 0), the_sbits(53, temp_rip))
else the_sbits(53, temp_rip)) : sbits(53) in
write_iptr(proc_mode, the_sbits(48, temp_rip))

val x86_near_jmp_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_near_jmp_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-NEAR-JMP-OP/EN-M") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let operand_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, true, true, true))) : {|8, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg, jmp_addr, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let jmp_addr = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then signed(I64(get_slice_int(64, jmp_addr, 0)))
else jmp_addr) : int in
if not_bool(if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then canonical_address_p(get_slice_int(48, jmp_addr, 0))
else let cs_limit = (seg_hidden_limits[1]) : bits(32) in
((0) <= (jmp_addr)) & ((jmp_addr) <= (unsigned(cs_limit))))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':BAD-RETURN-ADDRESS', 'JMP-ADDR']"))
else write_iptr(proc_mode, get_slice_int(48, jmp_addr, 0))

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
val x86_far_jmp_op_en_d : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_far_jmp_op_en_d (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-FAR-JMP-OP/EN-D") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then throw(Emsg("Model state error: :SOURCE-OPERAND-NOT-MEMORY-LOCATION"))
else let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let offset_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg, mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), the_range(2, 10, (2) + (offset_size)), inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, increment_rip_by)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let selector = (n16(get_slice_int(16, mem, 0))) : bits(16) in
let offset = (get_slice_int(ash(offset_size, 3), mem, 16)) in
let sel_index = (segment_selectorbits_get_index(selector)) : bits(13) in
let sel_ti = (segment_selectorbits_get_ti(selector)) : bits(1) in
let sel_rpl = (segment_selectorbits_get_rpl(selector)) : bits(2) in
if ((the_bits(8, sel_ti)) == (get_slice_int(8, 0, 0))) & ((the_bits(8, sel_index)) == (get_slice_int(8, 0, 0)))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':GP-NULLSELECTOR', '0']"))
else let (dt_base_addr, dt_limit) = (if (the_bits(8, sel_ti)) == (get_slice_int(8, 0, 0))
then let (elem0, elem1) = (let gdtr = (strs[0]) : bits(80) in
let gdtr_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then gdtr_idtrbits_get_base_addr(gdtr)
else the_bits(64, n32(the_bits(32, gdtr_idtrbits_get_base_addr(gdtr))))) : bits(64) in
let gdtr_limit = (gdtr_idtrbits_get_limit(gdtr)) : bits(16) in
(gdtr_base, gdtr_limit)) : (bits(64), bits(16)) in
(elem0, the_bits(32, elem1))
else let ldtr_base = (ssr_hidden_bases[0]) : bits(64) in
let ldtr_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then ldtr_base
else the_bits(64, n32(the_bits(32, ldtr_base)))) : bits(64) in
let ldtr_limit = (ssr_hidden_limits[0]) : bits(32) in
(ldtr_base, ldtr_limit)) : (bits(64), bits(32)) in
let largest_address = ((ash(unsigned(sel_index), 3)) + (7)) : int in
if (unsigned(dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'SELECTOR', 'DT-BASE-ADDR', 'DT-LIMIT']]"))
else let descriptor_addr = ((unsigned(dt_base_addr)) + (unsigned(the_bits(16, ash(unsigned(sel_index), 3))))) : int in
if not_bool(canonical_address_p(get_slice_int(48, descriptor_addr, 0)))
then throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else let (flg, descriptor) = (rml_size(8, get_slice_int(48, descriptor_addr, 0), ":X")) : (option(string), int) in
let descriptor = (the_bits(64, descriptor)) : bits(64) in
if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else let s = (code_segment_descriptorbits_get_s(descriptor)) : bits(1) in
if (unsigned(s)) == (1)
then let msb_of_type = (code_segment_descriptorbits_get_msb_of_type(descriptor)) : bits(1) in
if not_bool((unsigned(msb_of_type)) == (1))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':JMP-FAR-DATA-SEGMENT', 'SEL-INDEX']"))
else let d = (code_segment_descriptorbits_get_d(descriptor)) : bits(1) in
if ((get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))) & ((unsigned(d)) == (1))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':CS.D=1-IN-64-BIT-MODE', 'SEL-INDEX']"))
else let cpl = (let x86 = (()) : unit in
segment_selectorbits_get_rpl(seg_visibles[1])) : bits(2) in
let dpl = (code_segment_descriptorbits_get_dpl(descriptor)) : bits(2) in
let c = (code_segment_descriptorbits_get_c(descriptor)) : bits(1) in
let allowed = (if (unsigned(c)) == (1)
then (unsigned(dpl)) <= (unsigned(cpl))
else ((unsigned(sel_rpl)) <= (unsigned(cpl))) & ((the_bits(8, cpl)) == (the_bits(8, dpl)))) : bool in
if not_bool(allowed)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':DPL', 'DPL', ['ACONS', ':CPL', 'CPL', ['ACONS', ':RPL', 'SEL-RPL', 'NIL']]]]"))
else let p = (code_segment_descriptorbits_get_p(descriptor)) : bits(1) in
if not_bool((unsigned(p)) == (1))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':NP', 'SEL-INDEX', ':CODE-SEGMENT-NOT-PRESENT', 'SEL-INDEX']"))
else let jmp_addr = ((match offset_size {
2 => the_bits(65, n16(the_bits(16, offset))),
4 => the_bits(65, n32(the_bits(32, offset))),
_ => the_sbits(65, I64(the_bits(64, offset)))
}) : sbits(65)) : sbits(65) in
let jmp_addr_ok = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then canonical_address_p(the_sbits(48, jmp_addr))
else let limit15_0 = (code_segment_descriptorbits_get_limit15_0(descriptor)) : bits(16) in
let limit19_16 = (code_segment_descriptorbits_get_limit19_16(descriptor)) : bits(4) in
let limit = (changeSlice(ash(unsigned(limit19_16), 16), 0, 16, unsigned(limit15_0))) : int in
let g = (code_segment_descriptorbits_get_g(descriptor)) : bits(1) in
let max_offset : int = (if (unsigned(g)) == (1)
then (ash(limit, 12)) - (1)
else limit) : int in
(signed(jmp_addr)) < (max_offset : int)) : bool in
if not_bool(jmp_addr_ok)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':NONCANONICAL-OR-OUTSIDE-SEGMENT-LIMIT', 'JMP-ADDR']"))
else let new_cs_visible = (set_segment_selectorbits_get_rpl(cpl, selector)) : bits(16) in
{ seg_visibles[1] = new_cs_visible;
seg_hidden_bases[1] = dt_base_addr;
seg_hidden_limits[1] = dt_limit;
seg_hidden_attrs[1] = make_code_segment_attr_field(descriptor);
write_iptr(proc_mode, the_sbits(48, jmp_addr))
}
else if not_bool((get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0)))
then throw(Emsg("Model state error: :FAR-JMP-SYSTEM-UNIMPLEMENTED-IN-32-BIT-MODE"))
else let largest_address = ((ash(unsigned(sel_index), 3)) + (15)) : int in
if (unsigned(dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'SELECTOR', 'DT-BASE-ADDR', 'DT-LIMIT']]"))
else let (flg, descriptor) = (rml_size(16, get_slice_int(48, descriptor_addr, 0), ":X")) : (option(string), int) in
let descriptor = (the_bits(128, descriptor)) : bits(128) in
if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else let (call_gate_desc?, reason2) = (ia32e_valid_call_gate_segment_descriptor_p(descriptor)) : (bool, (string, (bits(128)))) in
if call_gate_desc?
then let cpl = (let x86 = (()) : unit in
segment_selectorbits_get_rpl(seg_visibles[1])) : bits(2) in
let dpl = (call_gate_descriptorbits_get_dpl(descriptor)) : bits(2) in
if not_bool(((unsigned(cpl)) <= (unsigned(dpl))) & ((unsigned(sel_rpl)) <= (unsigned(dpl))))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':DPL', 'DPL', ['ACONS', ':CPL', 'CPL', ['ACONS', ':RPL', 'SEL-RPL', 'NIL']]]]"))
else let cs_selector = (call_gate_descriptorbits_get_selector(descriptor)) : bits(16) in
let cs_sel_index = (segment_selectorbits_get_index(cs_selector)) : bits(13) in
let cs_sel_ti = (segment_selectorbits_get_ti(cs_selector)) : bits(1) in
let cs_sel_rpl = (segment_selectorbits_get_rpl(cs_selector)) : bits(2) in
if ((the_bits(8, cs_sel_ti)) == (get_slice_int(8, 0, 0))) & ((the_bits(8, cs_sel_index)) == (get_slice_int(8, 0, 0)))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':CALL-GATE-CODE-SEGMENT-NULLSELECTOR', '0']"))
else let (cs_dt_base_addr, cs_dt_limit) = (if (the_bits(8, sel_ti)) == (get_slice_int(8, 0, 0))
then let (elem0, elem1) = (let gdtr = (strs[0]) : bits(80) in
let gdtr_base = (gdtr_idtrbits_get_base_addr(gdtr)) : bits(64) in
let gdtr_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then gdtr_base
else the_bits(64, n32(the_bits(32, gdtr_base)))) : bits(64) in
let gdtr_limit = (gdtr_idtrbits_get_limit(gdtr)) : bits(16) in
(gdtr_base, gdtr_limit)) : (bits(64), bits(16)) in
(elem0, the_bits(32, elem1))
else let ldtr_base = (ssr_hidden_bases[0]) : bits(64) in
let ldtr_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then ldtr_base
else the_bits(64, n32(the_bits(32, ldtr_base)))) : bits(64) in
let ldtr_limit = (ssr_hidden_limits[0]) : bits(32) in
(ldtr_base, ldtr_limit)) : (bits(64), bits(32)) in
let largest_address = ((ash(unsigned(cs_sel_index), 3)) + (7)) : int in
if (unsigned(cs_dt_limit)) < (largest_address)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':GP-SELECTOR-LIMIT-CHECK-FAILED', ['LIST', 'CS-SELECTOR', 'CS-DT-BASE-ADDR', 'CS-DT-LIMIT']]"))
else let cs_descriptor_addr = ((unsigned(cs_dt_base_addr)) + (unsigned(the_bits(16, ash(unsigned(cs_sel_index), 3))))) : int in
if not_bool(canonical_address_p(get_slice_int(48, cs_descriptor_addr, 0)))
then throw(Emsg("Model state error: :CS-DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else let (flg, cs_descriptor) = (rml_size(8, get_slice_int(48, cs_descriptor_addr, 0), ":X")) : (option(string), int) in
let cs_descriptor = (the_bits(64, cs_descriptor)) : bits(64) in
if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else let (valid?, reason) = (ia32e_valid_code_segment_descriptor_p(cs_descriptor)) : (bool, (string, (bits(64)))) in
if not_bool(valid?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':CALL-GATE-CODE-SEGMENT-DESCRIPTOR-INVALID', ['CONS', 'REASON', 'CS-DESCRIPTOR']]"))
else let cs_dpl = (code_segment_descriptorbits_get_dpl(cs_descriptor)) : bits(2) in
let c_bit = (code_segment_descriptorbits_get_c(cs_descriptor)) : bits(1) in
if (((the_bits(8, c_bit)) == (get_slice_int(8, 1, 0))) & (not_bool((unsigned(cs_dpl)) <= (unsigned(cpl))))) | (((the_bits(8, c_bit)) == (get_slice_int(8, 0, 0))) & (not_bool((the_bits(8, cs_dpl)) == (the_bits(8, cpl)))))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'CS-SEL-INDEX', ':PRIVILEGE-CHECK-FAIL', ['ACONS', ':C-BIT', 'C-BIT', ['ACONS', ':CPL', 'CPL', ['ACONS', ':CS-DPL', 'CS-DPL', 'NIL']]]]"))
else let call_gate_offset15_0 = (call_gate_descriptorbits_get_offset15_0(descriptor)) : bits(16) in
let call_gate_offset31_16 = (call_gate_descriptorbits_get_offset31_16(descriptor)) : bits(16) in
let call_gate_offset63_32 = (call_gate_descriptorbits_get_offset63_32(descriptor)) : bits(32) in
let call_gate_offset31_0 = (changeSlice(ash(unsigned(call_gate_offset31_16), 16), 0, 16, unsigned(call_gate_offset15_0))) : int in
let call_gate_offset = (changeSlice(ash(unsigned(call_gate_offset63_32), 32), 0, 32, call_gate_offset31_0)) : int in
let jmp_addr = ((match offset_size {
2 => unsigned(n16(get_slice_int(16, call_gate_offset, 0))),
4 => unsigned(n32(get_slice_int(32, call_gate_offset, 0))),
_ => call_gate_offset
}) : int) : int in
if not_bool(canonical_address_p(get_slice_int(48, jmp_addr, 0)))
then throw(Emsg("Model state error: :TARGET-OFFSET-VIRTUAL-MEMORY-ERROR"))
else let new_cs_visible = (set_segment_selectorbits_get_rpl(cpl, cs_selector)) : bits(16) in
{ seg_visibles[1] = new_cs_visible;
seg_hidden_bases[1] = cs_dt_base_addr;
seg_hidden_limits[1] = cs_dt_limit;
seg_hidden_attrs[1] = make_code_segment_attr_field(cs_descriptor);
write_iptr(proc_mode, get_slice_int(48, jmp_addr, 0))
}
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', 'SEL-INDEX', ':EITHER-BOTH-CODE-SEGMENT-OR-CALL-GATE-ARE-ABSENT-OR-SOME-OTHER-DESCRIPTOR-IS-PRESENT', ['CONS', 'REASON2', 'DESCRIPTOR']]"))

val x86_loop : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_loop (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LOOP") : string in
let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 1, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let counter_size = (the_range(2, 8, select_address_size(proc_mode, p4?))) : {|8, 2, 4|} in
let counter = (rgfi_size(get_slice_int(4, counter_size, 0), get_slice_int(4, 1, 0), rex_byte)) : bits(64) in
let counter = (sail_mask(64, trunc(counter_size, get_slice_int(64, (unsigned(counter)) - (1), 0)))) : bits(64) in
let zf = (the_bits(1, let rflags_var = (rflags) : bits(32) in
the_bits(2, rflagsbits_get_zf(rflags_var)))) : bits(1) in
let branch_cond = (if (opcode) == (get_slice_int(8, 226, 0))
then not_bool((the_bits(8, counter)) == (get_slice_int(8, 0, 0)))
else if (opcode) == (get_slice_int(8, 225, 0))
then ((the_bits(8, zf)) == (get_slice_int(8, 1, 0))) & (not_bool((the_bits(8, counter)) == (get_slice_int(8, 0, 0))))
else ((the_bits(8, zf)) == (get_slice_int(8, 0, 0))) & (not_bool((the_bits(8, counter)) == (get_slice_int(8, 0, 0))))) : bool in
if branch_cond
then { let (flg, rel8) = (rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, (1) + (signed(rel8)), 0))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else { write_rgfi_size(get_slice_int(4, counter_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
write_iptr(proc_mode, the_sbits(48, next_rip))
}
}
else { let (flg, next_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else { write_rgfi_size(get_slice_int(4, counter_size, 0), get_slice_int(4, 1, 0), unsigned(counter), rex_byte);
write_iptr(proc_mode, the_sbits(48, next_rip))
}
}

