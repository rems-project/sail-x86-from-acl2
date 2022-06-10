$include <prelude.sail>
$include <string.sail>

/*<h3>Op/En = M: [OP m16@ ( ' & ' ) m32]</h3> 
 <h3>Op/En = M: [OP m16@ ( ' & ' ) m64]</h3> 
 
 <p>In 64-bit mode, the instruction ' s operand size is fixed at 8+2 
 bytes ( an 8-byte base and a 2-byte limit ) .</p> 
 
 <p>[OP M]<br/> 
 0F 01/2: LGDT m16@ ( ' & ' ) 32<br/> 
 0F 01/2: LGDT m16@ ( ' & ' ) 64</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p>*/
val x86_lgdt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_lgdt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LGDT") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
if app_view()
then throw(Emsg("Model state error: :LGDT-UNIMPLEMENTED-IN-APP-VIEW"))
else let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let base_size = (the_range(4, 8, if n64_bit_modep()
then 8
else 4)) : {|8, 4|} in
let base_size_plus_2 = (the_range(6, 10, (2) + (base_size))) : {|10, 6|} in
let inst_ac? = (false) : bool in
let (flg0, mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), base_size_plus_2, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let p3? = ((get_slice_int(8, 102, 0)) == (prefixes_get_opr(prefixes))) : bool in
let operand_size = (if (get_slice_int(8, base_size, 0)) == (get_slice_int(8, 8, 0))
then 8
else let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
if (unsigned(cs_d)) == (1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2) : {|8, 2, 4|} in
let base_bits = ((match operand_size {
8 => 64,
4 => 32,
_ => 24
}) : {|64, 24, 32|}) : {|64, 24, 32|} in
let gdtr_limit = (set_gdtr_idtrbits_get_limit(get_slice_int(16, mem, 0), get_slice_int(80, 0, 0))) : bits(80) in
let gdtr = (set_gdtr_idtrbits_get_base_addr(the_bits(64, get_slice_int(base_bits, mem, 16)), gdtr_limit)) : bits(80) in
{ strs[0] = gdtr;
write_iptr(proc_mode, temp_rip)
}

/*<h3>Op/En = M: [OP m16@ ( ' & ' ) m32]</h3> 
 <h3>Op/En = M: [OP m16@ ( ' & ' ) m64]</h3> 
 
 <p>In 64-bit mode, the instruction ' s operand size is fixed at 8+2 
 bytes ( an 8-byte base and a 2-byte limit ) .</p> 
 
 <p>[OP M]<br/> 
 0F 01/3: LIDT m16@ ( ' & ' ) 32<br/> 
 0F 01/3: LIDT m16@ ( ' & ' ) 64</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p>*/
val x86_lidt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_lidt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LIDT") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
if app_view()
then throw(Emsg("Model state error: :LIDT-UNIMPLEMENTED"))
else let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let base_size = (the_range(4, 8, if n64_bit_modep()
then 8
else 4)) : {|8, 4|} in
let base_size_plus_2 = (the_range(6, 10, (2) + (base_size))) : {|10, 6|} in
let inst_ac? = (false) : bool in
let (flg0, mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), base_size_plus_2, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let p3? = ((get_slice_int(8, 102, 0)) == (prefixes_get_opr(prefixes))) : bool in
let operand_size = (if (get_slice_int(8, base_size, 0)) == (get_slice_int(8, 8, 0))
then 8
else let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
if (unsigned(cs_d)) == (1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2) : {|8, 2, 4|} in
let base_bits = ((match operand_size {
8 => 64,
4 => 32,
_ => 24
}) : {|64, 24, 32|}) : {|64, 24, 32|} in
let idtr_limit = (set_gdtr_idtrbits_get_limit(get_slice_int(16, mem, 0), get_slice_int(80, 0, 0))) : bits(80) in
let idtr = (set_gdtr_idtrbits_get_base_addr(the_bits(64, get_slice_int(base_bits, mem, 16)), idtr_limit)) : bits(80) in
{ strs[1] = idtr;
write_iptr(proc_mode, temp_rip)
}

/*<h3>Op/En = M: [OP r/m16]</h3> 
 [OP M]<br/> 
 0F 00/2: LLDT r/m16<br/> 
 
 <p>If bits 2-15 of the source operand are 0, LDTR is marked invalid 
 and the LLDT instruction completes silently. However, all subsequent 
 references to descriptors in the LDT ( except by the LAR, VERR, VERW or 
 LSL instructions ) cause a general protection exception.</p> 
 
 <p>The operand-size attribute has no effect on this instruction. In 
 64-bit mode, the operand size is fixed at 16 bits.</p> 
 
 <p><b>TO-DO:</b> If a memory address referencing the SS segment is in 
 a non-canonical form, raise the SS exception.</p>*/
val x86_lldt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_lldt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LLDT") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
if app_view()
then throw(Emsg("Model state error: :LLDT-UNIMPLEMENTED"))
else let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (false) : bool in
let (flg0, selector, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), 2, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let sel_index = (segment_selectorbits_get_index(get_slice_int(16, selector, 0))) : bits(13) in
let sel_ti = (segment_selectorbits_get_ti(get_slice_int(16, selector, 0))) : bits(1) in
let sel_rpl = (segment_selectorbits_get_rpl(get_slice_int(16, selector, 0))) : bits(2) in
if (the_bits(8, sel_ti)) == (get_slice_int(8, 1, 0))
then throw(Emsg("Model state error: :GP-SELECTOR-DOES-NOT-POINT-TO-GDT"))
else let gdtr = (strs[0]) : bits(80) in
let gdtr_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then gdtr_idtrbits_get_base_addr(gdtr)
else the_bits(64, n32(the_bits(32, gdtr_idtrbits_get_base_addr(gdtr))))) : bits(64) in
let gdtr_limit = (gdtr_idtrbits_get_limit(gdtr)) : bits(16) in
let largest_address = ((ash(unsigned(sel_index), 3)) + (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then 15
else 7)) : int in
if (unsigned(gdtr_limit)) < (largest_address)
then throw(Emsg("Model state error: :GP-SELECTOR-LIMIT-CHECK-FAILED"))
else if (the_bits(8, sel_index)) == (get_slice_int(8, 0, 0))
then ssr_visibles[0] = get_slice_int(16, selector, 0)
else let descriptor_addr = ((unsigned(gdtr_base)) + (unsigned(the_bits(16, ash(unsigned(sel_index), 3))))) : int in
if not_bool(canonical_address_p(get_slice_int(48, descriptor_addr, 0)))
then throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else let descriptor_size = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then 16
else 8) : {|16, 8|} in
let (flg, descriptor) = (rml_size(descriptor_size, get_slice_int(48, descriptor_addr, 0), ":X")) : (option(string), int) in
let descriptor = (the_bits(128, descriptor)) : bits(128) in
if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else let (descriptor_valid?, reason) = (ia32e_valid_ldt_segment_descriptor_p(descriptor)) : (bool, (string, (bits(128)))) in
if not_bool(descriptor_valid?)
then throw(Emsg("Model state error: :INVALID-SEGMENT-DESCRIPTOR"))
else let ldtr_base15_0 = (system_segment_descriptorbits_get_base15_0(descriptor)) : bits(16) in
let ldtr_base23_16 = (system_segment_descriptorbits_get_base23_16(descriptor)) : bits(8) in
let ldtr_base31_24 = (system_segment_descriptorbits_get_base31_24(descriptor)) : bits(8) in
let ldtr_base63_32 = (system_segment_descriptorbits_get_base63_32(descriptor)) : bits(32) in
let ldtr_base63_24 = (the_bits(40, changeSlice(ash(unsigned(ldtr_base63_32), 8), 0, 8, unsigned(ldtr_base31_24)))) : bits(40) in
let ldtr_base23_0 = (the_bits(24, changeSlice(ash(unsigned(ldtr_base23_16), 16), 0, 16, unsigned(ldtr_base15_0)))) : bits(24) in
let ldtr_base = (the_bits(64, changeSlice(ash(unsigned(ldtr_base63_24), 24), 0, 24, unsigned(ldtr_base23_0)))) : bits(64) in
let ldtr_limit15_0 = (system_segment_descriptorbits_get_limit15_0(descriptor)) : bits(16) in
let ldtr_limit19_16 = (system_segment_descriptorbits_get_limit19_16(descriptor)) : bits(4) in
let ldtr_limit = (the_bits(32, changeSlice(ash(unsigned(ldtr_limit19_16), 16), 0, 16, unsigned(ldtr_limit15_0)))) : bits(32) in
let ldtr_attr = (make_system_segment_attr_field(descriptor)) : bits(16) in
{ ssr_visibles[0] = get_slice_int(16, selector, 0);
ssr_hidden_bases[0] = ldtr_base;
ssr_hidden_limits[0] = ldtr_limit;
ssr_hidden_attrs[0] = ldtr_attr;
write_iptr(proc_mode, temp_rip)
}

