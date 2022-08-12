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
val x86_lgdt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_lgdt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LGDT") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
{ if app_view
then throw(Emsg("Model state error: :LGDT-UNIMPLEMENTED-IN-APP-VIEW"))
else ();
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let base_size : {|4, 8|} = { (if n64_bit_modep()
then 8
else 4) } in
let base_size_plus_2 : {|6, 10|} = ((2) + (base_size)) in
let inst_ac? : bool = (false) in
let (flg0, mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, base_size_plus_2, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let p3? : bool = ((0x66) == (prefixes_get_opr(prefixes))) in
let operand_size : {|2, 4, 8|} = { (if (base_size) == (8)
then 8
else let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_d : bits(1) = (code_segment_descriptor_attributesbits_get_d(cs_attr)) in
if (cs_d) == (0b1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2) } in
let base_bits : {|24, 32, 64|} = { (match operand_size {
8 => 64,
4 => 32,
_ => 24
}) } in
let gdtr_limit : bits(80) = (set_gdtr_idtrbits_get_limit(mem[15 .. 0], 0x0000_0000_0000_0000_0000)) in
let gdtr : bits(80) = (set_gdtr_idtrbits_get_base_addr(sail_mask(64, bitslice(mem, 16, base_bits)), gdtr_limit)) in
{ strs[0] = gdtr;
write_iptr(proc_mode, temp_rip)
}
}
}
}
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
val x86_lidt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_lidt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LIDT") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
{ if app_view
then throw(Emsg("Model state error: :LIDT-UNIMPLEMENTED"))
else ();
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let base_size : {|4, 8|} = { (if n64_bit_modep()
then 8
else 4) } in
let base_size_plus_2 : {|6, 10|} = ((2) + (base_size)) in
let inst_ac? : bool = (false) in
let (flg0, mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, base_size_plus_2, inst_ac?, true, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let p3? : bool = ((0x66) == (prefixes_get_opr(prefixes))) in
let operand_size : {|2, 4, 8|} = { (if (base_size) == (8)
then 8
else let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_d : bits(1) = (code_segment_descriptor_attributesbits_get_d(cs_attr)) in
if (cs_d) == (0b1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2) } in
let base_bits : {|24, 32, 64|} = { (match operand_size {
8 => 64,
4 => 32,
_ => 24
}) } in
let idtr_limit : bits(80) = (set_gdtr_idtrbits_get_limit(mem[15 .. 0], 0x0000_0000_0000_0000_0000)) in
let idtr : bits(80) = (set_gdtr_idtrbits_get_base_addr(sail_mask(64, bitslice(mem, 16, base_bits)), idtr_limit)) in
{ strs[1] = idtr;
write_iptr(proc_mode, temp_rip)
}
}
}
}
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
val x86_lldt : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_lldt (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LLDT") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
{ if app_view
then throw(Emsg("Model state error: :LLDT-UNIMPLEMENTED"))
else ();
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (false) in
let (flg0, selector, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, 2, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let sel_index : bits(13) = (segment_selectorbits_get_index(truncate(selector, 16))) in
let sel_ti : bits(1) = (segment_selectorbits_get_ti(truncate(selector, 16))) in
let sel_rpl : bits(2) = (segment_selectorbits_get_rpl(truncate(selector, 16))) in
{ if (sel_ti) == (0b1)
then throw(Emsg("Model state error: :GP-SELECTOR-DOES-NOT-POINT-TO-GDT"))
else ();
let gdtr : bits(80) = (strs[0]) in
let gdtr_base : bits(64) = { (if (proc_mode) == (0)
then gdtr_idtrbits_get_base_addr(gdtr)
else sail_zero_extend(truncate(gdtr_idtrbits_get_base_addr(gdtr), 32), 64)) } in
let gdtr_limit : bits(16) = (gdtr_idtrbits_get_limit(gdtr)) in
let largest_address : int = ((ash(unsigned(sel_index), 3)) + (if (proc_mode) == (0)
then 15
else 7)) in
{ if (unsigned(gdtr_limit)) < (largest_address)
then throw(Emsg("Model state error: :GP-SELECTOR-LIMIT-CHECK-FAILED"))
else ();
if (sel_index) == (0b0_0000_0000_0000)
then ssr_visibles[0] = truncate(selector, 16)
else let descriptor_addr : int = ((unsigned(gdtr_base)) + (unsigned(bits_of_int(ash(unsigned(sel_index), 3), 16)))) in
{ if not_bool(canonical_address_p(descriptor_addr))
then throw(Emsg("Model state error: :DESCRIPTOR-ADDR-VIRTUAL-MEMORY-ERROR"))
else ();
let descriptor_size : {|8, 16|} = { (if (proc_mode) == (0)
then 16
else 8) } in
let (flg, descriptor) : (option(string), bits(128)) = (rml_size(descriptor_size, bits_of_int(descriptor_addr, 48), ":X")) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RML-SIZE-ERROR"))
else ();
let (descriptor_valid?, reason) : (bool, (string, (bits(128)))) = (ia32e_valid_ldt_segment_descriptor_p(descriptor)) in
{ if not_bool(descriptor_valid?)
then throw(Emsg("Model state error: :INVALID-SEGMENT-DESCRIPTOR"))
else ();
let ldtr_base15_0 : bits(16) = (system_segment_descriptorbits_get_base15_0(descriptor)) in
let ldtr_base23_16 : bits(8) = (system_segment_descriptorbits_get_base23_16(descriptor)) in
let ldtr_base31_24 : bits(8) = (system_segment_descriptorbits_get_base31_24(descriptor)) in
let ldtr_base63_32 : bits(32) = (system_segment_descriptorbits_get_base63_32(descriptor)) in
let ldtr_base63_24 : bits(40) = (bits_of_int(changeSlice(ash(unsigned(ldtr_base63_32), 8), 0, 8, unsigned(ldtr_base31_24)), 40)) in
let ldtr_base23_0 : bits(24) = (bits_of_int(changeSlice(ash(unsigned(ldtr_base23_16), 16), 0, 16, unsigned(ldtr_base15_0)), 24)) in
let ldtr_base : bits(64) = (bits_of_int(changeSlice(ash(unsigned(ldtr_base63_24), 24), 0, 24, unsigned(ldtr_base23_0)), 64)) in
let ldtr_limit15_0 : bits(16) = (system_segment_descriptorbits_get_limit15_0(descriptor)) in
let ldtr_limit19_16 : bits(4) = (system_segment_descriptorbits_get_limit19_16(descriptor)) in
let ldtr_limit : bits(32) = (bits_of_int(changeSlice(ash(unsigned(ldtr_limit19_16), 16), 0, 16, unsigned(ldtr_limit15_0)), 32)) in
let ldtr_attr : bits(16) = (make_system_segment_attr_field(descriptor)) in
{ ssr_visibles[0] = truncate(selector, 16);
ssr_hidden_bases[0] = ldtr_base;
ssr_hidden_limits[0] = ldtr_limit;
ssr_hidden_attrs[0] = ldtr_attr;
write_iptr(proc_mode, temp_rip)
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

