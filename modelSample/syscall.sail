$include <prelude.sail>
$include <string.sail>

val x86_syscall : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, wreg}
function x86_syscall (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SYSCALL") in
let ia32_efer : bits(12) = (truncate(msrs[0], 12)) in
let ia32_efer_sce : bits(1) = (ia32_eferbits_get_sce(ia32_efer)) in
{ if (ia32_efer_sce) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':UD', 'NIL', ':IA32-EFER-SCE=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0977179978>, 'IA32-EFER']]"))
else ();
let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
{ write_rgfi(1, sail_sign_extend(temp_rip, 64));
let lstar : bits(64) = (msrs[5]) in
let lstar_addr : sbits(64) = (lstar) in
{ if not_bool(canonical_address_p(signed(lstar_addr)))
then throw(Emsg("Model state error: :LSTAR-NOT-CANONICAL"))
else ();
rip = truncate(lstar_addr, 48);
let eflags : bits(32) = (rflags) in
{ wr64(0xb, sail_zero_extend(eflags, 64));
let fmask : bits(64) = (msrs[6]) in
let not_fmask : int = (lognot(unsigned(fmask))) in
let new_eflags : bits(32) = (bits_of_int(logand(unsigned(eflags), not_fmask), 32)) in
{ rflags = new_eflags;
let star : bits(64) = (msrs[4]) in
let new_cs_selector : bits(16) = (logand(star[47 .. 32], 0xfffc)) in
{ seg_visibles[1] = new_cs_selector;
let cs_hidden_base_addr : {|0|} = (0) in
let cs_hidden_limit : {|4294967295|} = (4294967295) in
let cs_attr : bits(16) = (change_code_segment_descriptor_attributesbits(cs_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b1), Some(0b1), Some(0b00), Some(0b1), None(), Some(0b1), Some(0b0), Some(0b1), None())) in
{ seg_hidden_bases[1] = bits_of_int(cs_hidden_base_addr, 64);
seg_hidden_limits[1] = bits_of_int(cs_hidden_limit, 32);
seg_hidden_attrs[1] = cs_attr;
let new_ss_selector : int = ((unsigned(star[47 .. 32])) + (8)) in
{ if not_bool(fits_in_signed_bitvector(16, new_ss_selector))
then throw(Emsg("Model state error: :NEW-SS-SELECTOR-TOO-LARGE"))
else ();
seg_visibles[2] = bits_of_int(new_ss_selector, 16);
let ss_hidden_base_addr : {|0|} = (0) in
let ss_hidden_limit : {|4294967295|} = (4294967295) in
let ss_attr : bits(16) = (seg_hidden_attrs[2]) in
let ss_attr : bits(16) = (change_data_segment_descriptor_attributesbits(ss_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b0), Some(0b1), Some(0b00), Some(0b1), None(), None(), Some(0b1), Some(0b1), None())) in
{ seg_hidden_bases[2] = bits_of_int(ss_hidden_base_addr, 64);
seg_hidden_limits[2] = bits_of_int(ss_hidden_limit, 32);
seg_hidden_attrs[2] = ss_attr
}
}
}
}
}
}
}
}
}

val x86_syscall_both_views : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, wreg}
function x86_syscall_both_views (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SYSCALL-BOTH-VIEWS") in
if app_view
then { unimplemented_x86_syscall_app_view(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else { x86_syscall(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}

/*Return from fast system call to user code at privilege level 
 3*/
/*<p>Op/En: NP<br/> 
 0F 07: SYSRET<br/> 
 REX.W + 0F 07: SYSRET</p> 
 
 <p>SYSRET when REX.W is not set is not supported yet because 0F 07 
 ( as opposed to REX.W + 0F 07 ) switches the machine to 
 compatibility mode, not 64-bit mode.</p>*/
val x86_sysret : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg}
function x86_sysret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SYSRET") in
{ if not_bool(logbitp(3, rex_byte))
then throw(Emsg("Model state error: :UNSUPPORTED-SYSRET-BECAUSE-REX.W!=1"))
else ();
let ia32_efer : bits(12) = (truncate(msrs[0], 12)) in
let ia32_efer_sce : bits(1) = (ia32_eferbits_get_sce(ia32_efer)) in
{ if (ia32_efer_sce) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':UD', 'NIL', ':IA32-EFER-SCE=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0976f2b940>, 'IA32-EFER']]"))
else ();
let current_cs_register : bits(16) = (seg_visibles[1]) in
let cpl : bits(2) = (segment_selectorbits_get_rpl(current_cs_register)) in
{ if not_bool((0b00) == (cpl))
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':CPL!=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0976aeecf8>, 'CURRENT-CS-REGISTER']]"))
else ();
let rcx_var : sbits(64) = (rgfi(1)) in
{ if not_bool(canonical_address_p(signed(rcx_var)))
then throw(Emsg("Model state error: :RCX-NON-CANONICAL"))
else ();
rip = truncate(rcx_var, 48);
let r11_var : bits(32) = (truncate(rgfi(11), 32)) in
{ rflags = logior(logand(r11_var, 0x003c_7fd7), 0x0000_0002);
let star : bits(64) = (msrs[4]) in
let new_cs_selector : int = ((unsigned(star[63 .. 48])) + (16)) in
{ if not_bool(fits_in_signed_bitvector(16, new_cs_selector))
then throw(Emsg("Model state error: :NEW-CS-SELECTOR-TOO-LARGE"))
else ();
let new_cs_selector : bits(16) = (set_segment_selectorbits_get_rpl(0b11, bits_of_int(new_cs_selector, 16))) in
{ seg_visibles[1] = new_cs_selector;
let cs_base_addr : {|0|} = (0) in
let cs_limit : {|4294967295|} = (4294967295) in
let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_attr : bits(16) = (change_code_segment_descriptor_attributesbits(cs_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b1), Some(0b1), Some(0b11), Some(0b1), None(), Some(0b1), Some(0b0), Some(0b1), None())) in
{ seg_hidden_bases[1] = bits_of_int(cs_base_addr, 64);
seg_hidden_limits[1] = bits_of_int(cs_limit, 32);
seg_hidden_attrs[1] = cs_attr;
let current_cs_register : bits(16) = (set_segment_selectorbits_get_rpl(0b00, current_cs_register)) in
{ seg_visibles[1] = current_cs_register;
let new_ss_selector : int = ((unsigned(star[63 .. 48])) + (8)) in
{ if not_bool(fits_in_signed_bitvector(16, new_ss_selector))
then throw(Emsg("Model state error: :NEW-SS-SELECTOR-TOO-LARGE"))
else ();
let new_ss_selector : bits(16) = (set_segment_selectorbits_get_rpl(0b11, bits_of_int(new_ss_selector, 16))) in
{ seg_visibles[2] = new_ss_selector;
let ss_base_addr : {|0|} = (0) in
let ss_limit : {|4294967295|} = (4294967295) in
let ss_attr : bits(16) = (seg_hidden_attrs[2]) in
let ss_attr : bits(16) = (change_data_segment_descriptor_attributesbits(ss_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b0), Some(0b1), Some(0b11), Some(0b1), None(), None(), Some(0b1), Some(0b1), None())) in
{ seg_hidden_bases[2] = bits_of_int(ss_base_addr, 64);
seg_hidden_limits[2] = bits_of_int(ss_limit, 32);
seg_hidden_attrs[2] = ss_attr
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
}
}

