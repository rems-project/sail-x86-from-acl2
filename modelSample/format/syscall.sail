
val x86_syscall : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_syscall (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSCALL";
    let ia32_efer : bits(12) = truncate(msrs[0], 12);
    let ia32_efer_sce : bits(1) = ia32_eferbits_get_sce(ia32_efer);
    if ia32_efer_sce == 0 then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':UD', 'NIL', ':IA32-EFER-SCE=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0977179978>, 'IA32-EFER']]"));
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    write_rgfi(1, sail_sign_extend(temp_rip, 64));
    let lstar : bits(64) = msrs[5];
    let lstar_addr : sbits(64) = lstar;
    if not_bool(canonical_address_p(signed(lstar_addr))) then throw(Emsg("Model state error: :LSTAR-NOT-CANONICAL"));
    rip = truncate(lstar_addr, 48);
    let eflags : bits(32) = rflags;
    wr64(0xb, sail_zero_extend(eflags, 64));
    let fmask : bits(64) = msrs[6];
    let not_fmask : int = lognot(unsigned(fmask));
    let new_eflags : bits(32) = bits_of_int(logand(unsigned(eflags), not_fmask), 32);
    rflags = new_eflags;
    let star : bits(64) = msrs[4];
    let new_cs_selector : bits(16) = logand(star[47 .. 32], 0xfffc);
    seg_visibles[1] = new_cs_selector;
    let cs_hidden_base_addr : {|0|} = 0;
    let cs_hidden_limit : {|4294967295|} = 4294967295;
    let cs_attr : bits(16) = change_code_segment_descriptor_attributesbits(cs_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b1), Some(0b1), Some(0b00), Some(0b1), None(), Some(0b1), Some(0b0), Some(0b1), None());
    seg_hidden_bases[1] = bits_of_int(cs_hidden_base_addr, 64);
    seg_hidden_limits[1] = bits_of_int(cs_hidden_limit, 32);
    seg_hidden_attrs[1] = cs_attr;
    let new_ss_selector : int = unsigned(star[47 .. 32]) + 8;
    if not_bool(fits_in_signed_bitvector(16, new_ss_selector)) then throw(Emsg("Model state error: :NEW-SS-SELECTOR-TOO-LARGE"));
    seg_visibles[2] = bits_of_int(new_ss_selector, 16);
    let ss_hidden_base_addr : {|0|} = 0;
    let ss_hidden_limit : {|4294967295|} = 4294967295;
    let ss_attr : bits(16) = seg_hidden_attrs[2];
    let ss_attr : bits(16) = change_data_segment_descriptor_attributesbits(ss_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b0), Some(0b1), Some(0b00), Some(0b1), None(), None(), Some(0b1), Some(0b1), None());
    seg_hidden_bases[2] = bits_of_int(ss_hidden_base_addr, 64);
    seg_hidden_limits[2] = bits_of_int(ss_hidden_limit, 32);
    seg_hidden_attrs[2] = ss_attr
}

val x86_syscall_both_views : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_syscall_both_views (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSCALL-BOTH-VIEWS";
    if app_view then {
        unimplemented_x86_syscall_app_view(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
    } else {
        x86_syscall(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
    }
}

val x86_sysret : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_sysret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSRET";
    if not_bool(logbitp(3, rex_byte)) then throw(Emsg("Model state error: :UNSUPPORTED-SYSRET-BECAUSE-REX.W!=1"));
    let ia32_efer : bits(12) = truncate(msrs[0], 12);
    let ia32_efer_sce : bits(1) = ia32_eferbits_get_sce(ia32_efer);
    if ia32_efer_sce == 0 then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':UD', 'NIL', ':IA32-EFER-SCE=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0976f2b940>, 'IA32-EFER']]"));
    let current_cs_register : bits(16) = seg_visibles[1];
    let cpl : bits(2) = segment_selectorbits_get_rpl(current_cs_register);
    if not_bool(0b00 == cpl) then throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':CPL!=0', ['CONS', <lex_parse.ACL2quote object at 0x7f0976aeecf8>, 'CURRENT-CS-REGISTER']]"));
    let rcx_var : sbits(64) = rgfi(1);
    if not_bool(canonical_address_p(signed(rcx_var))) then throw(Emsg("Model state error: :RCX-NON-CANONICAL"));
    rip = truncate(rcx_var, 48);
    let r11_var : bits(32) = truncate(rgfi(11), 32);
    rflags = logior(logand(r11_var, 0x003c7fd7), 0x00000002);
    let star : bits(64) = msrs[4];
    let new_cs_selector : int = unsigned(star[63 .. 48]) + 16;
    if not_bool(fits_in_signed_bitvector(16, new_cs_selector)) then throw(Emsg("Model state error: :NEW-CS-SELECTOR-TOO-LARGE"));
    let new_cs_selector : bits(16) = set_segment_selectorbits_get_rpl(0b11, bits_of_int(new_cs_selector, 16));
    seg_visibles[1] = new_cs_selector;
    let cs_base_addr : {|0|} = 0;
    let cs_limit : {|4294967295|} = 4294967295;
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    let cs_attr : bits(16) = change_code_segment_descriptor_attributesbits(cs_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b1), Some(0b1), Some(0b11), Some(0b1), None(), Some(0b1), Some(0b0), Some(0b1), None());
    seg_hidden_bases[1] = bits_of_int(cs_base_addr, 64);
    seg_hidden_limits[1] = bits_of_int(cs_limit, 32);
    seg_hidden_attrs[1] = cs_attr;
    let current_cs_register : bits(16) = set_segment_selectorbits_get_rpl(0b00, current_cs_register);
    seg_visibles[1] = current_cs_register;
    let new_ss_selector : int = unsigned(star[63 .. 48]) + 8;
    if not_bool(fits_in_signed_bitvector(16, new_ss_selector)) then throw(Emsg("Model state error: :NEW-SS-SELECTOR-TOO-LARGE"));
    let new_ss_selector : bits(16) = set_segment_selectorbits_get_rpl(0b11, bits_of_int(new_ss_selector, 16));
    seg_visibles[2] = new_ss_selector;
    let ss_base_addr : {|0|} = 0;
    let ss_limit : {|4294967295|} = 4294967295;
    let ss_attr : bits(16) = seg_hidden_attrs[2];
    let ss_attr : bits(16) = change_data_segment_descriptor_attributesbits(ss_attr, Some(0b1), Some(0b1), Some(0b0), Some(0b0), Some(0b1), Some(0b11), Some(0b1), None(), None(), Some(0b1), Some(0b1), None());
    seg_hidden_bases[2] = bits_of_int(ss_base_addr, 64);
    seg_hidden_limits[2] = bits_of_int(ss_limit, 32);
    seg_hidden_attrs[2] = ss_attr
}
