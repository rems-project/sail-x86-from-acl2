
val x86_syscall : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_syscall (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSCALL";
    let ia32_efer : bits(12) = truncate(read_msr(0), 12);
    let ia32_efer_sce : bits(1) = Mk_ia32_eferbits(ia32_efer)[sce];
    if ia32_efer_sce == 0 then x86_fault("(!!FAULT-FRESH :UD NIL :IA32-EFER-SCE=0 (CONS 'IA32_EFER IA32-EFER))");
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    write_rgfi(1, temp_rip);
    let lstar : bits(64) = read_msr(5);
    let lstar_addr : sbits(64) = lstar;
    if not_bool(canonical_address_p(signed(lstar_addr))) then x86_model_error(":LSTAR-NOT-CANONICAL");
    write_rip(lstar_addr);
    let eflags : rflagsbits = rflags;
    wr64(0xb, sail_zero_extend(eflags.bits, 64));
    let fmask : bits(64) = read_msr(6);
    let not_fmask : bits(64) = lognot(fmask);
    let new_eflags : bits(32) = truncate(logand(sail_zero_extend(eflags.bits, 64), not_fmask), 32);
    rflags = Mk_rflagsbits(new_eflags);
    let star : bits(64) = read_msr(4);
    let new_cs_selector : bits(16) = logand(star[47 .. 32], 0xfffc);
    seg_visibles[1] = new_cs_selector;
    let cs_hidden_base_addr : {|0|} = 0;
    let cs_hidden_limit : {|4294967295|} = 4294967295;
    let cs_attr : code_segment_descriptor_attributesbits = [Mk_code_segment_descriptor_attributesbits(cs_attr) with a = 0b1, r = 0b1, c = 0b0, msb_of_type = 0b1, s = 0b1, dpl = 0b00, p = 0b1, l = 0b1, d = 0b0, g = 0b1];
    seg_hidden_bases[1] = bits_of_int(cs_hidden_base_addr, 64);
    seg_hidden_limits[1] = bits_of_int(cs_hidden_limit, 32);
    seg_hidden_attrs[1] = cs_attr.bits;
    let new_ss_selector : int = unsigned(star[47 .. 32]) + 8;
    if not_bool(fits_in_signed_bitvector(16, new_ss_selector)) then x86_model_error(":NEW-SS-SELECTOR-TOO-LARGE");
    seg_visibles[2] = bits_of_int(new_ss_selector, 16);
    let ss_hidden_base_addr : {|0|} = 0;
    let ss_hidden_limit : {|4294967295|} = 4294967295;
    let ss_attr : bits(16) = seg_hidden_attrs[2];
    let ss_attr : data_segment_descriptor_attributesbits = [Mk_data_segment_descriptor_attributesbits(ss_attr) with a = 0b1, w = 0b1, e = 0b0, msb_of_type = 0b0, s = 0b1, dpl = 0b00, p = 0b1, d_b = 0b1, g = 0b1];
    seg_hidden_bases[2] = bits_of_int(ss_hidden_base_addr, 64);
    seg_hidden_limits[2] = bits_of_int(ss_hidden_limit, 32);
    seg_hidden_attrs[2] = ss_attr.bits
}

val x86_syscall_both_views : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_syscall_both_views (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSCALL-BOTH-VIEWS";
    if app_view then {
        unimplemented_x86_syscall_app_view(proc_mode, start_rip, temp_rip, prefixes.bits, rex_byte, opcode, modr_m.bits, sib.bits)
    } else {
        x86_syscall(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
    }
}

/*! Return from fast system call to user code at privilege level 
 3
<p>Op/En: NP<br/> 
 0F 07: SYSRET<br/> 
 REX.W + 0F 07: SYSRET</p> 
 
 <p>SYSRET when REX.W is not set is not supported yet because 0F 07 
 ( as opposed to REX.W + 0F 07 ) switches the machine to 
 compatibility mode, not 64-bit mode.</p> */
val x86_sysret : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_sysret (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-SYSRET";
    if not_bool(logbitp(3, rex_byte)) then x86_model_error(":UNSUPPORTED-SYSRET-BECAUSE-REX.W!=1");
    let ia32_efer : bits(12) = truncate(read_msr(0), 12);
    let ia32_efer_sce : bits(1) = Mk_ia32_eferbits(ia32_efer)[sce];
    if ia32_efer_sce == 0 then x86_fault("(!!FAULT-FRESH :UD NIL :IA32-EFER-SCE=0 (CONS 'IA32_EFER IA32-EFER))");
    let current_cs_register : bits(16) = seg_visibles[1];
    let cpl : bits(2) = Mk_segment_selectorbits(current_cs_register)[rpl];
    if not_bool(0b00 == cpl) then x86_fault("(!!FAULT-FRESH :GP 0 :CPL!=0 (CONS 'CS-REGISTER CURRENT-CS-REGISTER))");
    let rcx_var : sbits(64) = rgfi(1);
    if not_bool(canonical_address_p(signed(rcx_var))) then x86_model_error(":RCX-NON-CANONICAL");
    write_rip(rcx_var);
    let r11_var : bits(32) = truncate(rgfi(11), 32);
    rflags = Mk_rflagsbits(logior(logand(r11_var, 0x003c7fd7), 0x00000002));
    let star : bits(64) = read_msr(4);
    let new_cs_selector : int = unsigned(star[63 .. 48]) + 16;
    if not_bool(fits_in_signed_bitvector(16, new_cs_selector)) then x86_model_error(":NEW-CS-SELECTOR-TOO-LARGE");
    let new_cs_selector : segment_selectorbits = [Mk_segment_selectorbits(bits_of_int(new_cs_selector, 16)) with rpl = 0b11];
    seg_visibles[1] = new_cs_selector.bits;
    let cs_base_addr : {|0|} = 0;
    let cs_limit : {|4294967295|} = 4294967295;
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    let cs_attr : code_segment_descriptor_attributesbits = [Mk_code_segment_descriptor_attributesbits(cs_attr) with a = 0b1, r = 0b1, c = 0b0, msb_of_type = 0b1, s = 0b1, dpl = 0b11, p = 0b1, l = 0b1, d = 0b0, g = 0b1];
    seg_hidden_bases[1] = bits_of_int(cs_base_addr, 64);
    seg_hidden_limits[1] = bits_of_int(cs_limit, 32);
    seg_hidden_attrs[1] = cs_attr.bits;
    let current_cs_register : segment_selectorbits = [Mk_segment_selectorbits(current_cs_register) with rpl = 0b00];
    seg_visibles[1] = current_cs_register.bits;
    let new_ss_selector : int = unsigned(star[63 .. 48]) + 8;
    if not_bool(fits_in_signed_bitvector(16, new_ss_selector)) then x86_model_error(":NEW-SS-SELECTOR-TOO-LARGE");
    let new_ss_selector : segment_selectorbits = [Mk_segment_selectorbits(bits_of_int(new_ss_selector, 16)) with rpl = 0b11];
    seg_visibles[2] = new_ss_selector.bits;
    let ss_base_addr : {|0|} = 0;
    let ss_limit : {|4294967295|} = 4294967295;
    let ss_attr : bits(16) = seg_hidden_attrs[2];
    let ss_attr : data_segment_descriptor_attributesbits = [Mk_data_segment_descriptor_attributesbits(ss_attr) with a = 0b1, w = 0b1, e = 0b0, msb_of_type = 0b0, s = 0b1, dpl = 0b11, p = 0b1, d_b = 0b1, g = 0b1];
    seg_hidden_bases[2] = bits_of_int(ss_base_addr, 64);
    seg_hidden_limits[2] = bits_of_int(ss_limit, 32);
    seg_hidden_attrs[2] = ss_attr.bits
}
