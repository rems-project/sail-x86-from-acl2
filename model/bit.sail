
val x86_bt_0f_a3 : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_bt_0f_a3 (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-BT-0F-A3";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let bitoffset : bits(64) = rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte);
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            let p4? : bool = 0x67 == prefixes[adr];
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)
        }
    };
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let (bitoffset, bitbase) : (int, bits(128)) = {
        if mod_var == 0b11 then {
            (mod(unsigned(bitoffset), ash(operand_size, 3)), sail_zero_extend(rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), rex_byte), 128))
        } else {
            let bitoffset_int : sbits(64) = {
                match operand_size {
                  1 => sail_sign_extend(truncate(bitoffset, 8), 64),
                  2 => sail_sign_extend(truncate(bitoffset, 16), 64),
                  4 => sail_sign_extend(truncate(bitoffset, 32), 64),
                  _ => bitoffset
                }
            };
            let bitoffset_int_abs : int = abs(signed(bitoffset_int));
            let bitnumber : int = mod(bitoffset_int_abs, 8);
            let byte_addr : int = signed(addr) + floor2(signed(bitoffset_int), 8);
            let inst_ac? : bool = alignment_checking_enabled_p();
            let byte : bits(128) = {
                if signed_byte_p(64, byte_addr) then {
                    rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, bits_of_int(byte_addr, 64), None(), seg_reg, ":R", inst_ac?, false)
                } else {
                    x86_model_error("EFFECTIVE-ADDRESS-ERROR")
                }
            };
            (bitnumber, byte)
        }
    };
    let rflags_var : rflagsbits = rflags in
    let new_rflags : rflagsbits = [rflags_var with cf = logbit(bitoffset, bitbase)] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with pf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with af = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with sf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with of = sailval] in rflags = new_rflags;
    write_iptr(proc_mode, temp_rip)
}

val x86_bt_0f_ba : (proc_mode, sbits(64), sbits(64), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_bt_0f_ba (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-BT-0F-BA";
    let r_m : bits(3) = modr_m[r_m];
    let mod_var : bits(2) = modr_m[mod];
    let reg : bits(3) = modr_m[reg];
    let operand_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let p2 : bits(8) = prefixes[seg];
    let p4? : bool = 0x67 == prefixes[adr];
    let seg_reg : seg_reg_idx = select_segment_register(proc_mode, prefixes, mod_var, r_m, sib);
    let inst_ac? : bool = true;
    let (bitbase, increment_rip_by, addr) : (bits(128), {|0, 1, 2, 4|}, sbits(64)) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001);
    let increment_rip_by : bits(3) = bits_of_int(increment_rip_by, 3);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 64));
    let bitoffset : bits(8) = truncate(rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, temp_rip, None(), 1, ":X", false, false), 8);
    let temp_rip : sbits(64) = add_to_iptr(proc_mode, temp_rip, 0x0000000000000001);
    let badlength? : option(int) = check_instruction_length(start_rip, temp_rip, 0b000);
    if is_some(badlength?) then x86_fault("(!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)");
    let bitoffset : range(0, 64) = check_range(0, 64, mod(unsigned(bitoffset), ash(operand_size, 3)));
    let rflags_var : rflagsbits = rflags in
    let new_rflags : rflagsbits = [rflags_var with cf = logbit(bitoffset, bitbase)] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with pf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with af = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with sf = sailval] in rflags = new_rflags;
    let rflags_var : rflagsbits = rflags in
    let sailval : bits(1) = undef_flg() in
    let new_rflags : rflagsbits = [rflags_var with of = sailval] in rflags = new_rflags;
    write_iptr(proc_mode, temp_rip)
}
