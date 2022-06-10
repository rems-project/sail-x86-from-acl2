$include "./multiply_spec.sail"

val x86_mul : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_mul (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-MUL" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == get_slice_int(8, 246, 0)) : bool;
    let reg_mem_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
                let (product_high, product_low, product) = mul_spec(reg_mem_size, rax_var, get_slice_int(64, reg_mem, 0)) : (bits(64), bits(64), bits(128));
                (match reg_mem_size {
                  1 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 0, 0), unsigned(product), rex_byte),
                  _ => {
                      write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), unsigned(product_low), rex_byte);
                      write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), unsigned(product_high), rex_byte)
                  }
                }) : unit;
                if the_bits(8, product_high) == get_slice_int(8, 0, 0) then {
                    {
                        let rflags_var = rflags : bits(32) in
                        let new_rflags = set_rflagsbits_get_cf(get_slice_int(1, 0, 0), rflags_var) : bits(32) in
                          rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32);
                        let new_rflags = set_rflagsbits_get_of(get_slice_int(1, 0, 0), rflags_var) : bits(32);
                        rflags = new_rflags
                    }
                } else {
                    {
                        let rflags_var = rflags : bits(32) in
                        let new_rflags = set_rflagsbits_get_cf(get_slice_int(1, 1, 0), rflags_var) : bits(32) in
                          rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32) in
                        let sailval = undef_flg() : bits(1) in
                        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                        let rflags_var = rflags : bits(32);
                        let new_rflags = set_rflagsbits_get_of(get_slice_int(1, 1, 0), rflags_var) : bits(32);
                        rflags = new_rflags
                    }
                };
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}

val x86_imul_op_en_m : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_imul_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-IMUL-OP/EN-M" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == get_slice_int(8, 246, 0)) : bool;
    let reg_mem_size = the_range(1, 8, select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
                let (product_high, product_low, product, cf_and_of) = imul_spec(reg_mem_size, rax_var, get_slice_int(64, reg_mem, 0)) : (bits(64), bits(64), bits(128), bits(1));
                let cf_and_of = cf_and_of : bits(1);
                (match reg_mem_size {
                  1 => write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 0, 0), unsigned(product), rex_byte),
                  _ => {
                      write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), unsigned(product_low), rex_byte);
                      write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), unsigned(product_high), rex_byte)
                  }
                }) : unit;
                {
                    let rflags_var = rflags : bits(32) in
                    let new_rflags = set_rflagsbits_get_cf(cf_and_of, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32);
                    let new_rflags = set_rflagsbits_get_of(cf_and_of, rflags_var) : bits(32);
                    rflags = new_rflags
                };
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}

val x86_imul_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_imul_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-IMUL-OP/EN-RM" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let reg_mem_size = the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let register_var = rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte) : bits(64);
                let (product_high, product_low, product, cf_and_of) = imul_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), register_var) : (bits(64), bits(64), bits(128), bits(1));
                let cf_and_of = cf_and_of : bits(1);
                write_rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), unsigned(product_low), rex_byte);
                {
                    let rflags_var = rflags : bits(32) in
                    let new_rflags = set_rflagsbits_get_cf(cf_and_of, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    let rflags_var = rflags : bits(32);
                    let new_rflags = set_rflagsbits_get_of(cf_and_of, rflags_var) : bits(32);
                    rflags = new_rflags
                };
                write_iptr(proc_mode, temp_rip)
            }
        }
    }
}

val x86_imul_op_en_rmi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_imul_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-IMUL-OP/EN-RMI" : string;
    let r_m = modr_m_get_r_m(modr_m) : bits(3);
    let mod_var = modr_m_get_mod(modr_m) : bits(2);
    let reg = modr_m_get_reg(modr_m) : bits(3);
    let p2 = prefixes_get_seg(prefixes) : bits(8);
    let p4? = (get_slice_int(8, 103, 0) == prefixes_get_adr(prefixes)) : bool;
    let reg_mem_size = the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let imm_size = the_range(1, 4, if opcode == get_slice_int(8, 107, 0) then 1 else if get_slice_int(8, reg_mem_size, 0) == get_slice_int(8, 2, 0) then 2 else 4) : {|1, 2, 4|};
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib) : {|0, 1, 2, 3, 4, 5|};
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, imm_size, 0)) : (option(string), int, {|0, 1, 2, 4|}, sbits(66));
    let increment_rip_by = the_bits(3, increment_rip_by) : bits(3);
    let addr = the_sbits(64, addr) : sbits(64);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let (flg1, imm) = rme_size(proc_mode, imm_size, the_sbits(64, temp_rip), 1, ":X", false, struct { mem_ptr? = false }) : (option(string), int);
            let imm = the_bits(32, imm) : bits(32);
            if is_some(flg1) then {
                throw(Emsg("Model state error: :RME-SIZE-ERROR"))
            } else {
                let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, get_slice_int(48, imm_size, 0)) : (option(string), sbits(52));
                let temp_rip = the_sbits(48, temp_rip) : sbits(48);
                if is_some(flg) then {
                    throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
                } else {
                    let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
                    if is_some(badlength?) then {
                        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
                    } else {
                        let (product_high, product_low, product, cf_and_of) = imul_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), the_bits(64, imm)) : (bits(64), bits(64), bits(128), bits(1));
                        let cf_and_of = cf_and_of : bits(1);
                        write_rgfi_size(get_slice_int(4, reg_mem_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), unsigned(product_low), rex_byte);
                        {
                            let rflags_var = rflags : bits(32) in
                            let new_rflags = set_rflagsbits_get_cf(cf_and_of, rflags_var) : bits(32) in
                              rflags = new_rflags;
                            let rflags_var = rflags : bits(32) in
                            let sailval = undef_flg() : bits(1) in
                            let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32) in
                              rflags = new_rflags;
                            let rflags_var = rflags : bits(32) in
                            let sailval = undef_flg() : bits(1) in
                            let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32) in
                              rflags = new_rflags;
                            let rflags_var = rflags : bits(32) in
                            let sailval = undef_flg() : bits(1) in
                            let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32) in
                              rflags = new_rflags;
                            let rflags_var = rflags : bits(32) in
                            let sailval = undef_flg() : bits(1) in
                            let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32) in
                              rflags = new_rflags;
                            let rflags_var = rflags : bits(32);
                            let new_rflags = set_rflagsbits_get_of(cf_and_of, rflags_var) : bits(32);
                            rflags = new_rflags
                        };
                        write_iptr(proc_mode, temp_rip)
                    }
                }
            }
        }
    }
}
