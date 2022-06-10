$include "./divide_spec.sail"

val x86_div : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_div (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-DIV" : string;
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
    } else if get_slice_int(8, reg_mem, 0) == get_slice_int(8, 0, 0) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(48, temp_rip) : sbits(48);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT--ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(get_slice_int(4, if select_byte_operand then 2 else {
                    reg_mem_size
                }, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
                let rdx_var = (if select_byte_operand then {
                    get_slice_int(64, 0, 0)
                } else {
                    rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), rex_byte)
                }) : bits(64);
                let dividend = (if select_byte_operand then {
                    unsigned(rax_var)
                } else {
                    changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))
                }) : int;
                let (overflow?, quotient, remainder) = div_spec(reg_mem_size, get_slice_int(128, dividend, 0), get_slice_int(64, reg_mem, 0)) : (bool, bits(128), bits(64));
                if overflow? then {
                    throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
                } else {
                    (match reg_mem_size {
                      1 => let result = changeSlice(quotient, 8, 8, the_bits(8, remainder)) : bits(128) in
                        write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 0, 0), unsigned(result), rex_byte),
                      _ => {
                          write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), unsigned(quotient), rex_byte);
                          write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), unsigned(remainder), rex_byte)
                      }
                    }) : unit;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
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
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    write_iptr(proc_mode, temp_rip)
                }
            }
        }
    }
}

val x86_idiv : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_idiv (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-IDIV" : string;
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
    } else if get_slice_int(8, reg_mem, 0) == get_slice_int(8, 0, 0) then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by)) : (option(string), sbits(52));
        let temp_rip = the_sbits(49, temp_rip) : sbits(49);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0)) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(get_slice_int(4, if select_byte_operand then 2 else {
                    reg_mem_size
                }, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
                let rdx_var = (if select_byte_operand then {
                    get_slice_int(64, 0, 0)
                } else {
                    rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), rex_byte)
                }) : bits(64);
                let dividend = (if select_byte_operand then {
                    unsigned(rax_var)
                } else {
                    changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))
                }) : int;
                let (overflow?, quotient, remainder) = idiv_spec(reg_mem_size, get_slice_int(128, dividend, 0), get_slice_int(64, reg_mem, 0)) : (bool, bits(64), bits(64));
                if overflow? then {
                    throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
                } else {
                    (match reg_mem_size {
                      1 => let result = changeSlice(quotient, 8, 8, the_bits(8, remainder)) : bits(64) in
                        write_rgfi_size(get_slice_int(4, 2, 0), get_slice_int(4, 0, 0), unsigned(result), rex_byte),
                      _ => {
                          write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 0, 0), unsigned(quotient), rex_byte);
                          write_rgfi_size(get_slice_int(4, reg_mem_size, 0), get_slice_int(4, 2, 0), unsigned(remainder), rex_byte)
                      }
                    }) : unit;
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : bits(32) in rflags = new_rflags;
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
                    let rflags_var = rflags : bits(32) in
                    let sailval = undef_flg() : bits(1) in
                    let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : bits(32) in rflags = new_rflags;
                    write_iptr(proc_mode, the_sbits(48, temp_rip))
                }
            }
        }
    }
}
