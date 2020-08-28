$include "./divide_spec.sail"

val x86_div : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, undef, wreg}

function x86_div (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-DIV" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == 246) : bool;
    let reg_mem_size = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else if reg_mem == 0 then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT--ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(if select_byte_operand then 2 else {
                    reg_mem_size
                }, 0, rex_byte, 0) : int;
                let rdx_var = (if select_byte_operand then 0 else {
                    rgfi_size(reg_mem_size, 2, rex_byte, 0)
                }) : int;
                let dividend = (if select_byte_operand then {
                    rax_var
                } else {
                    changeBits(rax_var, ash(reg_mem_size, 3), ash(reg_mem_size, 4), rdx_var)
                }) : int;
                let (overflow?, quotient, remainder) = div_spec(reg_mem_size, dividend, reg_mem) : (bool, int, int);
                if overflow? then {
                    throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
                } else {
                    let x86 = (match reg_mem_size {
                      1 => let result = changeBits(quotient, 8, 8, remainder) : int in
                        let x86 = write_rgfi_size(2, 0, result, rex_byte, 0) : int in 0,
                      _ => let x86 = write_rgfi_size(reg_mem_size, 0, quotient, rex_byte, 0) : int in
                        let x86 = write_rgfi_size(reg_mem_size, 2, remainder, rex_byte, 0) : int in 0
                    }) : int : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}

val x86_idiv : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, undef, wreg}

function x86_idiv (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-IDIV" : string;
    let r_m = modr_m_get_r_m(modr_m) : int;
    let mod_var = modr_m_get_mod(modr_m) : int;
    let reg = modr_m_get_reg(modr_m) : int;
    let p2 = prefixes_get_seg(prefixes) : int;
    let p4? = (103 == prefixes_get_adr(prefixes)) : bool;
    let select_byte_operand = (opcode == 246) : bool;
    let reg_mem_size = select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0) : int;
    let seg_reg = select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0) : int;
    let inst_ac? = true : bool;
    let (flg0, reg_mem, increment_rip_by, addr, x86) = x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0) : (option(string), int, int, int, int);
    if is_some(flg0) then {
        throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
    } else if reg_mem == 0 then {
        throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
    } else {
        let (flg, temp_rip) = add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0) : (option(string), int);
        if is_some(flg) then {
            throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
        } else {
            let badlength? = check_instruction_length(start_rip, temp_rip, 0) : option(int);
            if is_some(badlength?) then {
                throw(Emsg("A fault occurred.  Original ACL2 AST:\n['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
            } else {
                let rax_var = rgfi_size(if select_byte_operand then 2 else {
                    reg_mem_size
                }, 0, rex_byte, 0) : int;
                let rdx_var = (if select_byte_operand then 0 else {
                    rgfi_size(reg_mem_size, 2, rex_byte, 0)
                }) : int;
                let dividend = (if select_byte_operand then {
                    rax_var
                } else {
                    changeBits(rax_var, ash(reg_mem_size, 3), ash(reg_mem_size, 4), rdx_var)
                }) : int;
                let (overflow?, quotient, remainder) = idiv_spec(reg_mem_size, dividend, reg_mem) : (bool, int, int);
                if overflow? then {
                    throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
                } else {
                    let x86 = (match reg_mem_size {
                      1 => let result = changeBits(quotient, 8, 8, remainder) : int in
                        let x86 = write_rgfi_size(2, 0, result, rex_byte, 0) : int in 0,
                      _ => let x86 = write_rgfi_size(reg_mem_size, 0, quotient, rex_byte, 0) : int in
                        let x86 = write_rgfi_size(reg_mem_size, 2, remainder, rex_byte, 0) : int in 0
                    }) : int : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = (let rflags_var = r_rflags(0) : int in
                      let (sailval, x86) = undef_flg(0) : (int, int) in
                        let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : int in write_rflags(new_rflags, 0)) : int;
                    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
                    0
                }
            }
        }
    }
}
