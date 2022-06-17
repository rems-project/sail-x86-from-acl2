$include <prelude.sail>
$include <string.sail>

val x86_mov_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-MR") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((opcode) == (0x88)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let register_var : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let inst_ac? : bool = (true) in
let flg2 : option(string) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(register_var), seg_reg, addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_mov_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-RM") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((opcode) == (0x8a)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), reg_mem, rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}

val x86_mov_op_en_fd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_fd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-FD") in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((opcode) == (0xa0)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let offset_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let (flg, offset) : (option(string), sbits(64)) = (rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, 0b00, 0b000, sib)) in
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let (flg, data) : (option(string), int) = (rme_size(proc_mode, operand_size, offset, seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
write_rgfi_size(bits_of_int(operand_size, 4), 0x0, data, rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_op_en_td : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_td (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-TD") in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((opcode) == (0xa2)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let offset_size : {|2, 4, 8|} = (select_address_size(proc_mode, p4?)) in
let (flg, offset) : (option(string), sbits(64)) = (rime_size(proc_mode, offset_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(offset_size, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, 0b00, 0b000, sib)) in
let data : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), 0x0, rex_byte)) in
let inst_ac? : bool = (true) in
let flg : option(string) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(data), seg_reg, offset, rex_byte, 0b000, 0b00)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_op_en_oi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_oi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-OI") in
let byte_operand? : bool = (((176) <= (unsigned(opcode))) & ((unsigned(opcode)) <= (183))) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let (flg0, imm) : (option(string), int) = (rme_size(proc_mode, operand_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let reg : bits(3) = (truncate(logand(0x07, opcode), 3)) in
{ write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b00), imm, rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_mov_op_en_mi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-MI") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let byte_operand? : bool = ((opcode) == (0xc6)) in
let imm_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false)) in
let reg_mem_size : {|1, 2, 4, 8|} = (if ((opcode) == (0xc7)) & (logbitp(3, rex_byte))
then 8
else imm_size) in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3))) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg2, imm) : (option(string), int) = (rme_size(proc_mode, imm_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
{ if is_some(flg2)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(imm_size, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let imm : int = (if (reg_mem_size) == (8)
then unsigned(n64(sail_zero_extend(n32_to_i32(bits_of_int(imm, 32)), 64)))
else imm) in
let inst_ac? : bool = (true) in
let flg3 : option(string) = (x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, nat_of_int(imm), seg_reg, addr, rex_byte, r_m, mod_var)) in
{ if is_some(flg3)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, temp_rip)
}
}
}
}
}
}

val x86_lea : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_lea (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LEA") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let register_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) in
let (flg0, m, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let m : bits(64) = (sail_mask(64, trunc(register_size, m))) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(m), rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_movsx : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movsx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVSX") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, true, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let register_size : {|1, 2, 4, 8|} = (if logbitp(3, rex_byte)
then 8
else reg_mem_size) in
let reg_mem : int = (if (register_size) == (8)
then unsigned(n64(sail_zero_extend(n32_to_i32(bits_of_int(reg_mem, 32)), 64)))
else reg_mem) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_movsxd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movsxd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVSXD") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let reg_mem_size : {|1, 2|} = (if (opcode) == (0xbe)
then 1
else 2) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let register_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let reg_mem : int = (match reg_mem_size {
1 => unsigned(slice(n08_to_i08(bits_of_int(reg_mem, 8)), 0, ash(register_size, 3))),
2 => (match register_size {
2 => reg_mem,
_ => unsigned(slice(n16_to_i16(bits_of_int(reg_mem, 16)), 0, ash(register_size, 3)))
}) : int
}) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_movzx : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movzx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVZX") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let p2 : bits(8) = (prefixes_get_seg(prefixes)) in
let p4? : bool = ((0x67) == (prefixes_get_adr(prefixes))) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let reg_mem_size : {|1, 2|} = (if (opcode) == (0xb6)
then 1
else 2) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), int, {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_sign_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else ();
let register_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_control_regs_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, undef, wreg}
function x86_mov_control_regs_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-CONTROL-REGS-OP/EN-MR") in
let r_m : bits(3) = (modr_m_get_r_m(modr_m)) in
let mod_var : bits(2) = (modr_m_get_mod(modr_m)) in
let reg : bits(3) = (modr_m_get_reg(modr_m)) in
let ctr_index : bits(4) = (if (logbitp(2, rex_byte)) & ((reg) == (0b000))
then 0x8
else sail_zero_extend(reg, 4)) in
let operand_size : {|4, 8|} = (if (proc_mode) == (0)
then 8
else 4) in
let ctr_val : bits(64) = (ctrs[unsigned(ctr_index)]) in
let ctr_val : bits(64) = (if (operand_size) == (4)
then sail_zero_extend(n32(truncate(ctr_val, 32)), 64)
else ctr_val) in
{ write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), unsigned(ctr_val), rex_byte);
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_cf(sailval, rflags_var)) in
rflags = new_rflags;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_pf(sailval, rflags_var)) in
rflags = new_rflags;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_af(sailval, rflags_var)) in
rflags = new_rflags;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_zf(sailval, rflags_var)) in
rflags = new_rflags;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_sf(sailval, rflags_var)) in
rflags = new_rflags;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : bits(32) = (set_rflagsbits_get_of(sailval, rflags_var)) in
rflags = new_rflags;
write_iptr(proc_mode, temp_rip)
}

