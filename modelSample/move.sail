$ifndef __X86_MOVE
$define __X86_MOVE
$include <prelude.sail>
$include <string.sail>

val x86_mov_op_en_mr : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-MR") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let byte_operand? : bool = ((opcode) == (0x88)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let register_var : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = { (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) } in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let inst_ac? : bool = (true) in
let flg2 : option(string) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(register_var, 128), seg_reg, addr, prefixes, rex_byte, r_m, mod_var)) in
{ if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_mov_op_en_rm : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-RM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let byte_operand? : bool = ((opcode) == (0x8a)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem), rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}

val x86_mov_op_en_fd : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_fd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-FD") in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let byte_operand? : bool = ((opcode) == (0xa0)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let offset_size : address_size = (select_address_size(proc_mode, Some(prefixes))) in
let (flg, offset) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, 0b00, 0b000, sib)) in
let inst_ac? : bool = (alignment_checking_enabled_p()) in
let (flg, data) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, offset, None(), seg_reg, ":R", inst_ac?, false)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else ();
write_rgfi_size(bits_of_int(operand_size, 4), 0x0, unsigned(data), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_op_en_td : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_td (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-TD") in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let byte_operand? : bool = ((opcode) == (0xa2)) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let offset_size : address_size = (select_address_size(proc_mode, Some(prefixes))) in
let (flg, offset) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), offset_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, 0b00, 0b000, sib)) in
let data : bits(64) = (rgfi_size(bits_of_int(operand_size, 4), 0x0, rex_byte)) in
let inst_ac? : bool = (true) in
let flg : option(string) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, sail_zero_extend(data, 128), seg_reg, offset, prefixes, rex_byte, 0b000, 0b00)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_op_en_oi : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_oi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-OI") in
let byte_operand? : bool = (((176) <= (unsigned(opcode))) & ((unsigned(opcode)) <= (183))) in
let operand_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false)) in
let (flg0, imm) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), operand_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(operand_size, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let reg : bits(3) = (truncate(logand(0x07, opcode), 3)) in
{ write_rgfi_size(bits_of_int(operand_size, 4), reg_index(reg, rex_byte, 0b00), unsigned(imm), rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_mov_op_en_mi : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-OP/EN-MI") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let byte_operand? : bool = ((opcode) == (0xc6)) in
let imm_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false)) in
let reg_mem_size : {|1, 2, 4, 8|} = { (if ((opcode) == (0xc7)) & (logbitp(3, rex_byte))
then 8
else imm_size) } in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = { (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3))) } in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg2, imm) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), imm_size, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let imm : bits(128) = { (if (reg_mem_size) == (8)
then sail_zero_extend(sail_sign_extend(truncate(imm, 32), 64), 128)
else imm) } in
let inst_ac? : bool = (true) in
let flg3 : option(string) = (x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, imm, seg_reg, addr, prefixes, rex_byte, r_m, mod_var)) in
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

val x86_lea : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape, rreg, wreg}
function x86_lea (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-LEA") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let register_size : {|2, 4, 8|} = (check_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) in
let (flg0, m, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = (x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let m : bits(64) = (sail_mask(64, trunc(register_size, m))) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(m), rex_byte);
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

val x86_movsx : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movsx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVSX") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, true, prefixes, false, false, false)) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let register_size : {|1, 2, 4, 8|} = { (if logbitp(3, rex_byte)
then 8
else reg_mem_size) } in
let reg_mem : bits(128) = { (if (register_size) == (8)
then sail_zero_extend(sail_sign_extend(truncate(reg_mem, 32), 64), 128)
else reg_mem) } in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_movsxd : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movsxd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVSXD") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let reg_mem_size : {|1, 2|} = { (if (opcode) == (0xbe)
then 1
else 2) } in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let register_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let reg_mem : bits(128) = { (match reg_mem_size {
1 => sail_zero_extend(sail_mask(64, signed_bitslice(truncate(reg_mem, 8), 0, ash(register_size, 3))), 128),
2 => { (match register_size {
2 => reg_mem,
_ => sail_zero_extend(sail_mask(64, signed_bitslice(truncate(reg_mem, 16), 0, ash(register_size, 3))), 128)
}) : bits(128) }
}) } in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_movzx : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movzx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVZX") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let reg_mem_size : {|1, 2|} = { (if (opcode) == (0xb6)
then 1
else 2) } in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let register_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
{ write_rgfi_size(bits_of_int(register_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem), rex_byte);
write_iptr(proc_mode, temp_rip)
}
}
}
}

val x86_mov_control_regs_op_en_mr : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {escape, rreg, undef, wreg}
function x86_mov_control_regs_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOV-CONTROL-REGS-OP/EN-MR") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let ctr_index : bits(4) = { (if (logbitp(2, rex_byte)) & ((reg) == (0b000))
then 0x8
else sail_zero_extend(reg, 4)) } in
let operand_size : {|4, 8|} = { (if (proc_mode) == (0)
then 8
else 4) } in
let ctr_val : bits(64) = (ctrs[unsigned(ctr_index)]) in
let ctr_val : bits(64) = { (if (operand_size) == (4)
then sail_zero_extend(truncate(ctr_val, 32), 64)
else ctr_val) } in
{ write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), unsigned(ctr_val), rex_byte);
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with pf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with af = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with zf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with sf = (sailval)]) in
rflags = (new_rflags).bits;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (sailval)]) in
rflags = (new_rflags).bits;
write_iptr(proc_mode, temp_rip)
}

val x86_movd_movq_to_xmm : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movd_movq_to_xmm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVD/MOVQ-TO-XMM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let operand_size : {|4, 8|} = { (if logbitp(3, rex_byte)
then 8
else 4) } in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, operand_size, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
write_xmmi_size(bits_of_int(operand_size, 5), reg_index(reg, rex_byte, 0b10), unsigned(reg_mem));
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}

val x86_movd_movq_from_xmm : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movd_movq_from_xmm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MOVD/MOVQ-FROM-XMM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let operand_size : {|4, 8|} = { (if logbitp(3, rex_byte)
then 8
else 4) } in
let register_var : bits(128) = (xmmi_size(bits_of_int(operand_size, 5), reg_index(reg, rex_byte, 0b10))) in
let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = { (if (mod_var) == (0b11)
then (None() : option(string), 0x0000_0000_0000_0000, 0)
else x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) } in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else ();
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let inst_ac? : bool = (true) in
let flg2 : option(string) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, register_var, seg_reg, addr, prefixes, rex_byte, r_m, mod_var)) in
{ if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else ();
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

$endif
