$include <prelude.sail>
$include <string.sail>

val x86_mov_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (get_slice_int(8, 136, 0))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let register_var = (rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)) : bits(64) in
let (flg0, addr, increment_rip_by) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg2 = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(register_var), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, the_sbits(48, temp_rip))

val x86_mov_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (get_slice_int(8, 138, 0))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else { write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), reg_mem, rex_byte);
write_iptr(proc_mode, the_sbits(48, temp_rip))
}

val x86_mov_op_en_fd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_fd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-FD") : string in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (get_slice_int(8, 160, 0))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let offset_size = (the_range(1, 8, select_address_size(proc_mode, p4?))) : {|8, 2, 4|} in
let (flg, offset) = (rime_size(proc_mode, offset_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, offset_size, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, get_slice_int(2, 0, 0), get_slice_int(3, 0, 0), sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (alignment_checking_enabled_p()) : bool in
let (flg, data) = (rme_size(proc_mode, operand_size, the_sbits(64, offset), seg_reg, ":R", inst_ac?, struct {mem_ptr? = false})) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else { write_rgfi_size(get_slice_int(4, operand_size, 0), get_slice_int(4, 0, 0), data, rex_byte);
write_iptr(proc_mode, temp_rip)
}

val x86_mov_op_en_td : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_td (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-TD") : string in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (get_slice_int(8, 162, 0))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let offset_size = (the_range(1, 8, select_address_size(proc_mode, p4?))) : {|8, 2, 4|} in
let (flg, offset) = (rime_size(proc_mode, offset_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, offset_size, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, get_slice_int(2, 0, 0), get_slice_int(3, 0, 0), sib)) : {|0, 1, 2, 3, 4, 5|} in
let data = (rgfi_size(get_slice_int(4, operand_size, 0), get_slice_int(4, 0, 0), rex_byte)) : bits(64) in
let inst_ac? = (true) : bool in
let flg = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(data), seg_reg, the_sbits(64, offset), rex_byte, get_slice_int(3, 0, 0), get_slice_int(2, 0, 0))) : option(string) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, temp_rip)

val x86_mov_op_en_oi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_mov_op_en_oi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-OI") : string in
let byte_operand? = (((176) <= (unsigned(opcode))) & ((unsigned(opcode)) <= (183))) : bool in
let operand_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let (flg0, imm) = (rme_size(proc_mode, operand_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), int) in
if is_some(flg0)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, operand_size, 0))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let reg = (the_bits(3, logand(get_slice_int(8, 7, 0), opcode))) : bits(3) in
{ write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 0, 0)), imm, rex_byte);
write_iptr(proc_mode, the_sbits(48, temp_rip))
}

val x86_mov_op_en_mi : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_mov_op_en_mi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-OP/EN-MI") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (get_slice_int(8, 198, 0))) : bool in
let imm_size = (the_range(1, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let reg_mem_size = (the_range(1, 8, if ((opcode) == (get_slice_int(8, 199, 0))) & (logbitp(3, rex_byte))
then 8
else imm_size)) : {|8, 1, 2, 4|} in
let (flg0, addr, increment_rip_by) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, imm_size, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg2, imm) = (rme_size(proc_mode, imm_size, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), int) in
if is_some(flg2)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, imm_size, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let imm = (if (get_slice_int(8, reg_mem_size, 0)) == (get_slice_int(8, 8, 0))
then unsigned(n64(the_bits(64, n32_to_i32(get_slice_int(32, imm, 0)))))
else imm) : int in
let inst_ac? = (true) : bool in
let flg3 = (x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, the_nat(imm), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg3)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, temp_rip)

val x86_lea : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_lea (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-LEA") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let register_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 2, 4|} in
let (flg0, m, increment_rip_by) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
let m = (the_sbits(64, m)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let m = (sail_mask(64, trunc(register_size, m))) : bits(64) in
{ write_rgfi_size(get_slice_int(4, register_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), unsigned(m), rex_byte);
write_iptr(proc_mode, the_sbits(48, temp_rip))
}

val x86_movsx : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movsx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVSX") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let reg_mem_size = (the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, true, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (if logbitp(3, rex_byte)
then 8
else reg_mem_size) : {|8, 1, 2, 4|} in
let reg_mem = (if (get_slice_int(8, register_size, 0)) == (get_slice_int(8, 8, 0))
then unsigned(n64(the_bits(64, n32_to_i32(get_slice_int(32, reg_mem, 0)))))
else reg_mem) : int in
{ write_rgfi_size(get_slice_int(4, register_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}

val x86_movsxd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movsxd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVSXD") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let reg_mem_size = (if (opcode) == (get_slice_int(8, 190, 0))
then 1
else 2) : {|1, 2|} in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|} in
let reg_mem = ((match reg_mem_size {
1 => unsigned(slice(n08_to_i08(get_slice_int(8, reg_mem, 0)), 0, ash(register_size, 3))),
2 => (match register_size {
2 => reg_mem,
_ => unsigned(slice(n16_to_i16(get_slice_int(16, reg_mem, 0)), 0, ash(register_size, 3)))
}) : int
}) : int) : int in
{ write_rgfi_size(get_slice_int(4, register_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}

val x86_movzx : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg, wreg}
function x86_movzx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVZX") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let reg_mem_size = (if (opcode) == (get_slice_int(8, 182, 0))
then 1
else 2) : {|1, 2|} in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
{ write_rgfi_size(get_slice_int(4, register_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), reg_mem, rex_byte);
write_iptr(proc_mode, temp_rip)
}

val x86_mov_control_regs_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rreg, undef, wreg}
function x86_mov_control_regs_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOV-CONTROL-REGS-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let ctr_index = (if (logbitp(2, rex_byte)) & ((the_bits(8, reg)) == (get_slice_int(8, 0, 0)))
then get_slice_int(4, 8, 0)
else the_bits(4, reg)) : bits(4) in
let operand_size = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then 8
else 4) : {|8, 4|} in
let ctr_val = (ctrs[unsigned(ctr_index)]) : bits(64) in
let ctr_val = (if (get_slice_int(8, operand_size, 0)) == (get_slice_int(8, 4, 0))
then the_bits(64, n32(the_bits(32, ctr_val)))
else ctr_val) : bits(64) in
{ write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), unsigned(ctr_val), rex_byte);
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_cf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_zf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
let rflags_var = (rflags) : bits(32) in
let sailval = (undef_flg()) : bits(1) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : bits(32) in
rflags = new_rflags;
write_iptr(proc_mode, temp_rip)
}

