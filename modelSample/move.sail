$include <prelude.sail>
$include <string.sail>

val x86_mov_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, wmv, rmem, eamem, escape}
function x86_mov_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (136)) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let register_var = (rgfi_size(operand_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg2, x86) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, register_var, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_mov_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (138)) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let x86 = (write_rgfi_size(operand_size, reg_index(reg, rex_byte, 2), reg_mem, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_op_en_fd : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_mov_op_en_fd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-FD") : string in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (160)) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let offset_size = (select_address_size(proc_mode, p4?, 0)) : int in
let (flg, offset, x86) = (rime_size(proc_mode, offset_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, offset_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, 0, 0, sib, 0)) : int in
let inst_ac? = (alignment_checking_enabled_p(0)) : bool in
let (flg, data, x86) = (rme_size(proc_mode, operand_size, offset, seg_reg, ":R", inst_ac?, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let x86 = (write_rgfi_size(operand_size, 0, data, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_op_en_td : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, wmv, rmem, eamem, escape}
function x86_mov_op_en_td (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-TD") : string in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (162)) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let offset_size = (select_address_size(proc_mode, p4?, 0)) : int in
let (flg, offset, x86) = (rime_size(proc_mode, offset_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, offset_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, 0, 0, sib, 0)) : int in
let data = (rgfi_size(operand_size, 0, rex_byte, 0)) : int in
let inst_ac? = (true) : bool in
let (flg, x86) = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, data, seg_reg, offset, rex_byte, 0, 0, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_op_en_oi : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_mov_op_en_oi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-OI") : string in
let byte_operand? = (((176) <= (opcode)) & ((opcode) <= (183))) : bool in
let operand_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false, 0)) : int in
let (flg0, imm, x86) = (rme_size(proc_mode, operand_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, operand_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let reg = (binary_logand(7, opcode)) : int in
let x86 = (write_rgfi_size(operand_size, reg_index(reg, rex_byte, 0), imm, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_op_en_mi : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, wmv, rmem, eamem, escape}
function x86_mov_op_en_mi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-OP/EN-MI") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = ((opcode) == (198)) : bool in
let imm_size = (select_operand_size(proc_mode, byte_operand?, rex_byte, true, prefixes, false, false, false, 0)) : int in
let reg_mem_size = (if ((opcode) == (199)) & (logbitp(3, rex_byte))
then 8
else imm_size) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, imm_size, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg2, imm, x86) = (rme_size(proc_mode, imm_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg2)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, imm_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let imm = (if (reg_mem_size) == (8)
then n64(n32_to_i32(imm))
else imm) : int in
let inst_ac? = (true) : bool in
let (flg3, x86) = (x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, imm, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg3)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_lea : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_lea (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-LEA") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let register_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let (flg0, m, increment_rip_by, x86) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let m = (trunc(register_size, m)) : int in
let x86 = (write_rgfi_size(register_size, reg_index(reg, rex_byte, 2), m, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_movsx : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_movsx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVSX") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let reg_mem_size = (select_operand_size(proc_mode, false, rex_byte, true, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (if logbitp(3, rex_byte)
then 8
else reg_mem_size) : int in
let reg_mem = (if (register_size) == (8)
then n64(n32_to_i32(reg_mem))
else reg_mem) : int in
let x86 = (write_rgfi_size(register_size, reg_index(reg, rex_byte, 2), reg_mem, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_movsxd : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_movsxd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVSXD") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let reg_mem_size = (if (opcode) == (190)
then 1
else 2) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let reg_mem = ((match reg_mem_size {
1 => unsigned(get_slice_int(ash(register_size, 3), n08_to_i08(reg_mem), 0)),
2 => (match register_size {
2 => reg_mem,
_ => unsigned(get_slice_int(ash(register_size, 3), n16_to_i16(reg_mem), 0))
}) : int
}) : int) : int in
let x86 = (write_rgfi_size(register_size, reg_index(reg, rex_byte, 2), reg_mem, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_movzx : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_movzx (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVZX") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let reg_mem_size = (if (opcode) == (182)
then 1
else 2) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let register_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let x86 = (write_rgfi_size(register_size, reg_index(reg, rex_byte, 2), reg_mem, rex_byte, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

val x86_mov_control_regs_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, escape, undef}
function x86_mov_control_regs_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOV-CONTROL-REGS-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let ctr_index = (if (logbitp(2, rex_byte)) & ((reg) == (0))
then 8
else reg) : int in
let operand_size = (if (proc_mode) == (0)
then 8
else 4) : int in
let ctr_val = (ctri(ctr_index, 0)) : int in
let ctr_val = (if (operand_size) == (4)
then n32(ctr_val)
else ctr_val) : int in
let x86 = (write_rgfi_size(operand_size, reg_index(r_m, rex_byte, 0), ctr_val, rex_byte, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_cf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_zf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

