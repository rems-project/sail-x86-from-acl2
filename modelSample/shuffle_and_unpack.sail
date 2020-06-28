$include <prelude.sail>
$include <string.sail>

val extract_32_bits : (int, int) -> int
function extract_32_bits (x, n) =
(match n {
0 => unsigned(get_slice_int((31) - (0), x, 0)),
1 => unsigned(get_slice_int((63) - (32), x, 32)),
2 => unsigned(get_slice_int((95) - (64), x, 64)),
_ => unsigned(get_slice_int((127) - (96), x, 96))
}) : int

val extract_64_bits : (int, int) -> int
function extract_64_bits (x, n) =
(match n {
0 => unsigned(get_slice_int((63) - (0), x, 0)),
_ => unsigned(get_slice_int((127) - (64), x, 64))
}) : int

/*Shuffle packed single-precision floating-point values*/
/*<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 0F C6: SHUFPS xmm1, xmm2/m128, imm8<br/>*/
val x86_shufps_op_en_rmi : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-SHUFPS-OP/EN-RMI") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, imm, x86) = (rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let imm0 = (unsigned(get_slice_int((1) - (0), imm, 0))) : int in
let imm1 = (unsigned(get_slice_int((3) - (2), imm, 2))) : int in
let imm2 = (unsigned(get_slice_int((5) - (4), imm, 4))) : int in
let imm3 = (unsigned(get_slice_int((7) - (6), imm, 6))) : int in
let dword0 = (extract_32_bits(xmm, imm0)) : int in
let dword1 = (extract_32_bits(xmm, imm1)) : int in
let dword2 = (extract_32_bits(xmm_mem, imm2)) : int in
let dword3 = (extract_32_bits(xmm_mem, imm3)) : int in
let result = (merge_4_u32s(dword3, dword2, dword1, dword0)) : int in
let x86 = (write_xmmi_size(16, xmm_index, result, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Shuffle packed double-precision floating-point values*/
/*<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 66 0F C6: SHUFPD xmm1, xmm2/m128, imm8<br/>*/
val x86_shufpd_op_en_rmi : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-SHUFPD-OP/EN-RMI") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 1, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, imm, x86) = (rme_size(proc_mode, 1, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let imm0 = (logbit(0, imm)) : int in
let imm1 = (logbit(1, imm)) : int in
let qword0 = (extract_64_bits(xmm, imm0)) : int in
let qword1 = (extract_64_bits(xmm_mem, imm1)) : int in
let result = (merge_2_u64s(qword1, qword0)) : int in
let x86 = (write_xmmi_size(16, xmm_index, result, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Unpack and interleave low/high packed single-precision floating-point 
 values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 14: UNPCKLPS xmm1, xmm2/m128<br/> 
 0F 15: UNPCKHPS xmm1, xmm2/m128<br/>*/
val x86_unpck?ps_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-UNPCK?PS-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let dword0 = (if (high_low) == (1)
then unsigned(get_slice_int((95) - (64), xmm, 64))
else unsigned(get_slice_int((31) - (0), xmm, 0))) : int in
let dword1 = (if (high_low) == (1)
then unsigned(get_slice_int((95) - (64), xmm_mem, 64))
else unsigned(get_slice_int((31) - (0), xmm_mem, 0))) : int in
let dword2 = (if (high_low) == (1)
then unsigned(get_slice_int((127) - (96), xmm, 96))
else unsigned(get_slice_int((63) - (32), xmm, 32))) : int in
let dword3 = (if (high_low) == (1)
then unsigned(get_slice_int((127) - (96), xmm_mem, 96))
else unsigned(get_slice_int((63) - (32), xmm_mem, 32))) : int in
let result = (merge_4_u32s(dword3, dword2, dword1, dword0)) : int in
let x86 = (write_xmmi_size(16, xmm_index, result, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Unpack and interleave low/high packed double-precision floating-point 
 values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 66 0F 14: UNPCKLPD xmm1, xmm2/m128<br/> 
 66 0F 15: UNPCKHPD xmm1, xmm2/m128<br/>*/
val x86_unpck?pd_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, rmem, escape}
function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-UNPCK?PD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let qword0 = (if (high_low) == (1)
then unsigned(get_slice_int((127) - (64), xmm, 64))
else unsigned(get_slice_int((63) - (0), xmm, 0))) : int in
let qword1 = (if (high_low) == (1)
then unsigned(get_slice_int((127) - (64), xmm_mem, 64))
else unsigned(get_slice_int((63) - (0), xmm_mem, 0))) : int in
let result = (merge_2_u64s(qword1, qword0)) : int in
let x86 = (write_xmmi_size(16, xmm_index, result, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

