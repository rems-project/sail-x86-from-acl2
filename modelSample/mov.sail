$include <prelude.sail>
$include <string.sail>

/*Move scalar single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 F3 0F 10: MOVSS xmm1, xmm2/m32<br/> 
 F2 0F 10: MOVSD xmm1, xmm2/m64<br/>*/
val x86_movss_movsd_op_en_rm : (int, int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_movss_movsd_op_en_rm (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVSS/MOVSD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let operand_size = (if (sp_dp) == (1)
then 8
else 4) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let operand_size = (if (mod_var) == (3)
then operand_size
else 16) : int in
let x86 = (write_xmmi_size(operand_size, xmm_index, xmm_mem, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move scalar single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 F3 0F 11: MOVSS xmm2/m32, xmm1<br/> 
 F2 0F 11: MOVSD xmm2/m64, xmm1<br/>*/
val x86_movss_movsd_op_en_mr : (int, int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movss_movsd_op_en_mr (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVSS/MOVSD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let operand_size = (if (sp_dp) == (1)
then 8
else 4) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(operand_size, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg1, x86) = (x86_operand_to_xmm_mem(proc_mode, operand_size, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move aligned packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 28: MOVAPS xmm1, xmm2/m128<br/> 
 66 0F 28: MOVAPD xmm1, xmm2/m128<br/>*/
val x86_movaps_movapd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_movaps_movapd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVAPS/MOVAPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
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
else let x86 = (write_xmmi_size(16, xmm_index, xmm_mem, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move aligned packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 29: MOVAPS xmm2/m128, xmm1<br/> 
 66 0F 29: MOVAPD xmm2/m128, xmm1<br/>*/
val x86_movaps_movapd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movaps_movapd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVAPS/MOVAPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg1, x86) = (x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 10: MOVUPS xmm1, xmm2/m128<br/> 
 66 0F 10: MOVUPD xmm1, xmm2/m128<br/> 
 F3 0F 6F: MOVDQU xmm1, xmm2/m128<br/> 
 
 <p>Note: The MOVDQU, MOVUPS, and MOVUPD instructions perform 128-bit 
 unaligned loads or stores. They do not generate general-protection 
 exceptions ( #GP ) when operands are not aligned on a 16-byte 
 boundary. If alignment checking is enabled, alignment-check 
 exceptions ( #AC ) may or may not be generated depending on processor 
 implementation when data addresses are not aligned on an 8-byte 
 boundary.</p>*/
val x86_movups_movupd_movdqu_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_movups_movupd_movdqu_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
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
else let x86 = (write_xmmi_size(16, xmm_index, xmm_mem, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 11: MOVUPS xmm2/m128, xmm1<br/> 
 66 0F 11: MOVUPD xmm2/m128, xmm1<br/> 
 F3 0F 7F: MOVDQU xmm2/m128, xmm1<br/>*/
val x86_movups_movupd_movdqu_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movups_movupd_movdqu_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (if (mod_var) == (3)
then (None() : option(string), 0 : int, 0 : int, 0 : int)
else x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg1, x86) = (x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move low packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 12: MOVLPS xmm, m64<br/> 
 66 0F 12: MOVLPD xmm, m64<br/>*/
val x86_movlps_movlpd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_movlps_movlpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVLPS/MOVLPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let x86 = (write_xmmi_size(8, xmm_index, mem, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move low packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 13: MOVLPS m64, xmm<br/> 
 66 0F 13: MOVLPD m64, xmm<br/>*/
val x86_movlps_movlpd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movlps_movlpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVLPS/MOVLPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(8, xmm_index, 0)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg1, x86) = (x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, xmm, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move high packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 16: MOVHPS xmm, m64<br/> 
 66 0F 16: MOVHPD xmm, m64<br/>*/
val x86_movhps_movhpd_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rmem, rreg, wreg}
function x86_movhps_movhpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVHPS/MOVHPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, mem, increment_rip_by, v_addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 1, 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let low_qword = (xmmi_size(8, xmm_index, 0)) : int in
let result = (merge_2_u64s(mem, low_qword)) : int in
let x86 = (write_xmmi_size(16, xmm_index, result, 0)) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*Move high packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 17: MOVHPS m64, xmm<br/> 
 66 0F 17: MOVHPD m64, xmm<br/>*/
val x86_movhps_movhpd_op_en_mr : (int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, wmv, wreg}
function x86_movhps_movhpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MOVHPS/MOVHPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let xmm_index = (reg_index(reg, rex_byte, 2)) : int in
let xmm = (xmmi_size(16, xmm_index, 0)) : int in
let high_qword = (unsigned(get_slice_int((127) - (64), xmm, 64))) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let (flg0, addr, increment_rip_by, x86) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let (flg1, x86) = (x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, high_qword, seg_reg, addr, rex_byte, r_m, mod_var, 0)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

