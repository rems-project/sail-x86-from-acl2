$include <prelude.sail>
$include <string.sail>

/*Move scalar single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 F3 0F 10: MOVSS xmm1, xmm2/m32<br/> 
 F2 0F 10: MOVSD xmm1, xmm2/m64<br/>*/
val x86_movss_movsd_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_movss_movsd_op_en_rm (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVSS/MOVSD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let operand_size = (the_range(4, 8, if (get_slice_int(8, sp_dp, 0)) == (get_slice_int(8, 1, 0))
then 8
else 4)) : {|8, 4|} in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let operand_size = (if (unsigned(mod_var)) == (3)
then operand_size
else 16) : {|8, 16, 4|} in
{ write_xmmi_size(get_slice_int(5, operand_size, 0), xmm_index, xmm_mem);
write_iptr(proc_mode, temp_rip)
}

/*Move scalar single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 F3 0F 11: MOVSS xmm2/m32, xmm1<br/> 
 F2 0F 11: MOVSD xmm2/m64, xmm1<br/>*/
val x86_movss_movsd_op_en_mr : (range(0, 1), range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movss_movsd_op_en_mr (sp_dp, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVSS/MOVSD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let operand_size = (the_range(4, 8, if (get_slice_int(8, sp_dp, 0)) == (get_slice_int(8, 1, 0))
then 8
else 4)) : {|8, 4|} in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let xmm = (xmmi_size(get_slice_int(5, operand_size, 0), xmm_index)) : bits(128) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg0, addr, increment_rip_by) = (if (the_bits(1, mod_var)) == (get_slice_int(1, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg1 = (x86_operand_to_xmm_mem(proc_mode, operand_size, inst_ac?, unsigned(xmm), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else write_iptr(proc_mode, temp_rip)

/*Move aligned packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 28: MOVAPS xmm1, xmm2/m128<br/> 
 66 0F 28: MOVAPD xmm1, xmm2/m128<br/>*/
val x86_movaps_movapd_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_movaps_movapd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVAPS/MOVAPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let xmm_mem = (the_bits(128, xmm_mem)) : bits(128) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else { write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(xmm_mem));
write_iptr(proc_mode, temp_rip)
}

/*Move aligned packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 29: MOVAPS xmm2/m128, xmm1<br/> 
 66 0F 29: MOVAPD xmm2/m128, xmm1<br/>*/
val x86_movaps_movapd_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movaps_movapd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVAPS/MOVAPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let xmm = (xmmi_size(get_slice_int(5, 16, 0), xmm_index)) : bits(128) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg0, addr, increment_rip_by) = (if (the_bits(1, mod_var)) == (get_slice_int(1, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg1 = (x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, unsigned(xmm), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else write_iptr(proc_mode, temp_rip)

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
val x86_movups_movupd_movdqu_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_movups_movupd_movdqu_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, xmm_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 16, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let xmm_mem = (the_bits(128, xmm_mem)) : bits(128) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else { write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(xmm_mem));
write_iptr(proc_mode, temp_rip)
}

/*<h3>Op/En = MR: [OP XMM/M, XMM]</h3> 
 0F 11: MOVUPS xmm2/m128, xmm1<br/> 
 66 0F 11: MOVUPD xmm2/m128, xmm1<br/> 
 F3 0F 7F: MOVDQU xmm2/m128, xmm1<br/>*/
val x86_movups_movupd_movdqu_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movups_movupd_movdqu_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVUPS/MOVUPD/MOVDQU-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let xmm = (xmmi_size(get_slice_int(5, 16, 0), xmm_index)) : bits(128) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg0, addr, increment_rip_by) = (if (the_bits(1, mod_var)) == (get_slice_int(1, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg1 = (x86_operand_to_xmm_mem(proc_mode, 16, inst_ac?, unsigned(xmm), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else write_iptr(proc_mode, temp_rip)

/*Move low packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 12: MOVLPS xmm, m64<br/> 
 66 0F 12: MOVLPD xmm, m64<br/>*/
val x86_movlps_movlpd_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_movlps_movlpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVLPS/MOVLPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let mem = (the_bits(64, mem)) : bits(64) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
let addr = (the_sbits(64, addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else { write_xmmi_size(get_slice_int(5, 8, 0), xmm_index, unsigned(mem));
write_iptr(proc_mode, temp_rip)
}

/*Move low packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 13: MOVLPS m64, xmm<br/> 
 66 0F 13: MOVLPD m64, xmm<br/>*/
val x86_movlps_movlpd_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movlps_movlpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVLPS/MOVLPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let xmm = (the_bits(64, xmmi_size(get_slice_int(5, 8, 0), xmm_index))) : bits(64) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg0, addr, increment_rip_by) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg1 = (x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, unsigned(xmm), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else write_iptr(proc_mode, temp_rip)

/*Move high packed single/double precision floating-point values*/
/*<h3>Op/En = RM: [OP XMM, M]</h3> 
 0F 16: MOVHPS xmm, m64<br/> 
 66 0F 16: MOVHPD xmm, m64<br/>*/
val x86_movhps_movhpd_op_en_rm : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {escape, rmem, rreg}
function x86_movhps_movhpd_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVHPS/MOVHPD-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, mem, increment_rip_by, v_addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 1, 0), 8, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let mem = (the_bits(64, mem)) : bits(64) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
let v_addr = (the_sbits(64, v_addr)) : sbits(64) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let low_qword = (the_bits(64, xmmi_size(get_slice_int(5, 8, 0), xmm_index))) : bits(64) in
let result = (merge_2_u64s(mem, low_qword)) : bits(128) in
{ write_xmmi_size(get_slice_int(5, 16, 0), xmm_index, unsigned(result));
write_iptr(proc_mode, temp_rip)
}

/*Move high packed single/double precision floating-point values*/
/*<h3>Op/En = MR: [OP M, XMM]</h3> 
 0F 17: MOVHPS m64, xmm<br/> 
 66 0F 17: MOVHPD m64, xmm<br/>*/
val x86_movhps_movhpd_op_en_mr : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_movhps_movhpd_op_en_mr (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-MOVHPS/MOVHPD-OP/EN-MR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let xmm_index = (reg_index(reg, rex_byte, get_slice_int(2, 2, 0))) : bits(4) in
let xmm = (xmmi_size(get_slice_int(5, 16, 0), xmm_index)) : bits(128) in
let high_qword = (slice(xmm, 64, 64)) : bits(64) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let (flg0, addr, increment_rip_by) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 0, 0))) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-EFFECTIVE-ADDR-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let inst_ac? = (true) : bool in
let flg1 = (x86_operand_to_xmm_mem(proc_mode, 8, inst_ac?, unsigned(high_qword), seg_reg, addr, rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg1)
then throw(Emsg("Model state error: :X86-OPERAND-TO-XMM/MEM"))
else write_iptr(proc_mode, temp_rip)

