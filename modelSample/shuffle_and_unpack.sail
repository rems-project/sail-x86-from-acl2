$ifndef __X86_SHUFFLE_AND_UNPACK
$define __X86_SHUFFLE_AND_UNPACK
$include <prelude.sail>
$include <string.sail>

val extract_32_bits : (bits(128), bits(2)) -> bits(32)
function extract_32_bits (x, n) =
(match n {
0 => x[31 .. 0],
1 => x[63 .. 32],
2 => x[95 .. 64],
_ => x[127 .. 96]
}) : bits(32)

val extract_64_bits : (bits(128), range(0, 1)) -> bits(64)
function extract_64_bits (x, n) =
(match n {
0 => x[63 .. 0],
_ => x[127 .. 64]
}) : bits(64)

/*Shuffle packed single-precision floating-point values*/
/*<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 0F C6: SHUFPS xmm1, xmm2/m128, imm8<br/>*/
val x86_shufps_op_en_rmi : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_shufps_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SHUFPS-OP/EN-RMI") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let xmm_index : bits(4) = (reg_index(reg, rex_byte, 0b10)) in
let xmm : bits(128) = (xmmi_size(0b1_0000, xmm_index)) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg1, imm) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let imm : bits(8) = (truncate(imm, 8)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let imm0 : bits(2) = (imm[1 .. 0]) in
let imm1 : bits(2) = (imm[3 .. 2]) in
let imm2 : bits(2) = (imm[5 .. 4]) in
let imm3 : bits(2) = (imm[7 .. 6]) in
let dword0 : bits(32) = (extract_32_bits(xmm, imm0)) in
let dword1 : bits(32) = (extract_32_bits(xmm, imm1)) in
let dword2 : bits(32) = (extract_32_bits(xmm_mem, imm2)) in
let dword3 : bits(32) = (extract_32_bits(xmm_mem, imm3)) in
let result : bits(128) = ((dword3) @ ((dword2) @ ((dword1) @ (dword0)))) in
{ write_xmmi_size(0b1_0000, xmm_index, unsigned(result));
write_iptr(proc_mode, temp_rip)
}
}
}
}
}
}

/*Shuffle packed double-precision floating-point values*/
/*<h3>Op/En = RMI: [OP XMM, XMM/M, IMM]</h3> 
 66 0F C6: SHUFPD xmm1, xmm2/m128, imm8<br/>*/
val x86_shufpd_op_en_rmi : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_shufpd_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-SHUFPD-OP/EN-RMI") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let xmm_index : bits(4) = (reg_index(reg, rex_byte, 0b10)) in
let xmm : bits(128) = (xmmi_size(0b1_0000, xmm_index)) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b001)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg1, imm) : (option(string), bits(128)) = (rme_size(proc_mode, select_address_size(proc_mode, Some(prefixes)), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let imm : bits(8) = (truncate(imm, 8)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :IMM-RME-SIZE-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let imm0 : bits(1) = (logbit(0, imm)) in
let imm1 : bits(1) = (logbit(1, imm)) in
let qword0 : bits(64) = (extract_64_bits(xmm, unsigned(imm0))) in
let qword1 : bits(64) = (extract_64_bits(xmm_mem, unsigned(imm1))) in
let result : bits(128) = ((qword1) @ (qword0)) in
{ write_xmmi_size(0b1_0000, xmm_index, unsigned(result));
write_iptr(proc_mode, temp_rip)
}
}
}
}
}
}

/*Unpack and interleave low/high packed single-precision floating-point 
 values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 0F 14: UNPCKLPS xmm1, xmm2/m128<br/> 
 0F 15: UNPCKHPS xmm1, xmm2/m128<br/>*/
val x86_unpck?ps_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_unpck?ps_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-UNPCK?PS-OP/EN-RM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let xmm_index : bits(4) = (reg_index(reg, rex_byte, 0b10)) in
let xmm : bits(128) = (xmmi_size(0b1_0000, xmm_index)) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let dword0 : bits(32) = { (if (high_low) == (1)
then xmm[95 .. 64]
else xmm[31 .. 0]) } in
let dword1 : bits(32) = { (if (high_low) == (1)
then xmm_mem[95 .. 64]
else xmm_mem[31 .. 0]) } in
let dword2 : bits(32) = { (if (high_low) == (1)
then xmm[127 .. 96]
else xmm[63 .. 32]) } in
let dword3 : bits(32) = { (if (high_low) == (1)
then xmm_mem[127 .. 96]
else xmm_mem[63 .. 32]) } in
let result : bits(128) = ((dword3) @ ((dword2) @ ((dword1) @ (dword0)))) in
{ write_xmmi_size(0b1_0000, xmm_index, unsigned(result));
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*Unpack and interleave low/high packed double-precision floating-point 
 values*/
/*<h3>Op/En = RM: [OP XMM, XMM/M]</h3> 
 66 0F 14: UNPCKLPD xmm1, xmm2/m128<br/> 
 66 0F 15: UNPCKHPD xmm1, xmm2/m128<br/>*/
val x86_unpck?pd_op_en_rm : (range(0, 1), range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, wmv}
function x86_unpck?pd_op_en_rm (high_low, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-UNPCK?PD-OP/EN-RM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let xmm_index : bits(4) = (reg_index(reg, rex_byte, 0b10)) in
let xmm : bits(128) = (xmmi_size(0b1_0000, xmm_index)) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let seg_reg : seg_reg_idx = (select_segment_register(proc_mode, prefixes, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, xmm_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b1, 16, inst_ac?, false, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, bits_of_int(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let qword0 : bits(64) = { (if (high_low) == (1)
then xmm[127 .. 64]
else xmm[63 .. 0]) } in
let qword1 : bits(64) = { (if (high_low) == (1)
then xmm_mem[127 .. 64]
else xmm_mem[63 .. 0]) } in
let result : bits(128) = ((qword1) @ (qword0)) in
{ write_xmmi_size(0b1_0000, xmm_index, unsigned(result));
write_iptr(proc_mode, temp_rip)
}
}
}
}

$endif
