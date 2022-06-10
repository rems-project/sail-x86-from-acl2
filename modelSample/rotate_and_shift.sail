$include <prelude.sail>
$include <string.sail>
$include "shifts_spec.sail"
$include "rotates_spec.sail"

/*<p> 
 Op/En: MI<br/> 
 C0/0: ROL r/m8, imm8<br/> 
 C0/1: ROR r/m8, imm8<br/> 
 C0/2: RCL r/m8, imm8<br/> 
 C0/3: RCR r/m8, imm8<br/> 
 C0/4: SAL/SHL r/m8, imm8<br/> 
 C0/5: SHR r/m8, imm8<br/> 
 C0/7: SAR r/m8, imm8<br/> 
 C1/0: ROL r/m16/32/64, imm8<br/> 
 C1/1: ROR r/m16/32/64, imm8<br/> 
 C1/2: RCL r/m16/32/64, imm8<br/> 
 C1/3: RCR r/m16/32/64, imm8<br/> 
 C1/4: SAL/SHL r/m16/32/64, imm8<br/> 
 C1/5: SHR r/m16/32/64, imm8<br/> 
 C1/7: SAR r/m16/32/64. imm8<br/> 
 </p> 
 
 <p> 
 Op/En: M1<br/> 
 D0/0: ROL r/m8, 1<br/> 
 D0/1: ROR r/m8, 1<br/> 
 D0/2: RCL r/m8, 1<br/> 
 D0/3: RCR r/m8, 1<br/> 
 D0/4: SAL/SHL r/m8, 1<br/> 
 D0/5: SHR r/m8, 1<br/> 
 D0/7: SAR r/m8, 1<br/> 
 D1/0: ROL r/m16/32/64, 1<br/> 
 D1/1: ROR r/m16/32/64, 1<br/> 
 D1/2: RCL r/m16/32/64, 1<br/> 
 D1/3: RCR r/m16/32/64, 1<br/> 
 D1/4: SAL/SHL r/m16/32/64, 1<br/> 
 D1/5: SHR r/m16/32/64, 1<br/> 
 D1/7: SAR r/m16/32/64, 1<br/> 
 </p> 
 
 <p> 
 Op/En: MC<br/> 
 D2/0: ROL r/m8, CL<br/> 
 D2/1: ROR r/m8, CL<br/> 
 D2/2: RCL r/m8, CL<br/> 
 D2/3: RCR r/m8, CL<br/> 
 D2/4: SAL/SHL r/m8, CL<br/> 
 D2/5: SHR r/m8, CL<br/> 
 D2/7: SAR r/m8, CL<br/> 
 D3/0: ROL r/m16/32/64, CL<br/> 
 D3/1: ROR r/m16/32/64, CL<br/> 
 D3/2: RCL r/m16/32/64, CL<br/> 
 D3/3: RCR r/m16/32/64, CL<br/> 
 D3/4: SAL/SHL r/m16/32/64, CL<br/> 
 D3/5: SHR r/m16/32/64, CL<br/> 
 D3/7: SAR r/m16/32/64, CL<br/> 
 </p>*/
val x86_sal_sar_shl_shr_rcl_rcr_rol_ror : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_sal_sar_shl_shr_rcl_rcr_rol_ror (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let byte_operand? = (((opcode) == (get_slice_int(8, 192, 0))) | (((opcode) == (get_slice_int(8, 208, 0))) | ((opcode) == (get_slice_int(8, 210, 0))))) : bool in
let reg_mem_size = (the_range(0, 8, select_operand_size(proc_mode, byte_operand?, rex_byte, false, prefixes, false, false, false))) : {|8, 1, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, if ((opcode) == (get_slice_int(8, 192, 0))) | ((opcode) == (get_slice_int(8, 193, 0)))
then 1
else 0, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
let increment_rip_by = (the_bits(3, increment_rip_by)) : bits(3) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, the_bits(48, increment_rip_by))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, shift_rotate_by) = ((match opcode {
208 => (None() : option(string), 1 : {|1|}),
209 => (None() : option(string), 1 : {|1|}),
210 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
211 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
192 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false}),
193 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false}),
_ => (None() : option(string), 0 : {|0|})
}) : (option(string), int)) : (option(string), int) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let countmask = (if logbitp(3, rex_byte)
then 63
else 31) : {|31, 63|} in
let shift_rotate_by = (logand(countmask, shift_rotate_by)) : int in
let (flg, temp_rip) = (if ((opcode) == (get_slice_int(8, 192, 0))) | ((opcode) == (get_slice_int(8, 193, 0)))
then add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))
else (None() : option(string), the_sbits(52, temp_rip))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(49, temp_rip)) : sbits(49) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, the_sbits(48, temp_rip), get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let input_rflags = (rflags) : bits(32) in
let (result, output_rflags, undefined_flags) = ((match reg {
0 => rol_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
1 => ror_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
2 => rcl_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
3 => rcr_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
4 => sal_shl_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
5 => shr_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
7 => sar_spec(reg_mem_size, get_slice_int(64, reg_mem, 0), get_slice_int(6, shift_rotate_by, 0), input_rflags),
_ => (get_slice_int(64, 0, 0), get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bits(32), bits(32))) : (bits(64), bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
{ write_user_rflags(output_rflags, undefined_flags);
let flg2 = (x86_operand_to_reg_mem(proc_mode, reg_mem_size, inst_ac?, false, unsigned(sail_mask(64, trunc(reg_mem_size, result))), seg_reg, the_sbits(64, addr), rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg2)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, the_sbits(48, temp_rip))
}

/*Double-precision shift left or right.*/
/*<p> 
 Op/En: MRI<br/> 
 0F A4: SHLD r/m16, r16, imm8<br/> 
 0F A4: SHLD r/m32, r32, imm8<br/> 
 0F A4: SHLD r/m64, r64, imm8<br/> 
 </p> 
 
 <p> 
 Op/En: MRC<br/> 
 0F A5: SHLD r/m16, r16, CL<br/> 
 0F A5: SHLD r/m32, r32, CL<br/> 
 0F A5: SHLD r/m64, r64, CL<br/> 
 </p> 
 
 <p> 
 Op/En: MRI<br/> 
 0F AC: SHRD r/m16, r16, imm8<br/> 
 0F AC: SHRD r/m32, r32, imm8<br/> 
 0F AC: SHRD r/m64, r64, imm8<br/> 
 </p> 
 
 <p> 
 Op/En: MRC<br/> 
 0F AD: SHRD r/m16, r16, CL<br/> 
 0F AD: SHRD r/m32, r32, CL<br/> 
 0F AD: SHRD r/m64, r64, CL<br/> 
 </p>*/
val x86_shld_shrd : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_shld_shrd (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx = ("X86-SHLD/SHRD") : string in
let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
let reg = (modr_m_get_reg(modr_m)) : bits(3) in
let p2 = (prefixes_get_seg(prefixes)) : bits(8) in
let p4? = ((get_slice_int(8, 103, 0)) == (prefixes_get_adr(prefixes))) : bool in
let operand_size = (the_range(2, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false))) : {|8, 2, 4|} in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) : {|0, 1, 2, 3, 4, 5|} in
let inst_ac? = (true) : bool in
let (flg, dst_value, increment_rip_by, dst_addr) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, get_slice_int(1, 0, 0), operand_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, get_slice_int(3, 1, 0))) : (option(string), int, {|0, 1, 2, 4|}, sbits(66)) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, increment_rip_by, 0))) : (option(string), sbits(52)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let src_value = (rgfi_size(get_slice_int(4, operand_size, 0), reg_index(reg, rex_byte, get_slice_int(2, 2, 0)), rex_byte)) : bits(64) in
let (flg, count) = ((match opcode {
164 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false}),
172 => rme_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false}),
165 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
173 => (None() : option(string), unsigned(rr08(get_slice_int(4, 1, 0), rex_byte))),
_ => (None() : option(string), 0 : {|0|})
}) : (option(string), int)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let (flg, temp_rip) = ((match opcode {
164 => let (elem0, elem1) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))) : (option(string), sbits(52)) in
(elem0, the_sbits(53, elem1)),
172 => let (elem0, elem1) = (add_to_iptr(proc_mode, temp_rip, get_slice_int(48, 1, 0))) : (option(string), sbits(52)) in
(elem0, the_sbits(53, elem1)),
165 => (None() : option(string), the_sbits(53, temp_rip)),
173 => (None() : option(string), the_sbits(53, temp_rip)),
_ => (None() : option(string), get_slice_int(53, 0, 0))
}) : (option(string), sbits(53))) : (option(string), sbits(53)) in
let temp_rip = (the_sbits(48, temp_rip)) : sbits(48) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, get_slice_int(3, 0, 0))) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let count_mask = (if logbitp(3, rex_byte)
then 63
else 31) : {|31, 63|} in
let count = (logand(count_mask, count)) : int in
let input_rflags = (rflags) : bits(32) in
let (result, result_undefined?, output_rflags, undefined_flags) = ((match opcode {
164 => shld_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
165 => shld_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
172 => shrd_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
173 => shrd_spec(operand_size, get_slice_int(64, dst_value, 0), src_value, get_slice_int(6, count, 0), input_rflags),
_ => (get_slice_int(64, 0, 0), false, get_slice_int(32, 0, 0), get_slice_int(32, 0, 0))
}) : (bits(64), bool, bits(32), bits(32))) : (bits(64), bool, bits(32), bits(32)) in
let output_rflags = (output_rflags) : bits(32) in
let undefined_flags = (undefined_flags) : bits(32) in
let result = (if result_undefined?
then undef_read()
else unsigned(result)) : int in
{ write_user_rflags(output_rflags, undefined_flags);
let flg = (x86_operand_to_reg_mem(proc_mode, operand_size, inst_ac?, false, unsigned(sail_mask(64, trunc(operand_size, get_slice_int(64, result, 0)))), seg_reg, the_sbits(64, dst_addr), rex_byte, r_m, mod_var)) : option(string) in
if is_some(flg)
then throw(Emsg("Model state error: :X86-OPERAND-TO-REG/MEM"))
else write_iptr(proc_mode, temp_rip)
}

