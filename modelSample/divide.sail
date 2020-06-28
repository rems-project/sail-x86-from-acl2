$include <prelude.sail>
$include <string.sail>
$include "divide_spec.sail"

/*<h4>Op/En: M</h4> 
 
 <p>F6/6:<br/> 
 DIV r/m8: ( AX div r/m8 ) , AH := Remainder, AL := Quotient<br/><br/> 
 F7/6:<br/> 
 DIV r/m16: ( DX:AX div r/m16 ) , DX := Remainder, AX := Quotient<br/> 
 DIV r/m32: ( EDX:EAX div r/m8 ) , EDX := Remainder, EAX := Quotient<br/> 
 DIV r/m64: ( RDX:RAX div r/m8 ) , RDX := Remainder, RAX := Quotient<br/></p>*/
val x86_div : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_div (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-DIV") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((opcode) == (246)) : bool in
let reg_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else if (reg_mem) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT--ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let rax = (rgfi_size(if select_byte_operand
then 2
else reg_mem_size, 0, rex_byte, 0)) : int in
let rdx = (if select_byte_operand
then 0
else rgfi_size(reg_mem_size, 2, rex_byte, 0)) : int in
let dividend = (if select_byte_operand
then rax
else changeBits(rax, ash(reg_mem_size, 3), ash(reg_mem_size, 4), rdx)) : int in
let (overflow?, quotient, remainder) = (div_spec(reg_mem_size, dividend, reg_mem)) : (bool, int, int) in
if overflow?
then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
else let x86 = ((match reg_mem_size {
1 => let result = (changeBits(quotient, 8, 8, remainder)) : int in
let x86 = (write_rgfi_size(2, 0, result, rex_byte, 0)) : int in
0,
_ => let x86 = (write_rgfi_size(reg_mem_size, 0, quotient, rex_byte, 0)) : int in
let x86 = (write_rgfi_size(reg_mem_size, 2, remainder, rex_byte, 0)) : int in
0
}) : int) : int in
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

/*<h4>Op/En: M</h4> 
 
 <p>F6/7:<br/> 
 IDIV r/m8: ( AX div r/m8 ) , AH := Remainder, AL := Quotient<br/><br/> 
 
 F7/7:<br/> 
 IDIV r/m16: ( DX:AX div r/m16 ) , DX := Remainder, AX := Quotient <br/> 
 IDIV r/m32: ( EDX:EAX div r/m8 ) , EDX := Remainder, EAX := Quotient <br/> 
 IDIV r/m64: ( RDX:RAX div r/m8 ) , RDX := Remainder, RAX := Quotient</p>*/
val x86_idiv : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_idiv (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-IDIV") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let select_byte_operand = ((opcode) == (246)) : bool in
let reg_mem_size = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false, 0)) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else if (reg_mem) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':DE', 'NIL', ':DE-EXCEPTION-SOURCE-OPERAND-ZERO', 'REG/MEM']"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let rax = (rgfi_size(if select_byte_operand
then 2
else reg_mem_size, 0, rex_byte, 0)) : int in
let rdx = (if select_byte_operand
then 0
else rgfi_size(reg_mem_size, 2, rex_byte, 0)) : int in
let dividend = (if select_byte_operand
then rax
else changeBits(rax, ash(reg_mem_size, 3), ash(reg_mem_size, 4), rdx)) : int in
let (overflow?, quotient, remainder) = (idiv_spec(reg_mem_size, dividend, reg_mem)) : (bool, int, int) in
if overflow?
then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
else let x86 = ((match reg_mem_size {
1 => let result = (changeBits(quotient, 8, 8, remainder)) : int in
let x86 = (write_rgfi_size(2, 0, result, rex_byte, 0)) : int in
0,
_ => let x86 = (write_rgfi_size(reg_mem_size, 0, quotient, rex_byte, 0)) : int in
let x86 = (write_rgfi_size(reg_mem_size, 2, remainder, rex_byte, 0)) : int in
0
}) : int) : int in
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

