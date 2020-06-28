$include <prelude.sail>
$include <string.sail>
$include "multiply_spec.sail"

/*<h4>Op/En: M</h4> 
 
 <p>F6/4: <br/> 
 MUL r/m8: AX := AL * r/m8<br/><br/> 
 
 F7/4: <br/> 
 MUL r/m16: DX:AX := AX * r/m16<br/> 
 MUL r/m32: EDX:EAX := EAX * r/m32<br/> 
 MUL r/m64: RDX:RAX := RAX * r/m64<br/></p>*/
val x86_mul : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_mul (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-MUL") : string in
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
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let rax = (rgfi_size(reg_mem_size, 0, rex_byte, 0)) : int in
let (product_high, product_low, product) = (mul_spec(reg_mem_size, rax, reg_mem)) : (int, int, int) in
let x86 = ((match reg_mem_size {
1 => let x86 = (write_rgfi_size(2, 0, product, rex_byte, 0)) : int in
0,
_ => let x86 = (write_rgfi_size(reg_mem_size, 0, product_low, rex_byte, 0)) : int in
let x86 = (write_rgfi_size(reg_mem_size, 2, product_high, rex_byte, 0)) : int in
0
}) : int) : int in
let x86 = (if (product_high) == (0)
then let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(0, rflags_var)) : int in
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
let new_rflags = (set_rflagsbits_get_of(0, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0
else let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(1, rflags_var)) : int in
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
let new_rflags = (set_rflagsbits_get_of(1, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*<h4>Op/En: M</h4> 
 
 <p>F6/5: <br/> 
 IMUL r/m8: AX := AL * r/m8<br/><br/> 
 
 F7/5: <br/> 
 IMUL r/m16: DX:AX := AX * r/m16<br/> 
 IMUL r/m32: EDX:EAX := EAX * r/m32<br/> 
 IMUL r/m64: RDX:RAX := RAX * r/m64<br/></p>*/
val x86_imul_op_en_m : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_imul_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-IMUL-OP/EN-M") : string in
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
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let rax = (rgfi_size(reg_mem_size, 0, rex_byte, 0)) : int in
let (product_high, product_low, product, cf_and_of) = (imul_spec(reg_mem_size, rax, reg_mem)) : (int, int, int, int) in
let x86 = ((match reg_mem_size {
1 => let x86 = (write_rgfi_size(2, 0, product, rex_byte, 0)) : int in
0,
_ => let x86 = (write_rgfi_size(reg_mem_size, 0, product_low, rex_byte, 0)) : int in
let x86 = (write_rgfi_size(reg_mem_size, 2, product_high, rex_byte, 0)) : int in
0
}) : int) : int in
let x86 = (let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(cf_and_of, rflags_var)) : int in
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
let new_rflags = (set_rflagsbits_get_of(cf_and_of, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*<h4>Op/En: RM</h4> 
 
 <p>0F AF:<br/> 
 IMUL r16, r/m16: r16 := r16 * r/m16 <br/> 
 IMUL r32, r/m32: r32 := r32 * r/m32 <br/> 
 IMUL r64, r/m64: r64 := r64 * r/m64 <br/> </p>*/
val x86_imul_op_en_rm : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, wreg, undef, rmem, escape}
function x86_imul_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-IMUL-OP/EN-RM") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let reg_mem_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
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
else let register_var = (rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), rex_byte, 0)) : int in
let (product_high, product_low, product, cf_and_of) = (imul_spec(reg_mem_size, reg_mem, register_var)) : (int, int, int, int) in
let x86 = (write_rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), product_low, rex_byte, 0)) : int in
let x86 = (let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(cf_and_of, rflags_var)) : int in
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
let new_rflags = (set_rflagsbits_get_of(cf_and_of, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

/*<h4>Op/En: RMI</h4> 
 
 <p>6B ib:<br/> 
 IMUL r16, r/m16, imm8<br/> 
 IMUL r32, r/m32, imm8 <br/> 
 IMUL r64, r/m64, imm8 <br/> <br/> 
 
 69 iw:<br/> 
 IMUL r16, r/m16, imm16 <br/> 
 IMUL r32, r/m32, imm32 <br/> 
 IMUL r64, r/m64, imm32 <br/> </p>*/
val x86_imul_op_en_rmi : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wreg, rmem, escape}
function x86_imul_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
let ctx = ("X86-IMUL-OP/EN-RMI") : string in
let r_m = (modr_m_get_r_m(modr_m)) : int in
let mod_var = (modr_m_get_mod(modr_m)) : int in
let reg = (modr_m_get_reg(modr_m)) : int in
let p2 = (prefixes_get_seg(prefixes)) : int in
let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let reg_mem_size = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0)) : int in
let imm_size = (if (opcode) == (107)
then 1
else if (reg_mem_size) == (2)
then 2
else 4) : int in
let seg_reg = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib, 0)) : int in
let inst_ac? = (true) : bool in
let (flg0, reg_mem, increment_rip_by, addr, x86) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, imm_size, 0)) : (option(string), int, int, int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, increment_rip_by, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let (flg1, imm, x86) = (rme_size(proc_mode, imm_size, temp_rip, 1, ":X", false, 0, struct {mem_ptr? = false})) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, imm_size, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else let badlength? = (check_instruction_length(start_rip, temp_rip, 0)) : option(int) in
if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['!!FAULT-FRESH', ':GP', '0', ':INSTRUCTION-LENGTH', 'BADLENGTH?']"))
else let (product_high, product_low, product, cf_and_of) = (imul_spec(reg_mem_size, reg_mem, imm)) : (int, int, int, int) in
let x86 = (write_rgfi_size(reg_mem_size, reg_index(reg, rex_byte, 2), product_low, rex_byte, 0)) : int in
let x86 = (let x86 = (let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(cf_and_of, rflags_var)) : int in
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
let new_rflags = (set_rflagsbits_get_of(cf_and_of, rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0) : int in
let x86 = (write_iptr(proc_mode, temp_rip, 0)) : int in
0

