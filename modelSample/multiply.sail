$ifndef __X86_MULTIPLY
$define __X86_MULTIPLY
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
val x86_mul : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_mul (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-MUL") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let select_byte_operand : bool = ((opcode) == (0xf6)) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
let rax_var : bits(64) = (rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, rex_byte)) in
let (product_high, product_low, product) : (bits(64), bits(64), bits(128)) = (mul_spec(reg_mem_size, rax_var, truncate(reg_mem, 64))) in
{ (match reg_mem_size {
1 => write_rgfi_size(0x2, 0x0, unsigned(product), rex_byte),
_ => { { write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(product_low), rex_byte);
write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(product_high), rex_byte)
} }
}) : unit;
if (product_high) == (0x0000_0000_0000_0000)
then { { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (0b0)]) in
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
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (0b0)]) in
rflags = (new_rflags).bits
}
}
else { { let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (0b1)]) in
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
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (0b1)]) in
rflags = (new_rflags).bits
}
};
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*<h4>Op/En: M</h4> 
 
 <p>F6/5: <br/> 
 IMUL r/m8: AX := AL * r/m8<br/><br/> 
 
 F7/5: <br/> 
 IMUL r/m16: DX:AX := AX * r/m16<br/> 
 IMUL r/m32: EDX:EAX := EAX * r/m32<br/> 
 IMUL r/m64: RDX:RAX := RAX * r/m64<br/></p>*/
val x86_imul_op_en_m : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_imul_op_en_m (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-IMUL-OP/EN-M") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let select_byte_operand : bool = ((opcode) == (0xf6)) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, select_byte_operand, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
let rax_var : bits(64) = (rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, rex_byte)) in
let (product_high, product_low, product, cf_and_of) : (bits(64), bits(64), bits(128), bits(1)) = (imul_spec(reg_mem_size, rax_var, truncate(reg_mem, 64))) in
{ (match reg_mem_size {
1 => write_rgfi_size(0x2, 0x0, unsigned(product), rex_byte),
_ => { { write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(product_low), rex_byte);
write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(product_high), rex_byte)
} }
}) : unit;
{ let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (cf_and_of)]) in
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
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (cf_and_of)]) in
rflags = (new_rflags).bits
};
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*<h4>Op/En: RM</h4> 
 
 <p>0F AF:<br/> 
 IMUL r16, r/m16: r16 := r16 * r/m16 <br/> 
 IMUL r32, r/m32: r32 := r32 * r/m32 <br/> 
 IMUL r64, r/m64: r64 := r64 * r/m64 <br/> </p>*/
val x86_imul_op_en_rm : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_imul_op_en_rm (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-IMUL-OP/EN-RM") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, 0b000)) in
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
let register_var : bits(64) = (rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), rex_byte)) in
let (product_high, product_low, product, cf_and_of) : (bits(64), bits(64), bits(128), bits(1)) = (imul_spec(reg_mem_size, truncate(reg_mem, 64), register_var)) in
{ write_rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(product_low), rex_byte);
{ let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (cf_and_of)]) in
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
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (cf_and_of)]) in
rflags = (new_rflags).bits
};
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*<h4>Op/En: RMI</h4> 
 
 <p>6B ib:<br/> 
 IMUL r16, r/m16, imm8<br/> 
 IMUL r32, r/m32, imm8 <br/> 
 IMUL r64, r/m64, imm8 <br/> <br/> 
 
 69 iw:<br/> 
 IMUL r16, r/m16, imm16 <br/> 
 IMUL r32, r/m32, imm32 <br/> 
 IMUL r64, r/m64, imm32 <br/> </p>*/
val x86_imul_op_en_rmi : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_imul_op_en_rmi (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-IMUL-OP/EN-RMI") in
let r_m : bits(3) = ((modr_m)[r_m]) in
let mod_var : bits(2) = ((modr_m)[mod]) in
let reg : bits(3) = ((modr_m)[reg]) in
let p2 : bits(8) = ((prefixes)[seg]) in
let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let reg_mem_size : {|1, 2, 4, 8|} = (select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) in
let imm_size : {|1, 2, 4|} = { (if (opcode) == (0x6b)
then 1
else if (reg_mem_size) == (2)
then 2
else 4) } in
let seg_reg : {|0, 1, 2, 3, 4, 5|} = (select_segment_register(proc_mode, p2, p4?, mod_var, r_m, sib)) in
let inst_ac? : bool = (true) in
let (flg0, reg_mem, increment_rip_by, addr) : (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64)) = (x86_operand_from_modr_m_and_sib_bytes(proc_mode, 0b0, reg_mem_size, inst_ac?, false, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, bits_of_int(imm_size, 3))) in
let increment_rip_by : bits(3) = (bits_of_int(increment_rip_by, 3)) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :X86-OPERAND-FROM-MODR/M-AND-SIB-BYTES"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let (flg1, imm) : (option(string), bits(128)) = (rme_size(proc_mode, imm_size, sail_sign_extend(temp_rip, 64), 1, ":X", false, struct {mem_ptr? = false})) in
let imm : bits(32) = (truncate(imm, 32)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :RME-SIZE-ERROR"))
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
let (product_high, product_low, product, cf_and_of) : (bits(64), bits(64), bits(128), bits(1)) = (imul_spec(reg_mem_size, truncate(reg_mem, 64), sail_zero_extend(imm, 64))) in
{ write_rgfi_size(bits_of_int(reg_mem_size, 4), reg_index(reg, rex_byte, 0b10), unsigned(product_low), rex_byte);
{ let rflags_var : bits(32) = (rflags) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (cf_and_of)]) in
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
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (cf_and_of)]) in
rflags = (new_rflags).bits
};
write_iptr(proc_mode, temp_rip)
}
}
}
}
}
}

$endif
