$ifndef __X86_DIVIDE
$define __X86_DIVIDE
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
val x86_div : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_div (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-DIV") in
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
if (reg_mem) == (0x0000_0000_0000_0000_0000_0000_0000_0000)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
let temp_rip : sbits(48) = (truncate(temp_rip, 48)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT--ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, temp_rip, 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let rax_var : bits(64) = (rgfi_size(bits_of_int(if select_byte_operand
then 2
else reg_mem_size, 4), 0x0, rex_byte)) in
let rdx_var : bits(64) = { (if select_byte_operand
then 0x0000_0000_0000_0000
else rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, rex_byte)) } in
let dividend : int = { (if select_byte_operand
then unsigned(rax_var)
else changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))) } in
let (overflow?, quotient, remainder) : (bool, bits(128), bits(64)) = (div_spec(reg_mem_size, bits_of_int(dividend, 128), truncate(reg_mem, 64))) in
{ if overflow?
then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
else ();
(match reg_mem_size {
1 => { let result : bits(128) = (changeSlice(quotient, 8, 8, truncate(remainder, 8))) in
write_rgfi_size(0x2, 0x0, unsigned(result), rex_byte) },
_ => { { write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(quotient), rex_byte);
write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(remainder), rex_byte)
} }
}) : unit;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (sailval)]) in
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
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (sailval)]) in
rflags = (new_rflags).bits;
write_iptr(proc_mode, temp_rip)
}
}
}
}

/*<h4>Op/En: M</h4> 
 
 <p>F6/7:<br/> 
 IDIV r/m8: ( AX div r/m8 ) , AH := Remainder, AL := Quotient<br/><br/> 
 
 F7/7:<br/> 
 IDIV r/m16: ( DX:AX div r/m16 ) , DX := Remainder, AX := Quotient <br/> 
 IDIV r/m32: ( EDX:EAX div r/m8 ) , EDX := Remainder, EAX := Quotient <br/> 
 IDIV r/m64: ( RDX:RAX div r/m8 ) , RDX := Remainder, RAX := Quotient</p>*/
val x86_idiv : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_idiv (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) =
let ctx : string = ("X86-IDIV") in
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
if (reg_mem) == (0x0000_0000_0000_0000_0000_0000_0000_0000)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :DE NIL :DE-EXCEPTION-SOURCE-OPERAND-ZERO REG/MEM)"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, sail_zero_extend(increment_rip_by, 48))) in
{ if is_some(flg)
then throw(Emsg("Model state error: :RIP-INCREMENT-ERROR"))
else ();
let badlength? : option(int) = (check_instruction_length(start_rip, truncate(temp_rip, 48), 0b000)) in
{ if is_some(badlength?)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!FAULT-FRESH :GP 0 :INSTRUCTION-LENGTH BADLENGTH?)"))
else ();
let rax_var : bits(64) = (rgfi_size(bits_of_int(if select_byte_operand
then 2
else reg_mem_size, 4), 0x0, rex_byte)) in
let rdx_var : bits(64) = { (if select_byte_operand
then 0x0000_0000_0000_0000
else rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, rex_byte)) } in
let dividend : int = { (if select_byte_operand
then unsigned(rax_var)
else changeSlice(unsigned(rax_var), ash(reg_mem_size, 3), ash(reg_mem_size, 4), unsigned(rdx_var))) } in
let (overflow?, quotient, remainder) : (bool, bits(64), bits(64)) = (idiv_spec(reg_mem_size, bits_of_int(dividend, 128), truncate(reg_mem, 64))) in
{ if overflow?
then throw(Emsg("Model state error: :UNSIGNED-DIVIDE-ERROR-OVERFLOW"))
else ();
(match reg_mem_size {
1 => { let result : bits(64) = (changeSlice(quotient, 8, 8, truncate(remainder, 8))) in
write_rgfi_size(0x2, 0x0, unsigned(result), rex_byte) },
_ => { { write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x0, unsigned(quotient), rex_byte);
write_rgfi_size(bits_of_int(reg_mem_size, 4), 0x2, unsigned(remainder), rex_byte)
} }
}) : unit;
let rflags_var : bits(32) = (rflags) in
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with cf = (sailval)]) in
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
let sailval : bits(1) = (undef_flg()) in
let new_rflags : rflagsbits = ([(Mk_rflagsbits(rflags_var)) with of = (sailval)]) in
rflags = (new_rflags).bits;
write_iptr(proc_mode, truncate(temp_rip, 48))
}
}
}
}

$endif
