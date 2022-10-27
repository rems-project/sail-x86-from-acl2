$ifndef __X86_DECODING_AND_SPEC_UTILS
$define __X86_DECODING_AND_SPEC_UTILS
$include <prelude.sail>
$include <string.sail>
$include "other_non_det.sail"
/*Operations to manipulate instruction pointers.*/
/*Read the instruction pointer from the register RIP, EIP, or IP.*/
/*<p> 
 In 64-bit mode, a 64-bit instruction pointer is read from the full RIP. 
 Since, in the model, this is a 48-bit signed integer, 
 this function returns a 48-bit signed integer. 
 </p> 
 <p> 
 In 32-bit mode, a 32-bit or 16-bit instruction pointer is read from 
 EIP ( i.e. the low 32 bits of RIP ) 
 or IP ( i.e. the low 16 bits of RIP ) , 
 based on the CS.D bit, 
 i.e. the D bit of the current code segment descriptor. 
 Either way, this function returns an unsigned 32-bit or 16-bit integer, 
 which is also a signed 48-bit integer. 
 </p> 
 <p> 
 See AMD manual, Oct ' 13, Vol. 1, Sec. 2.2.4 and Sec. 2.5. 
 AMD manual, Apr ' 16, Vol. 2, Sec 4.7.2., 
 and Intel manual, Mar ' 17, Vol. 1, Sec. 3.6. 
 </p> 
 <p> 
 In 32-bit mode, the address-size override prefix ( if present ) 
 should not affect the instruction pointer size. 
 It does not seem to make sense 
 to change the instruction pointer size on a per-instruction basis. 
 </p>*/
val read_iptr : (range(0, 4)) -> sbits(48) effect {escape}
function read_iptr (proc_mode) =
let iptr : sbits(48) = (read_rip()) in
(match proc_mode {
0 => iptr,
1 => sail_zero_extend(let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_d : bits(1) = ((Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) in
if (cs_d) == (0b1)
then truncate(iptr, 32)
else sail_zero_extend(truncate(iptr, 16), 32), 48),
_ => 0x0000_0000_0000
}) : sbits(48)

/*Add a specified amount to an instruction pointer.*/
/*<p> 
 The amount may be positive ( increment ) or negative ( decrement ) . 
 This just calculates the new instruction pointer value, 
 without storing it into the register RIP, EIP, or IP. 
 The starting value is the result of @ ( tsee read-*ip ) 
 or a previous invocation of @ ( tsee add-to-*ip ) . 
 </p> 
 <p> 
 In 64-bit mode, we check whether the result is a canonical address; 
 in 32-bit mode, we check whether the result is within the segment limit. 
 If these checks are not satisfied, 
 this function returns an error flag ( and 0 as incremented address ) , 
 which causes the x86 model to stop execution with an error. 
 It is not clear whether these checks should be performed 
 when the instruction pointer is incremented 
 or when an instruction byte is eventually accessed; 
 the Intel and AMD manuals seem unclear in this respect. 
 But since the failure of these checks stops execution with an error, 
 and it is in a way always ` ` safe ' ' to stop execution with an error 
 ( in the sense that the model provides no guarantees when this happens ) , 
 for now we choose to perform these checks here. 
 </p> 
 <p> 
 Note that a code segment is never expand-down, 
 so the valid effective addresses are always between 0 and the segment limit 
 ( cf. @ ( tsee segment-base-and-bounds ) ) . 
 </p>*/
val add_to_iptr : (range(0, 4), sbits(48), sbits(48)) -> (option(string), sbits(49)) effect {escape}
function add_to_iptr (proc_mode, iptr, delta) =
let iptr_plus_delta : sbits(49) = (bits_of_int((signed(iptr)) + (signed(delta)), 49)) in
(match proc_mode {
0 => { if canonical_address_p(signed(iptr_plus_delta))
then (None() : option(string), iptr_plus_delta)
else (Some(":NON-CANONICAL-INSTRUCTION-POINTER"), 0b0_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000) },
1 => { let (elem0, elem1) : (option(string), bits(32)) = { (let cs_limit : bits(32) = (bits_of_int(loghead(32, unsigned(seg_hidden_limits[1])), 32)) in
if ((0) <= (signed(iptr_plus_delta))) & ((signed(iptr_plus_delta)) <= (unsigned(cs_limit)))
then (None() : option(string), truncate(iptr_plus_delta, 32))
else (Some(":OUT-OF-SEGMENT-INSTRUCTION-POINTER"), 0x0000_0000)) } in
(elem0, sail_zero_extend(elem1, 49)) },
_ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0b0_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000)
}) : (option(string), sbits(49))

/*Write an instruction pointer into the register RIP, EIP, or IP.*/
/*<p> 
 In 64-bit mode, a 64-bit instruction pointer is written into the full RIP. 
 Since, in the model, this is a 48-bit signed integer, 
 this function consumes a 48-bit signed integer. 
 </p> 
 <p> 
 In 32-bit mode, the instruction pointer is 32 or 16 bits 
 based on the CS.D bit, i.e. the D bit of the current code segment descriptor. 
 In these cases, the argument to this function should be 
 a 32-bit or 16-bit unsigned integer, which is also a 48-bit signed integer. 
 </p> 
 <p> 
 See AMD manual, Oct ' 13, Vol. 1, Sec. 2.2.4 and Sec. 2.5. 
 AMD manual, Apr ' 16, Vol. 2, Sec 4.7.2., 
 and Intel manual, Mar ' 17, Vol. 1, Sec. 3.6. 
 </p> 
 <p> 
 According to Intel manual, Mar ' 17, Vol. 1, Table 3-1, 
 it seems that 
 when writing a 32-bit instruction pointer ( EIP ) 
 the high 32 bits of RIP should be set to 0, 
 and when writing a 16-bit instruction pointer ( IP ) 
 the high 48 bits of RIP should be left unmodified; 
 since in our model the RIP is 48 bits, 
 the above applies to the high 16 and 32 bits, respectively. 
 The pseudocode for the JMP instruction in Intel manual, Mar ' 17, Vol. 2 
 shows an assignment @ ( ' EIP <- tempEIP AND 0000FFFFh ' ) for the 16-bit case, 
 which seems to imply that 
 the high 32 ( or 16, in our model ) bits are left unmodified 
 and the high 16 bits of EIP are set to 0, 
 which would contradict Table 3-1; 
 the pseudocode for some other instructions 
 that directly write the instruction pointer ( e.g. RET and Jcc ) 
 show similar assignments. 
 However, it is possible that this assignment has a typo and should be 
 @ ( ' IP <- tempEIP AND 0000FFFFh ' ) instead, 
 which would be consistent with Table 3-1. 
 But we also note that the pseudocode for the JMP instruction 
 shows an assignment @ ( ' EIP <- tempEIP ' ) for the 32-bit case, 
 which seems to imply that 
 the high 32 ( or 16, in our model ) bits are left unmodified, 
 which would contradict Table 3-1. 
 The AMD manuals do not show pseudocode for these instructions, 
 and AMD manual, Oct ' 13, Vol. 1, Fig. 2-10 
 ( which is somewhat analogous to Intel ' s Table 3-1 ) 
 shows the high bits simply grayed out; 
 so the AMD manuals do not provide disambiguation help. 
 It is also possible that Table 3-1 has a typo and should say 
 that a 16-bit instruction pointer is zero-extended, 
 but that is not quite consistent with the pseudocode assignments to EIP, 
 which seem to imply that the high bits are untouched. 
 Table 3-1 is under a section titled 
 ` Address Calculation in 64-Bit Mode ' , 
 which may suggest that the table may not apply to 32-bit mode, 
 but then it is not clear how it would just apply to 64-bit mode. 
 For now, we decide to have this function follow Intel ' s Table 3-1, 
 but we may revise that if we manage to resolve these ambiguities. 
 We also note that Intel ' s Table 3-1 is consistent with the way in which 
 32-bit and 16-bit values are written to general-purpose registers 
 ( even though RIP/EIP/IP is not a general-purpose register ) ; 
 see @ ( tsee wr32 ) and @ ( tsee wr16 ) . 
 </p> 
 <p> 
 This function should be always called 
 with an instruction pointer of the right type 
 ( 48-bit signed, 32-bit unsigned, or 16-bit unsigned ) 
 based on the mode and code segment. 
 We may add a guard to ensure that in the future, 
 but for now in the code below 
 we coerce the instruction pointer to 32 and 16 bits as appropriate, 
 to verify guards; 
 these coercions are expected not to change 
 the argument instruction pointer. 
 </p>*/
val write_iptr : (range(0, 4), sbits(48)) -> unit effect {escape}
function write_iptr (proc_mode, iptr) =
(match proc_mode {
0 => write_rip(iptr),
1 => { let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_d : bits(1) = ((Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) in
if (cs_d) == (0b1)
then { write_rip(sail_zero_extend(truncate(iptr, 32), 48))
}
else { let rip_var : sbits(48) = (read_rip()) in
let rip_new : sbits(48) = (changeSlice(rip_var, 0, 16, truncate(iptr, 16))) in
write_rip(rip_new)
} },
_ => ()
}) : unit

/*Operations to manipulate stack pointers.*/
/*Read the stack pointer from the register RSP, ESP, or SP.*/
/*<p> 
 In 64-bit mode, a 64-bit stack pointer is read from the full RSP. 
 Since, in the model, this is a 64-bit signed integer, 
 this function returns a 64-bit signed integer. 
 </p> 
 <p> 
 In 32-bit mode, a 32-bit or 16-bit stack pointer is read from 
 ESP ( i.e. the low 32 bits of RSP ) 
 or SP ( i.e. the low 16 bits of RSP ) , 
 based on the SS.B bit, 
 i.e. the B bit of the current stack segment register. 
 Either way, this function returns an unsigned 32-bit or 16-bit integer, 
 which is also a signed 64-bit integer. 
 </p> 
 <p> 
 See Intel manual, Mar ' 17, Vol. 1, Sec. 6.2.3 and Sec. 6.2.5, 
 and AMD manual, Apr ' 16, Vol. 2, Sec 2.4.5 and Sec. 4.7.3. 
 The actual size of the value returned by this function 
 is @ ( ' StackAddrSize ' ) , 
 introduced in Intel manual, Mar ' 17, Vol. 2, Sec. 3.1.1.9. 
 </p> 
 <p> 
 In 32-bit mode, the address-size override prefix ( if present ) 
 should not affect the stack address size. 
 It does not seem to make sense 
 to change the stack address size on a per-instruction basis. 
 </p>*/
val read_sptr : (range(0, 4)) -> sbits(64) effect {escape, rreg}
function read_sptr (proc_mode) =
let sptr : sbits(64) = (rgfi(4)) in
(match proc_mode {
0 => sptr,
1 => sail_zero_extend(let ss_attr : bits(16) = (seg_hidden_attrs[2]) in
let ss_b : bits(1) = ((Mk_data_segment_descriptor_attributesbits(ss_attr))[d_b]) in
if (ss_b) == (0b1)
then truncate(sptr, 32)
else sail_zero_extend(truncate(sptr, 16), 32), 64),
_ => 0x0000_0000_0000_0000
}) : sbits(64)

/*Add a specified amount to a stack pointer.*/
/*<p> 
 The amount may be positive ( increment ) or negative ( decrement ) . 
 This just calculates the new stack pointer value, 
 without storing it into the register RSP, ESP, or SP. 
 The starting value is the result of @ ( tsee read-*sp ) 
 or a previous invocation of @ ( tsee add-to-*sp ) . 
 </p> 
 <p> 
 The increment or decrement is modular: 
 64 bits in 64-bit mode, 
 and either 32 or 16 bits in 32-bit mode ( depending on the SS.B bit ) . 
 Since our model uses signed 64-bit addresses, we use @ ( tsee i64 ) for them, 
 while we use @ ( tsee n32 ) or @ ( tsee n16 ) for 32-bit and 16-bit addresses. 
 </p> 
 <p> 
 In 64-bit mode, we check whether the result is a canonical address; 
 in 32-bit mode, we check whether the result is within the segment limit. 
 If these checks are not satisfied, 
 this function returns an error flag ( and 0 as new pointer ) , 
 which causes the x86 model to stop execution with an error. 
 It is not clear whether these checks should be performed 
 when the stack pointer is updated, 
 or when the stack is eventually accessed through the updated pointer; 
 the Intel and AMD manuals seem unclear in this respect. 
 But since the failure of these checks stops execution with an error, 
 and it is in a way always ` ` safe ' ' to stop execution with an error 
 ( in the sense that the model provides no guarantees when this happens ) , 
 for now we choose to perform these checks here. 
 </p> 
 <p> 
 Note that a stack segment may be expand-down or expand-up 
 ( see Intel manual, Mar ' 17, Vol. 3, Sec. 3.4.5.1 ) , 
 so the checks need to cover these two cases. 
 See @ ( tsee segment-base-and-bounds ) and @ ( tsee ea-to-la ) . 
 </p>*/
val add_to_sptr : (range(0, 4), sbits(64), sbits(64)) -> (option(string), sbits(64)) effect {escape}
function add_to_sptr (proc_mode, sptr, delta) =
let sptr_plus_delta : sbits(65) = (bits_of_int((signed(sptr)) + (signed(delta)), 65)) in
(match proc_mode {
0 => { let sptr_plus_delta : sbits(64) = (truncate(sptr_plus_delta, 64)) in
if canonical_address_p(signed(sptr_plus_delta))
then (None() : option(string), sptr_plus_delta)
else (Some(":NON-CANONICAL-STACK-ADDRESS"), 0x0000_0000_0000_0000) },
1 => { let (elem0, elem1) : (option(string), bits(32)) = { (let ss_limit : bits(32) = (seg_hidden_limits[2]) in
let ss_attr : bits(16) = (seg_hidden_attrs[2]) in
let ss_b : bits(1) = ((Mk_data_segment_descriptor_attributesbits(ss_attr))[d_b]) in
let ss_e : bits(1) = ((Mk_data_segment_descriptor_attributesbits(ss_attr))[e]) in
let ss_lower : int = { (if (ss_e) == (0b1)
then (1) + (unsigned(ss_limit))
else 0) } in
let ss_upper : bits(32) = { (if (ss_e) == (0b1)
then bits_of_int(if (ss_b) == (0b1)
then 4294967295
else 65535, 32)
else ss_limit) } in
let sptr_plus_delta : bits(32) = { (if (ss_b) == (0b1)
then truncate(sptr_plus_delta, 32)
else sail_zero_extend(truncate(sptr_plus_delta, 16), 32)) } in
if not_bool(((ss_lower) <= (unsigned(sptr_plus_delta))) & ((unsigned(sptr_plus_delta)) <= (unsigned(ss_upper))))
then (Some(":OUT-OF-SEGMENT-STACK-ADDRESS"), 0x0000_0000)
else (None() : option(string), sptr_plus_delta)) } in
(elem0, sail_zero_extend(elem1, 64)) },
_ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0x0000_0000_0000_0000)
}) : (option(string), sbits(64))

/*Write a stack pointer into the register RSP, ESP, or SP.*/
/*<p> 
 In 64-bit mode, a 64-bit stack pointer is written into the full RSP. 
 Since, in the model, this is a 64-bit signed integer, 
 this function consumes a 64-bit signed integer. 
 </p> 
 <p> 
 In 32-bit mode, the stack pointer is 32 or 16 bits based on the SS.B bit, 
 i.e. the B bit of the current stack segment descriptor. 
 In these cases, the argument to this function should be 
 a 32-bit or 16-bit unsigned integer, which is also a 64-bit signed integer. 
 </p> 
 <p> 
 See Intel manual, Mar ' 17, Vol. 1, Sec. 6.2.3 and Sec. 6.2.5, 
 and AMD manual, Apr ' 16, Vol. 2, Sec 2.4.5 and Sec. 4.7.3. 
 The actual size of the value consumed by this function 
 should be @ ( ' StackAddrSize ' ) , 
 introduced in Intel manual, Mar ' 17, Vol. 2, Sec. 3.1.1.9. 
 </p> 
 <p> 
 The pseudocode of stack instructions like PUSH 
 in Intel manual, Mar ' 17, Vol. 2 
 show assignments of the form 
 @ ( ' RSP <- ... ' ) , @ ( ' ESP <- ... ' ) , and @ ( ' SP <- ... ' ) 
 based on the stack address size. 
 This may suggests that 
 when the stack address size is 32 
 the assignment to ESP leaves the high 32 bits of RSP unchanged, 
 and when the stack address size is 16 
 the assignment to SP leaves the high 48 bits of RSP unchanged. 
 However, 
 as explained in the documentation of @ ( tsee wr32 ) and @ ( tsee wr16 ) , 
 normally writing to the low 32 bits of a general-purpose register 
 ( which RSP/ESP/SP is ) zeros the high 32 bits, 
 while writing the low 16 bits leaves the high 48 bits unchanged. 
 Thus, we follow this requirement also when writing RSP/ESP/SP implicitly, 
 via stack manipulation instructions like PUSH that use 
 this @ ( tsee write-*sp ) function to update the stack pointer register. 
 </p> 
 <p> 
 This function should be always called 
 with a stack pointer of the right type 
 ( 64-bit signed, 32-bit unsigned, or 16-bit unsigned ) 
 based on the stack address size. 
 We may add a guard to ensure that in the future, 
 but for now in the code below 
 we coerce the stack pointer to 32 and 16 bits as appropriate, 
 to verify guards; 
 these coercions are expected not to change the argument stack pointer. 
 </p>*/
val write_sptr : (range(0, 4), sbits(64)) -> unit effect {escape, rreg, wreg}
function write_sptr (proc_mode, sptr) =
(match proc_mode {
0 => write_rgfi(4, sptr),
1 => { let ss_attr : bits(16) = (seg_hidden_attrs[2]) in
let ss_b : bits(1) = ((Mk_data_segment_descriptor_attributesbits(ss_attr))[d_b]) in
if (ss_b) == (0b1)
then { write_rgfi(4, sail_zero_extend(truncate(sptr, 32), 64))
}
else { let rsp_var : sbits(64) = (rgfi(4)) in
let rsp_new : sbits(64) = (changeSlice(rsp_var, 0, 16, truncate(sptr, 16))) in
write_rgfi(4, rsp_new)
} },
_ => ()
}) : unit

/*Computing effective address using ModR/M, SIB bytes, and 
 displacement bytes present in the instruction*/
/*Calculates effective address when SIB is present.*/
/*<p>Source: Intel Vol. 2A, Table 2-3.</p> 
 <p>Also see Intel Vol. 2A, Table 2-2 and Figure 2-6.</p> 
 <p>In 64-bit mode, 
 we use @ ( ' rgfi ' ) to read bases as signed linear addresses, 
 which encode canonical linear addresses, 
 which are also effective addresses in 64-bit mode. 
 In 32-bit mode, 
 we use @ ( ' rr32 ' ) to read bases as unsigned effective addresses.</p> 
 <p>In 64-bit mode, 
 we use @ ( ' rgfi ' ) to read indices as signed 64-bit values. 
 In 32-bit mode, 
 we limit them to signed 32-bit values.</p> 
 <p>Note that, in 32-bit mode, 
 we call this function only when the address size is 32 bits. 
 When the address size is 16 bits, there is no SIB byte: 
 See Intel Vol. 2 Table 2-1.</p> 
 <p>The displacement is read as a signed values: 
 see AMD manual, Dec ' 17, Volume 3, Section 1.5.</p>*/
val x86_effective_addr_from_sib : (range(0, 4), sbits(48), bits(8), bits(2), sib) -> (option(string), int, sbits(64), {|0, 1, 4|}) effect {escape, rreg}
function x86_effective_addr_from_sib (proc_mode, temp_rip, rex_byte, mod_var, sib) =
let b : bits(3) = ((sib)[base]) in
let check_alignment? : bool = (false) in
let (flg, base, displacement, nrip_bytes) : (option(string), sbits(64), sbits(64), {|0, 1, 4|}) = { (match mod_var {
0 => { if (b) == (0b101)
then let (elem0, elem1, elem2, elem3) : (option(string), {|0|}, sbits(64), {|0, 4|}) = { (let (flg0, dword) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 4, sail_sign_extend(temp_rip, 64), None(), 1, ":X", check_alignment?, false)) in
if is_some(flg0)
then (flg0, 0, 0x0000_0000_0000_0000, 0)
else (None() : option(string), 0, dword, 4)) } in
(elem0, bits_of_int(elem1, 64), elem2, elem3)
else (None() : option(string), if (proc_mode) == (0)
then rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64), 0x0000_0000_0000_0000, 0) },
1 => { let (flg1, byte) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", check_alignment?, false)) in
if is_some(flg1)
then (flg1, 0x0000_0000_0000_0000, 0x0000_0000_0000_0000, 0)
else (None() : option(string), if (proc_mode) == (0)
then rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64), byte, 1) },
2 => { let (flg2, dword) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 4, sail_sign_extend(temp_rip, 64), None(), 1, ":X", check_alignment?, false)) in
if is_some(flg2)
then (flg2, 0x0000_0000_0000_0000, 0x0000_0000_0000_0000, 0)
else (None() : option(string), if (proc_mode) == (0)
then rgfi(unsigned(reg_index(b, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(b, 4)), 64), dword, 4) },
_ => (Some("mod-can-not-be-anything-other-than-0-1-or-2"), 0x0000_0000_0000_0000, 0x0000_0000_0000_0000, 0)
}) } in
let ix : bits(4) = (reg_index((sib)[index], rex_byte, 0b01)) in
let index : sbits(64) = { (match ix {
4 => 0x0000_0000_0000_0000,
_ => { if (proc_mode) == (0)
then rgfi(unsigned(ix))
else sail_sign_extend(truncate(rgfi(unsigned(ix)), 32), 64) }
}) } in
let scale : bits(2) = ((sib)[scale]) in
let scaled_index : int = (ash(signed(index), unsigned(scale))) in
let effective_addr : int = ((signed(base)) + (scaled_index)) in
(flg, effective_addr, displacement, nrip_bytes)

/*Calculate the displacement for 
 16-bit effective address calculation.*/
/*<p> 
 This is according to Intel manual, Mar ' 17, Vol. 2, Table 2-1. 
 </p> 
 <p> 
 The displacement is absent ( i.e. 0 ) when Mod is 00b. 
 An exception to this is when R/M is 110b, 
 in which case there is a 16-bit displacement that is added to the index. 
 This case is not handled by this function, 
 but is instead handled in 
 its caller function @ ( tsee x86-effective-addr-16 ) . 
 </p> 
 <p> 
 The displacement is a signed 8-bit value when Mod is 01b. 
 The displacement is a signed 16-bit value when Mod is 10b. 
 This function is not called when Mod is 11b. 
 </p> 
 <p> 
 If an error occurs when trying to read the displacement, 
 0 is returned as displacement, 
 but the caller ignores the returned displacement given the error. 
 </p> 
 <p> 
 This function is called only when the address size is 16 bits. 
 </p>*/
val x86_effective_addr_16_disp : (range(0, 4), sbits(48), bits(2)) -> (option(string), sbits(64), {|0, 1, 2|}) effect {escape}
function x86_effective_addr_16_disp (proc_mode, temp_rip, mod_var) =
(match mod_var {
0 => (None() : option(string), 0x0000_0000_0000_0000, 0),
1 => { let (flg, byte) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
if is_some(flg)
then (flg, 0x0000_0000_0000_0000, 0)
else (None() : option(string), byte, 1) },
2 => { let (flg, word) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 2, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
if is_some(flg)
then (flg, 0x0000_0000_0000_0000, 0)
else (None() : option(string), word, 2) },
_ => (Some("mod-value-wrong"), 0x0000_0000_0000_0000, 0)
}) : (option(string), sbits(64), {|0, 1, 2|})

/*Effective address calculation with 16-bit addressing.*/
/*<p> 
 This is according to Intel manual, Mar ' 17, Vol. 2, Table 2-1. 
 </p> 
 <p> 
 We assume that the additions in the table are modular, 
 even though the documentation is not clear in that respect. 
 So we simply apply @ ( ' n16 ' ) to the exact integer result. 
 This is in analogy to the use of @ ( ' n32 ' ) 
 for effective address calculation in 64-bit mode 
 when there is an address size override prefix: 
 see @ ( tsee x86-effective-addr-32/64 ) . 
 </p>*/
val x86_effective_addr_16 : (range(0, 4), sbits(48), bits(3), bits(2)) -> (option(string), bits(16), {|0, 1, 2|}) effect {escape, rreg}
function x86_effective_addr_16 (proc_mode, temp_rip, r_m, mod_var) =
(match r_m {
0 => { let bx : bits(16) = (rr16(0x3)) in
let si : bits(16) = (rr16(0x6)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bx)) + ((unsigned(si)) + (signed(disp))), 16), increment_rip_by) },
1 => { let bx : bits(16) = (rr16(0x3)) in
let di : bits(16) = (rr16(0x7)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bx)) + ((unsigned(di)) + (signed(disp))), 16), increment_rip_by) },
2 => { let bp : bits(16) = (rr16(0x5)) in
let si : bits(16) = (rr16(0x6)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bp)) + ((unsigned(si)) + (signed(disp))), 16), increment_rip_by) },
3 => { let bp : bits(16) = (rr16(0x5)) in
let di : bits(16) = (rr16(0x7)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bp)) + ((unsigned(di)) + (signed(disp))), 16), increment_rip_by) },
4 => { let si : bits(16) = (rr16(0x6)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(si)) + (signed(disp)), 16), increment_rip_by) },
5 => { let di : bits(16) = (rr16(0x7)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(di)) + (signed(disp)), 16), increment_rip_by) },
6 => { (match mod_var {
0 => { let (flg, disp) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 2, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), truncate(disp, 16), 2) },
_ => { let bp : bits(16) = (rr16(0x5)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bp)) + (signed(disp)), 16), increment_rip_by) }
}) : (option(string), bits(16), {|0, 1, 2|}) },
7 => { let bx : bits(16) = (rr16(0x3)) in
let (flg, disp, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2|}) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) in
if is_some(flg)
then (flg, 0x0000, 0)
else (None() : option(string), bits_of_int((unsigned(bx)) + (signed(disp)), 16), increment_rip_by) },
_ => (Some(":R/M-OUT-OF-RANGE"), 0x0000, 0)
}) : (option(string), bits(16), {|0, 1, 2|})

/*Effective address calculation with 32-bit and 64-bit addressing.*/
/*<p>Note that we do not add segment bases 
 ( such as the FS and GS bases, if FS and GS overrides are present ) 
 to the effective address computed in this function. 
 Addition of those segment base addresses is a part of the 
 segmentation process --- we handle that in the function @ ( see 
 ea-to-la ) that performs the segment address translation.</p> 
 
 <p>Quoting from Intel Vol 1, Sec 3.3.7:</p> 
 
 <p><em>In 64-bit mode, the effective address components are 
 added and the effective address is truncated ( See for example 
 the instruction LEA ) before adding the full 64-bit segment 
 base. The base is never truncated, regardless of addressing 
 mode in 64-bit mode.</em></p> 
 
 <p>Quoting Intel Vol. 1 Sec. 3.3.7 ( Address Calculations in 
 64-Bit Mode ) :</p> 
 
 <p><em>All 16-bit and 32-bit address calculations are 
 zero-extended in IA-32e mode to form 64-bit addresses. Address 
 calculations are first truncated to the effective address size 
 of the current mode ( 64-bit mode or compatibility mode ) , as 
 overridden by any address-size prefix. The result is then 
 zero-extended to the full 64-bit address width. Because of 
 this, 16-bit and 32-bit applications running in compatibility 
 mode can access only the low 4 GBytes of the 64-bit mode 
 effective addresses. Likewise, a 32-bit address generated in 
 64-bit mode can access only the low 4 GBytes of the 64-bit 
 mode effective addresses.</em></p> 
 
 <p>Also: Intel Vol 1, Section 3.3.7 says that we need 
 sign-extended displacements in effective address calculations. In 
 Lisp, sign-extension is implicit.</p> 
 
 <p>In 64-bit mode, instructions such as LEA use this function to 
 compute the effective address. LEA, at least, does not check 
 whether the generated address is canonical or not, which is why we 
 don ' t make the canonical-address-p check in this function.</p> 
 
 <p>In 64-bit mode, 
 we use @ ( ' rgfi-size ' ) to read bases as signed linear addresses, 
 which encode canonical linear addresses, 
 which are also effective addresses in 64-bit mode. 
 In 32-bit mode, 
 we use @ ( ' rr32 ' ) to read bases as unsigned effective addresses.</p>*/
val x86_effective_addr_32_64 : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), sib, bits(3)) -> (option(string), sbits(64), {|0, 1, 4|}) effect {escape, rreg}
function x86_effective_addr_32_64 (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) =
let (flg, addr, displacement, increment_rip_by) : (option(string), int, sbits(64), {|0, 1, 4|}) = { (match mod_var {
0 => { (match r_m {
4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
5 => { let (elem0, elem1, elem2, elem3) : (option(string), sbits(49), sbits(64), {|0, 4|}) = { (if (proc_mode) == (0)
then let (flg0, dword) : (option(string), sbits(64)) = (rime_size(0, select_address_size(proc_mode, None()), 4, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let (flg, next_rip) : (option(string), sbits(49)) = (add_to_iptr(0, temp_rip, bits_of_int((4) + (unsigned(num_imm_bytes)), 48))) in
if is_some(flg)
then (flg, 0b0_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0000, 0)
else (flg0, next_rip, dword, 4)
else let (elem0, elem1, elem2, elem3) : (option(string), {|0|}, sbits(64), {|0, 4|}) = { (let (flg, dword) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 4, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
if is_some(flg)
then (flg, 0, 0x0000_0000_0000_0000, 0)
else (None() : option(string), 0, dword, 4)) } in
(elem0, bits_of_int(elem1, 49), elem2, elem3)) } in
(elem0, signed(elem1), elem2, elem3) },
_ => (None() : option(string), signed(if (proc_mode) == (0)
then rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)), 0x0000_0000_0000_0000, 0)
}) : (option(string), int, sbits(64), {|0, 1, 4|}) },
1 => { (match r_m {
4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
_ => { let (elem0, elem1, elem2, elem3) : (option(string), sbits(64), sbits(64), {|1|}) = { (let (flg2, byte2) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 1, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let reg : sbits(64) = { (if (proc_mode) == (0)
then rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)) } in
(flg2, reg, byte2, 1)) } in
(elem0, signed(elem1), elem2, elem3) }
}) : (option(string), int, sbits(64), {|0, 1, 4|}) },
2 => { (match r_m {
4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
_ => { let (elem0, elem1, elem2, elem3) : (option(string), sbits(64), sbits(64), {|4|}) = { (let (flg1, dword) : (option(string), sbits(64)) = (rime_size(proc_mode, select_address_size(proc_mode, None()), 4, sail_sign_extend(temp_rip, 64), None(), 1, ":X", false, false)) in
let reg : sbits(64) = { (if (proc_mode) == (0)
then rgfi(unsigned(reg_index(r_m, rex_byte, 0b00)))
else sail_zero_extend(rr32(sail_zero_extend(r_m, 4)), 64)) } in
(flg1, reg, dword, 4)) } in
(elem0, signed(elem1), elem2, elem3) }
}) : (option(string), int, sbits(64), {|0, 1, 4|}) },
_ => (Some("mod-value-wrong"), 0, 0x0000_0000_0000_0000, 0)
}) } in
let dst_base : int = ((addr) + (signed(displacement))) in
let dst_base : sbits(64) = { (if (proc_mode) == (0)
then if p4
then sail_zero_extend(bits_of_int(dst_base, 32), 64)
else bits_of_int(dst_base, 64)
else sail_zero_extend(bits_of_int(dst_base, 32), 64)) } in
(flg, dst_base, increment_rip_by)

/*Effective address calculation.*/
/*<p> 
 This is a wrapper that calls 
 @ ( tsee x86-effective-addr-16 ) or @ ( tsee x86-effective-addr-32/64 ) 
 based on the address size. 
 </p>*/
val x86_effective_addr : (range(0, 4), prefixes, sbits(48), bits(8), bits(3), bits(2), sib, bits(3)) -> (option(string), sbits(64), {|0, 1, 2, 4|})

function x86_effective_addr (proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let p4 = (0x67 == prefixes[adr]);
    if 2 == select_address_size(proc_mode, Some(prefixes)) then {
        let (elem0, elem1, elem2) : (option(string), bits(16), {|0, 1, 2|}) = x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var);
        (elem0, sail_zero_extend(elem1, 64), elem2)
    } else {
        x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
    }
}

/*Functions to fetch and read operands from an instruction, 
 and to write results to appropriate registers/memory locations, 
 based on ModR/M, SIB, immediate, and/or displacement bytes.*/
/*Checking if alignment is enabled*/
/*<p> Source: Intel Manuals, Volume 3, Section 6.15, Exception 
 and Interrupt Reference:</p> 
 
 <h4>Interrupt 17 Alignment Check Exception ( #AC ) </h4> 
 
 <h5>Exception Class: Fault.</h5> 
 
 <blockquote>Description: Indicates that the processor detected an 
 unaligned memory operand when alignment checking was 
 enabled. Alignment checks are only carried out in data ( or stack ) 
 accesses ( not in code fetches or system segment accesses ) . An example 
 of an alignment-check violation is a word stored at an odd byte 
 address, or a doubleword stored at an address that is not an integer 
 multiple of 4.</blockquote> 
 
 <blockquote>Note that the alignment check exception ( #AC ) is 
 generated only for data types that must be aligned on word, 
 doubleword, and quadword boundaries. A general-protection 
 exception ( #GP ) is generated 128-bit data types that are not aligned 
 on a 16-byte boundary.</blockquote> 
 
 <blockquote>To enable alignment checking, the following conditions 
 must be true:</blockquote> 
 <ul> 
 <li> AM flag in CR0 register is set. </li> 
 <li> AC flag in the EFLAGS register is set. </li> 
 <li> The CPL is 3 ( protected mode or virtual-8086 mode ) . </li> 
 </ul> 
 
 <blockquote> Alignment-check exceptions ( #AC ) are generated only when 
 operating at privilege level 3 ( user mode ) . Memory references that 
 default to privilege level 0, such as segment descriptor loads, do not 
 generate alignment-check exceptions, even when caused by a memory 
 reference made from privilege level 3.</blockquote>*/
val alignment_checking_enabled_p : unit -> bool effect {escape}
function alignment_checking_enabled_p () =
let cr0 : bits(32) = (truncate(ctrs[0], 32)) in
let am : bits(1) = ((Mk_cr0bits(cr0))[am]) in
let ac : bits(2) = { (let rflags_var : bits(32) = (rflags) in
sail_zero_extend((Mk_rflagsbits(rflags_var))[ac], 2)) } in
let cpl : bits(2) = { ({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
}) } in
((am) == (0b1)) & (((ac) == (0b01)) & ((cpl) == (0b11)))

/*Read an operand from memory or a register.*/
/*<p> 
 Based on the ModR/M byte, 
 the operand is read from either a register or memory. 
 In the latter case, we calculate the effective address 
 and the we read the operand from it. 
 Besides returning the operand, 
 we also return the calculated effective address. 
 This is useful for instructions that modify the operand after reading it 
 ( e.g. the source/destination operand of ADD ) , 
 which pass the effective address calculated by this function 
 to @ ( tsee x86-operand-to-reg/mem ) ( which writes the result to memory ) . 
 </p>*/
val x86_operand_from_modr_m_and_sib_bytes : (range(0, 4), bits(1), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, range(0, 5), prefixes, sbits(48), bits(8), bits(3), bits(2), sib, bits(3)) -> (option(string), bits(128), {|0, 1, 2, 4|}, sbits(64))

function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (flg0, addr, increment_rip_by) : (option(string), sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (None() : option(string), 0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
        }
    };
    if is_some(flg0) then {
        (Some("x86-effective-addr-error"), 0x00000000000000000000000000000000, 0, 0x0000000000000000)
    } else {
        let (flg2, operand) : (option(string), bits(128)) = {
            if mod_var == 0b11 then {
                if reg_type == 0b0 then {
                    (None() : option(string), sail_zero_extend(rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), rex_byte), 128))
                } else {
                    (None() : option(string), xmmi_size(bits_of_int(operand_size, 5), reg_index(r_m, rex_byte, 0b00)))
                }
            } else {
                let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
                let addr_size = select_address_size(proc_mode, Some(prefixes));
                let base_reg = select_base_register(proc_mode, rex_byte, r_m, mod_var, sib);
                rme_size(proc_mode, addr_size, operand_size, addr, base_reg, seg_reg, ":R", check_alignment?, memory_ptr?)
                // match ext_check_memory_access(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, memory_ptr?) {
                //   Some(addr) => rme_size(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, struct { mem_ptr? = memory_ptr? }),
                //   None() => return (Some("Ext-Access-Check-Error"), 0x00000000000000000000000000000000, 0, 0x0000000000000000)
                // }
            }
        };
        if is_some(flg2) then {
            (Some("Rm-Size-Error"), 0x00000000000000000000000000000000, 0, 0x0000000000000000)
        } else {
            (None() : option(string), operand, increment_rip_by, addr)
        }
    }
}

/*Write an operand to memory or a general-purpose register.*/
/*<p> 
 Based on the ModR/M byte, 
 the operand is written to either a register or memory. 
 The address argument of this function is often 
 the effective address calculated and returned by 
 @ ( tsee x86-operand-from-modr/m-and-sib-bytes ) . 
 </p>*/
val x86_operand_to_reg_mem : (proc_mode, {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, bits(128), seg_reg_idx, sbits(64), prefixes, bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, prefixes, rex_byte, r_m, mod_var) = {
    if mod_var == 0b11 then {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), unsigned(operand), rex_byte);
        None() : option(string)
    } else {
        let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
        let addr_size = select_address_size(proc_mode, Some(prefixes));
        let base_reg : option(base_reg_idx) = None(); // TODO
        wme_size(proc_mode, addr_size, operand_size, addr, base_reg, seg_reg, operand, check_alignment?, memory_ptr?)
    }
}

/*Write an operand to memory or an XMM register.*/
/*<p> 
 Based on the ModR/M byte, 
 the operand is written to either a register or memory. 
 The address argument of this function is often 
 the effective address calculated and returned by 
 @ ( tsee x86-operand-from-modr/m-and-sib-bytes ) . 
 </p>*/
val x86_operand_to_xmm_mem : (proc_mode, {|4, 8, 16|}, bool, bits(128), seg_reg_idx, sbits(64), prefixes, bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_xmm_mem (proc_mode, operand_size, inst_ac?, operand, seg_reg, addr, prefixes, rex_byte, r_m, mod_var) = {
    if mod_var == 0b11 then {
        write_xmmi_size(bits_of_int(operand_size, 5), reg_index(r_m, rex_byte, 0b00), unsigned(operand));
        None() : option(string)
    } else {
        let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
        let addr_size = select_address_size(proc_mode, Some(prefixes));
        let base_reg : option(base_reg_idx) = None(); // TODO
        wme_size(proc_mode, addr_size, operand_size, addr, base_reg, seg_reg, operand, check_alignment?, false)
    }
}

/*Selecting the operand size for general-purpose instructions*/
/*<p>@ ( ' select-operand-size ' ) selects the operand size of the 
 instruction. It is cognizant of the instruction prefixes, the 
 @ ( ' rex ' ) byte, the operand type ( e.g., immediate operand or not ) , 
 and the default operand size ( obtained from the state ) .</p> 
 
 <p>This function was written by referring to the following: 
 <ol> 
 <li>Intel Manuals, Vol. 1, Section 3.6, Table 3-3</li> 
 <li>Intel Manuals, Vol. 1, Section 3.6, Table 3-4</li> 
 <li>Intel Manuals, Vol. 2, Section 2.2.1.2</li> 
 </ol> 
 </p> 
 
 <p><img src= ' res/images/Vol-1-Table-3-3-small.png ' width= ' 8% ' 
 height= ' 8% ' /> 
 
 <p><img src= ' res/images/Vol-1-Table-3-4-small.png ' width= ' 8% ' 
 height= ' 8% ' /> 
 
 The first image above has been captured from Volume 1: Basic Architecture, 
 Intel\ ( R\ ) 64 and IA-32 Architectures Software Developer ' s Manual, 
 Order Number: 253665-062US, March 2017.</p> 
 
 The second image above has been captured from Volume 1: Basic Architecture, 
 Intel\ ( R\ ) 64 and IA-32 Architectures Software Developer ' s Manual, 
 Combined Volumes: 1, 2A, 2B, 2C, 3A, 3B and 3C, Order Number: 
 325462-054US, April 2015.</p> 
 
 <i> 
 <ul> 
 <li>Setting REX.W can be used to determine the operand size but does 
 not solely determine operand width. Like the 66H size prefix, 64-bit 
 operand size override has no effect on byte-specific operations.</li> 
 
 <li>For non-byte operations: if a 66H prefix is used with prefix 
 \ ( REX.W = 1\ ) , 66H is ignored.</li> 
 
 <li>If a 66H override is used with REX and REX.W = 0, the operand size 
 is 16 bits.</li> 
 </ul> 
 </i> 
 
 <p>This function also includes three additional boolean parameters that serve 
 to accommodate instructions that do not quite follow the general rules 
 specified by the table above:</p> 
 
 <ul> 
 
 <li>The @ ( ' default64? ' ) parameter says whether the default operand size in 
 64-bit mode should be 64 bits instead of 32 bits. Examples are @ ( tsee 
 x86-near-jmp-op/en-m ) and @ ( tsee x86-push-general-register ) .</li> 
 
 <li>The @ ( ' ignore-rex? ' ) parameter says whether, in 64-bit mode, REX.W should 
 be ignored for the purpose of determining the operand size. Examples are 
 @ ( tsee x86-two-byte-jcc ) , @ ( tsee x86-near-jmp-op/en-m ) , and @ ( tsee 
 x86-push-general-register ) .</li> 
 
 <li>The @ ( ' ignore-p3-64? ' ) parameter says whether, in 64-bit mode, P3 should 
 be ignored for the purpose of determining the operand size. Examples are 
 @ ( tsee x86-two-byte-jcc ) and @ ( tsee x86-near-jmp-op/en-m ) .</li> 
 
 </ul>*/
val select_operand_size : (range(0, 4), bool, bits(8), bool, prefixes, bool, bool, bool) -> {|1, 2, 4, 8|} effect {escape}
function select_operand_size (proc_mode, byte_operand?, rex_byte, imm?, prefixes, default64?, ignore_rex?, ignore_p3_64?) =
if byte_operand?
then 1
else if (proc_mode) == (0)
then if (logbitp(3, rex_byte)) & (not_bool(ignore_rex?))
then if imm?
then 4
else 8
else if ((0x66) == ((prefixes)[opr])) & (not_bool(ignore_p3_64?))
then 2
else if default64?
then 8
else 4
else let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
let cs_d : bits(1) = ((Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) in
let p3? : bool = ((0x66) == ((prefixes)[opr])) in
if (cs_d) == (0b1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2

/*Check if the length of an instruction exceeds 15 bytes.*/
/*<p> 
 The maximum length of an instruction is 15 bytes; 
 a longer instruction causes a #GP ( 0 ) exception. 
 See AMD manual, Dec ' 17, Volume 2, Table 8-6. 
 This function is used to check this condition. 
 </p> 
 <p> 
 The @ ( ' start-rip ' ) argument is 
 the instruction pointer at the beginning of the instruction. 
 The @ ( ' temp-rip ' ) argument is generally 
 the instruction pointer just past the end of the instruction, 
 in which case the @ ( ' delta-rip ' ) argument is 0. 
 In the other cases, @ ( ' delta-rip ' ) is a small non-zero number, 
 and @ ( ' temp-rip + delta-rip ' ) is 
 the instruction pointer just past the end of the instruction. 
 </p> 
 <p> 
 This function returns @ ( ' nil ' ) if the length does not exceed 15 bytes. 
 Otherwise, this function returns the offending length ( a number above 15 ) , 
 which is useful for error reporting in the model. 
 </p>*/
val check_instruction_length : (sbits(48), sbits(48), bits(3)) -> option(int)
function check_instruction_length (start_rip, temp_rip, delta_rip) =
let start_rip : int = (signed(start_rip)) in
let temp_rip : int = (signed(temp_rip)) in
let delta_rip : nat = (unsigned(delta_rip)) in
let end_rip : sbits(49) = (bits_of_int((signed(bits_of_int(temp_rip, 48))) + (unsigned(bits_of_int(delta_rip, 3))), 49)) in
let length : sbits(50) = (bits_of_int((signed(end_rip)) - (signed(bits_of_int(start_rip, 48))), 50)) in
if (15) < (signed(length))
then Some(signed(length))
else None()

$endif
