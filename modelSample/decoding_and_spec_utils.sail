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
val read_iptr : (range(0, 4)) -> sbits(50) effect {escape}
function read_iptr (proc_mode) =
let iptr = (the_sbits(48, rip)) : sbits(48) in
(match proc_mode {
0 => the_sbits(50, iptr),
1 => the_bits(50, let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
if (unsigned(cs_d)) == (1)
then n32(the_bits(32, iptr))
else the_bits(32, n16(the_bits(16, iptr)))),
_ => get_slice_int(50, 0, 0)
}) : sbits(50)

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
val add_to_iptr : (range(0, 4), sbits(48), sbits(48)) -> (option(string), sbits(52)) effect {escape}
function add_to_iptr (proc_mode, iptr, delta) =
let iptr_plus_delta = (the_sbits(49, (signed(iptr)) + (signed(delta)))) : sbits(49) in
(match proc_mode {
0 => let (elem0, elem1) = (if canonical_address_p(the_sbits(48, iptr_plus_delta))
then (None() : option(string), the_sbits(50, iptr_plus_delta))
else (Some(":NON-CANONICAL-INSTRUCTION-POINTER"), get_slice_int(50, 0, 0))) : (option(string), sbits(50)) in
(elem0, the_sbits(52, elem1)),
1 => let (elem0, elem1) = (let cs_limit = (the_bits(32, loghead(32, unsigned(seg_hidden_limits[1])))) : bits(32) in
if ((0) <= (signed(iptr_plus_delta))) & ((signed(iptr_plus_delta)) <= (unsigned(cs_limit)))
then (None() : option(string), the_bits(32, iptr_plus_delta))
else (Some(":OUT-OF-SEGMENT-INSTRUCTION-POINTER"), get_slice_int(32, 0, 0))) : (option(string), bits(32)) in
(elem0, the_bits(52, elem1)),
_ => (Some(":UNIMPLEMENTED-PROC-MODE"), get_slice_int(52, 0, 0))
}) : (option(string), sbits(52))

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
0 => rip = the_bits(64, iptr),
1 => let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
if (unsigned(cs_d)) == (1)
then { rip = the_bits(64, n32(the_bits(32, iptr)))
}
else { let rip_var = (the_sbits(48, rip)) : sbits(48) in
let rip_new = (changeSlice(rip_var, 0, 16, n16(the_bits(16, iptr)))) : sbits(48) in
rip = the_bits(64, rip_new)
},
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
val read_sptr : (range(0, 4)) -> bits(64) effect {escape, rreg}
function read_sptr (proc_mode) =
let sptr = (rgfi(4)) : bits(64) in
(match proc_mode {
0 => sptr,
1 => the_bits(64, let ss_attr = (seg_hidden_attrs[2]) : bits(16) in
let ss_b = (data_segment_descriptor_attributesbits_get_d_b(ss_attr)) : bits(1) in
if (unsigned(ss_b)) == (1)
then n32(the_bits(32, sptr))
else the_bits(32, n16(the_bits(16, sptr)))),
_ => get_slice_int(64, 0, 0)
}) : bits(64)

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
val add_to_sptr : (range(0, 4), sbits(64), sbits(64)) -> (option(string), sbits(67)) effect {escape}
function add_to_sptr (proc_mode, sptr, delta) =
let sptr_plus_delta = (the_sbits(65, (signed(sptr)) + (signed(delta)))) : sbits(65) in
(match proc_mode {
0 => let (elem0, elem1) = (let sptr_plus_delta = (I64(the_bits(64, sptr_plus_delta))) : sbits(64) in
if canonical_address_p(the_sbits(48, sptr_plus_delta))
then (None() : option(string), the_sbits(65, sptr_plus_delta))
else (Some(":NON-CANONICAL-STACK-ADDRESS"), get_slice_int(65, 0, 0))) : (option(string), sbits(65)) in
(elem0, the_sbits(67, elem1)),
1 => let (elem0, elem1) = (let ss_limit = (seg_hidden_limits[2]) : bits(32) in
let ss_attr = (seg_hidden_attrs[2]) : bits(16) in
let ss_b = (data_segment_descriptor_attributesbits_get_d_b(ss_attr)) : bits(1) in
let ss_e = (data_segment_descriptor_attributesbits_get_e(ss_attr)) : bits(1) in
let ss_lower = (if (unsigned(ss_e)) == (1)
then (1) + (unsigned(ss_limit))
else 0) : int in
let ss_upper = (if (unsigned(ss_e)) == (1)
then get_slice_int(32, if (unsigned(ss_b)) == (1)
then 4294967295
else 65535, 0)
else ss_limit) : bits(32) in
let sptr_plus_delta = (if (unsigned(ss_b)) == (1)
then n32(the_bits(32, sptr_plus_delta))
else the_bits(32, n16(the_bits(16, sptr_plus_delta)))) : bits(32) in
if not_bool(((ss_lower) <= (unsigned(sptr_plus_delta))) & ((unsigned(sptr_plus_delta)) <= (unsigned(ss_upper))))
then (Some(":OUT-OF-SEGMENT-STACK-ADDRESS"), get_slice_int(32, 0, 0))
else (None() : option(string), sptr_plus_delta)) : (option(string), bits(32)) in
(elem0, the_bits(67, elem1)),
_ => (Some(":UNIMPLEMENTED-PROC-MODE"), get_slice_int(67, 0, 0))
}) : (option(string), sbits(67))

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
1 => let ss_attr = (seg_hidden_attrs[2]) : bits(16) in
let ss_b = (data_segment_descriptor_attributesbits_get_d_b(ss_attr)) : bits(1) in
if (unsigned(ss_b)) == (1)
then { write_rgfi(4, the_bits(64, n32(the_bits(32, sptr))))
}
else { let rsp_var = (rgfi(4)) : bits(64) in
let rsp_new = (changeSlice(rsp_var, 0, 16, n16(the_bits(16, sptr)))) : bits(64) in
write_rgfi(4, rsp_new)
},
_ => ()
}) : unit

/*Address size of an instruction, in bytes.*/
/*<p> 
 This is based on AMD manual, Dec ' 17, Volume 3, Table 1-3, 
 and AMD manual, Dec ' 17, Volume 2, Sections 4.7 and 4.8. 
 </p> 
 <p> 
 In 64-bit mode, the address size is 
 64 bits if there is no address override prefix, 
 32 bits if there is an address override prefix. 
 In 32-bit mode, the address size is 
 32 bits if either 
 ( i ) the default address size is 32 bits 
 and there is no address override prefix, or 
 ( ii ) the default address size is 16 bits 
 and there is an eddress override prefix; 
 otherwise, the address size is 16 bits. 
 In 32-bit mode, 
 the default address size is determined by the CS.D bit of the code segment: 
 32 bits if CS.D is 1, 16 bits if CS.D is 0. 
 </p> 
 <p> 
 The boolean argument of this function 
 indicates whether there is an override prefix or not. 
 </p>*/
val select_address_size : (range(0, 4), bool) -> {|8, 2, 4|} effect {escape}
function select_address_size (proc_mode, p4?) =
(match proc_mode {
0 => if p4?
then 4
else 8,
_ => let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
if (unsigned(cs_d)) == (1)
then if p4?
then 2
else 4
else if p4?
then 4
else 2
}) : {|8, 2, 4|}

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
val x86_effective_addr_from_sib : (range(0, 4), sbits(48), bits(8), bits(2), bits(8)) -> (option(string), int, sbits(71), {|0, 1, 4|}) effect {escape, rmem, rreg}
function x86_effective_addr_from_sib (proc_mode, temp_rip, rex_byte, mod_var, sib) =
let b = (sib_get_base(sib)) : bits(3) in
let check_alignment? = (false) : bool in
let (flg, base, displacement, nrip_bytes) = ((match mod_var {
0 => let (elem0, elem1, elem2, elem3) = (if (the_bits(8, b)) == (get_slice_int(8, 5, 0))
then let (elem0, elem1, elem2, elem3) = (let (flg0, dword) = (rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg0)
then (flg0, 0 : {|0|}, get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), 0 : {|0|}, the_sbits(69, dword), 4 : {|4|})) : (option(string), {|0|}, sbits(69), {|0, 4|}) in
(elem0, get_slice_int(64, elem1, 0), the_sbits(70, elem2), elem3)
else (None() : option(string), if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, b))), get_slice_int(70, 0, 0), 0 : {|0|})) : (option(string), bits(64), sbits(70), {|0, 4|}) in
(elem0, elem1, the_sbits(71, elem2), elem3),
1 => let (elem0, elem1, elem2, elem3) = (let (flg1, byte) = (rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg1)
then (flg1, get_slice_int(64, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, b))), the_sbits(69, byte), 1 : {|1|})) : (option(string), bits(64), sbits(69), {|0, 1|}) in
(elem0, elem1, the_sbits(71, elem2), elem3),
2 => let (elem0, elem1, elem2, elem3) = (let (flg2, dword) = (rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", check_alignment?, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg2)
then (flg2, get_slice_int(64, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(b, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, b))), the_sbits(69, dword), 4 : {|4|})) : (option(string), bits(64), sbits(69), {|0, 4|}) in
(elem0, elem1, the_sbits(71, elem2), elem3),
_ => (Some("mod-can-not-be-anything-other-than-0-1-or-2"), get_slice_int(64, 0, 0), get_slice_int(71, 0, 0), 0 : {|0|})
}) : (option(string), bits(64), sbits(71), {|0, 1, 4|})) : (option(string), bits(64), sbits(71), {|0, 1, 4|}) in
let ix = (reg_index(sib_get_index(sib), rex_byte, get_slice_int(2, 1, 0))) : bits(4) in
let index = ((match ix {
4 => get_slice_int(66, 0, 0),
_ => the_sbits(66, if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then the_bits(65, rgfi(unsigned(ix)))
else the_sbits(65, I32(the_bits(32, rgfi(unsigned(ix))))))
}) : sbits(66)) : sbits(66) in
let scale = (sib_get_scale(sib)) : bits(2) in
let scaled_index = (ash(signed(index), unsigned(scale))) : int in
let effective_addr = ((unsigned(base)) + (scaled_index)) : int in
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
val x86_effective_addr_16_disp : (range(0, 4), sbits(48), bits(2)) -> (option(string), sbits(71), {|0, 1, 2|}) effect {escape, rmem, rreg}
function x86_effective_addr_16_disp (proc_mode, temp_rip, mod_var) =
(match mod_var {
0 => (None() : option(string), get_slice_int(71, 0, 0), 0 : {|0|}),
1 => let (elem0, elem1, elem2) = (let (flg, byte) = (rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then (flg, get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), the_sbits(69, byte), 1 : {|1|})) : (option(string), sbits(69), {|0, 1|}) in
(elem0, the_sbits(71, elem1), elem2),
2 => let (elem0, elem1, elem2) = (let (flg, word) = (rime_size(proc_mode, 2, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then (flg, get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), the_sbits(69, word), 2 : {|2|})) : (option(string), sbits(69), {|0, 2|}) in
(elem0, the_sbits(71, elem1), elem2),
_ => (Some("mod-value-wrong"), get_slice_int(71, 0, 0), 0 : {|0|})
}) : (option(string), sbits(71), {|0, 1, 2|})

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
val x86_effective_addr_16 : (range(0, 4), sbits(48), bits(3), bits(2)) -> (option(string), bits(16), {|0, 1, 2|}) effect {escape, rmem, rreg}
function x86_effective_addr_16 (proc_mode, temp_rip, r_m, mod_var) =
(match r_m {
0 => let bx = (rr16(get_slice_int(4, 3, 0))) : bits(16) in
let si = (rr16(get_slice_int(4, 6, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bx)) + ((unsigned(si)) + (signed(disp))), 0)), increment_rip_by),
1 => let bx = (rr16(get_slice_int(4, 3, 0))) : bits(16) in
let di = (rr16(get_slice_int(4, 7, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bx)) + ((unsigned(di)) + (signed(disp))), 0)), increment_rip_by),
2 => let bp = (rr16(get_slice_int(4, 5, 0))) : bits(16) in
let si = (rr16(get_slice_int(4, 6, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bp)) + ((unsigned(si)) + (signed(disp))), 0)), increment_rip_by),
3 => let bp = (rr16(get_slice_int(4, 5, 0))) : bits(16) in
let di = (rr16(get_slice_int(4, 7, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bp)) + ((unsigned(di)) + (signed(disp))), 0)), increment_rip_by),
4 => let si = (rr16(get_slice_int(4, 6, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(si)) + (signed(disp)), 0)), increment_rip_by),
5 => let di = (rr16(get_slice_int(4, 7, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(di)) + (signed(disp)), 0)), increment_rip_by),
6 => (match mod_var {
0 => let (flg, disp) = (rime_size(proc_mode, 2, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(the_bits(16, disp)), 2 : {|2|}),
_ => let bp = (rr16(get_slice_int(4, 5, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bp)) + (signed(disp)), 0)), increment_rip_by)
}) : (option(string), bits(16), {|0, 1, 2|}),
7 => let bx = (rr16(get_slice_int(4, 3, 0))) : bits(16) in
let (flg, disp, increment_rip_by) = (x86_effective_addr_16_disp(proc_mode, temp_rip, mod_var)) : (option(string), sbits(71), {|0, 1, 2|}) in
if is_some(flg)
then (flg, get_slice_int(16, 0, 0), 0 : {|0|})
else (None() : option(string), n16(get_slice_int(16, (unsigned(bx)) + (signed(disp)), 0)), increment_rip_by),
_ => (Some(":R/M-OUT-OF-RANGE"), get_slice_int(16, 0, 0), 0 : {|0|})
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
val x86_effective_addr_32_64 : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(66), {|0, 1, 4|}) effect {escape, rmem, rreg}
function x86_effective_addr_32_64 (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) =
let (flg, addr, displacement, increment_rip_by) = ((match mod_var {
0 => let (elem0, elem1, elem2, elem3) = ((match r_m {
4 => let (elem0, elem1, elem2, elem3) = (x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib)) : (option(string), int, sbits(71), {|0, 1, 4|}) in
(elem0, elem1, the_sbits(72, elem2), elem3),
5 => let (elem0, elem1, elem2, elem3) = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then let (elem0, elem1, elem2, elem3) = (let (flg0, dword) = (rime_size(0, 4, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
let (flg, next_rip) = (add_to_iptr(0, temp_rip, get_slice_int(48, (4) + (unsigned(num_imm_bytes)), 0))) : (option(string), sbits(52)) in
if is_some(flg)
then (flg, get_slice_int(53, 0, 0), get_slice_int(69, 0, 0), 0 : {|0|})
else (flg0, the_sbits(53, next_rip), the_sbits(69, dword), 4 : {|4|})) : (option(string), sbits(53), sbits(69), {|0, 4|}) in
(elem0, the_sbits(54, elem1), elem2, elem3)
else let (elem0, elem1, elem2, elem3) = (let (flg, dword) = (rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
if is_some(flg)
then (flg, 0 : {|0|}, get_slice_int(69, 0, 0), 0 : {|0|})
else (None() : option(string), 0 : {|0|}, the_sbits(69, dword), 4 : {|4|})) : (option(string), {|0|}, sbits(69), {|0, 4|}) in
(elem0, get_slice_int(54, elem1, 0), elem2, elem3)) : (option(string), sbits(54), sbits(69), {|0, 4|}) in
(elem0, signed(elem1), the_sbits(72, elem2), elem3),
_ => (None() : option(string), unsigned(if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, r_m)))), get_slice_int(72, 0, 0), 0 : {|0|})
}) : (option(string), int, sbits(72), {|0, 1, 4|})) : (option(string), int, sbits(72), {|0, 1, 4|}) in
(elem0, elem1, the_sbits(73, elem2), elem3),
1 => let (elem0, elem1, elem2, elem3) = ((match r_m {
4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
_ => let (elem0, elem1, elem2, elem3) = (let (flg2, byte2) = (rime_size(proc_mode, 1, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
let reg = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, r_m)))) : bits(64) in
(flg2, reg, byte2, 1 : {|1|})) : (option(string), bits(64), sbits(68), {|1|}) in
(elem0, unsigned(elem1), the_sbits(71, elem2), elem3)
}) : (option(string), int, sbits(71), {|0, 1, 4|})) : (option(string), int, sbits(71), {|0, 1, 4|}) in
(elem0, elem1, the_sbits(73, elem2), elem3),
2 => let (elem0, elem1, elem2, elem3) = ((match r_m {
4 => x86_effective_addr_from_sib(proc_mode, temp_rip, rex_byte, mod_var, sib),
_ => let (elem0, elem1, elem2, elem3) = (let (flg1, dword) = (rime_size(proc_mode, 4, the_sbits(64, temp_rip), 1, ":X", false, struct {mem_ptr? = false})) : (option(string), sbits(68)) in
let reg = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then rgfi(unsigned(reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))
else the_bits(64, rr32(the_bits(4, r_m)))) : bits(64) in
(flg1, reg, dword, 4 : {|4|})) : (option(string), bits(64), sbits(68), {|4|}) in
(elem0, unsigned(elem1), the_sbits(71, elem2), elem3)
}) : (option(string), int, sbits(71), {|0, 1, 4|})) : (option(string), int, sbits(71), {|0, 1, 4|}) in
(elem0, elem1, the_sbits(73, elem2), elem3),
_ => (Some("mod-value-wrong"), 0 : {|0|}, get_slice_int(73, 0, 0), 0 : {|0|})
}) : (option(string), int, sbits(73), {|0, 1, 4|})) : (option(string), int, sbits(73), {|0, 1, 4|}) in
let dst_base = ((addr) + (signed(displacement))) : int in
let dst_base = (if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then the_sbits(66, if p4
then the_bits(65, n32(get_slice_int(32, dst_base, 0)))
else the_sbits(65, n64_to_i64(n64(get_slice_int(64, dst_base, 0)))))
else the_bits(66, n32(get_slice_int(32, dst_base, 0)))) : sbits(66) in
(flg, dst_base, increment_rip_by)

/*Effective address calculation.*/
/*<p> 
 This is a wrapper that calls 
 @ ( tsee x86-effective-addr-16 ) or @ ( tsee x86-effective-addr-32/64 ) 
 based on the address size. 
 </p>*/
val x86_effective_addr : (range(0, 4), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), sbits(67), {|0, 1, 2, 4|}) effect {escape, rmem, rreg}
function x86_effective_addr (proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) =
if (get_slice_int(8, 2, 0)) == (get_slice_int(8, select_address_size(proc_mode, if p4
then true
else false), 0))
then let (elem0, elem1, elem2) = (x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var)) : (option(string), bits(16), {|0, 1, 2|}) in
(elem0, the_bits(67, elem1), elem2)
else let (elem0, elem1, elem2) = (x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)) : (option(string), sbits(66), {|0, 1, 4|}) in
(elem0, the_sbits(67, elem1), elem2)

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
let cr0 = (n32(the_bits(32, ctrs[0]))) : bits(32) in
let am = (cr0bits_get_am(cr0)) : bits(1) in
let ac = (let rflags_var = (rflags) : bits(32) in
the_bits(2, rflagsbits_get_ac(rflags_var))) : bits(2) in
let cpl = (let x86 = (()) : unit in
segment_selectorbits_get_rpl(seg_visibles[1])) : bits(2) in
((the_bits(8, am)) == (get_slice_int(8, 1, 0))) & (((the_bits(8, ac)) == (get_slice_int(8, 1, 0))) & ((the_bits(8, cpl)) == (get_slice_int(8, 3, 0))))

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
val x86_operand_from_modr_m_and_sib_bytes : (range(0, 4), bits(1), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, range(0, 5), bool, sbits(48), bits(8), bits(3), bits(2), bits(8), bits(3)) -> (option(string), int, {|0, 1, 2, 4|}, sbits(66)) effect {escape, rmem, rreg}
function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) =
let (flg0, addr, increment_rip_by) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then (None() : option(string), get_slice_int(68, 0, 0), 0 : {|0|})
else let (elem0, elem1, elem2) = (x86_effective_addr(proc_mode, p4?, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)) : (option(string), sbits(67), {|0, 1, 2, 4|}) in
(elem0, the_sbits(68, elem1), elem2)) : (option(string), sbits(68), {|0, 1, 2, 4|}) in
let addr = (the_sbits(64, addr)) : sbits(64) in
let increment_rip_by = (the_range(0, 4, increment_rip_by)) : {|0, 1, 2, 4|} in
if is_some(flg0)
then (Some("x86-effective-addr-error"), 0 : {|0|}, 0 : {|0|}, get_slice_int(66, 0, 0))
else let (elem0, elem1, elem2, elem3) = (let (flg2, operand) = (if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then let (elem0, elem1) = (if (reg_type) == (get_slice_int(1, 0, 0))
then (None() : option(string), the_bits(128, rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), rex_byte)))
else (None() : option(string), xmmi_size(get_slice_int(5, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0))))) : (option(string), bits(128)) in
(elem0, unsigned(elem1))
else let check_alignment? = ((inst_ac?) & (alignment_checking_enabled_p())) : bool in
rme_size(proc_mode, operand_size, addr, seg_reg, ":R", check_alignment?, struct {mem_ptr? = memory_ptr?})) : (option(string), int) in
if is_some(flg2)
then (Some("Rm-Size-Error"), 0 : {|0|}, 0 : {|0|}, get_slice_int(65, 0, 0))
else (None() : option(string), operand, increment_rip_by, the_sbits(65, addr))) : (option(string), int, {|0, 1, 2, 4|}, sbits(65)) in
(elem0, elem1, elem2, the_sbits(66, elem3))

/*Write an operand to memory or a general-purpose register.*/
/*<p> 
 Based on the ModR/M byte, 
 the operand is written to either a register or memory. 
 The address argument of this function is often 
 the effective address calculated and returned by 
 @ ( tsee x86-operand-from-modr/m-and-sib-bytes ) . 
 </p>*/
val x86_operand_to_reg_mem : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string) effect {eamem, escape, rreg, wmv, wreg}
function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, rex_byte, r_m, mod_var) =
if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then let x86 = (write_rgfi_size(get_slice_int(4, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), operand, rex_byte)) : unit in
None() : option(string)
else let check_alignment? = ((inst_ac?) & (alignment_checking_enabled_p())) : bool in
let flg = (wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct {mem_ptr? = memory_ptr?})) : option(string) in
flg

/*Write an operand to memory or an XMM register.*/
/*<p> 
 Based on the ModR/M byte, 
 the operand is written to either a register or memory. 
 The address argument of this function is often 
 the effective address calculated and returned by 
 @ ( tsee x86-operand-from-modr/m-and-sib-bytes ) . 
 </p>*/
val x86_operand_to_xmm_mem : (range(0, 4), {|4, 8, 16|}, bool, nat, range(0, 5), sbits(64), bits(8), bits(3), bits(2)) -> option(string) effect {eamem, escape, rreg, wmv}
function x86_operand_to_xmm_mem (proc_mode, operand_size, inst_ac?, operand, seg_reg, addr, rex_byte, r_m, mod_var) =
if (the_bits(8, mod_var)) == (get_slice_int(8, 3, 0))
then let x86 = (write_xmmi_size(get_slice_int(5, operand_size, 0), reg_index(r_m, rex_byte, get_slice_int(2, 0, 0)), operand)) : unit in
None() : option(string)
else let check_alignment? = ((inst_ac?) & (alignment_checking_enabled_p())) : bool in
let flg = (wme_size(proc_mode, operand_size, addr, seg_reg, operand, check_alignment?, struct {mem_ptr? = false})) : option(string) in
flg

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
val select_operand_size : (range(0, 4), bool, bits(8), bool, bits(52), bool, bool, bool) -> {|8, 1, 2, 4|} effect {escape}
function select_operand_size (proc_mode, byte_operand?, rex_byte, imm?, prefixes, default64?, ignore_rex?, ignore_p3_64?) =
if byte_operand?
then 1
else if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then if (logbitp(3, rex_byte)) & (not_bool(ignore_rex?))
then if imm?
then 4
else 8
else if ((get_slice_int(8, 102, 0)) == (prefixes_get_opr(prefixes))) & (not_bool(ignore_p3_64?))
then 2
else if default64?
then 8
else 4
else let cs_attr = (seg_hidden_attrs[1]) : bits(16) in
let cs_d = (code_segment_descriptor_attributesbits_get_d(cs_attr)) : bits(1) in
let p3? = ((get_slice_int(8, 102, 0)) == (prefixes_get_opr(prefixes))) : bool in
if (unsigned(cs_d)) == (1)
then if p3?
then 2
else 4
else if p3?
then 4
else 2

/*Segment register to use for an instruction operand in memory.*/
/*<p> 
 If there is a segment register override prefix, 
 the prefix determines the segment register, 
 according to Intel manual, Mar ' 17, Volume 2, Section 2.1.1. 
 </p> 
 <p> 
 Otherwise, we use the default segment selection rules 
 in Intel manual, May ' 18, Volume 1, Table 3-5. 
 Since we only call this function for instruction operands, 
 the CS rule does not apply. 
 The ES rule applies to string instructions, 
 but our model does not use this function 
 to determine the ES segment for string instructions 
 ( which cannot be overridden, 
 at least for the string instructions we currently support ) , 
 so this function does not take the ES rule into account either. 
 So the result is either SS or DS, 
 based on whether the base register is one of rSP and rBP or not: 
 this determination is made based on 
 Intel manual, May ' 18, Volume 2, Table 2-1 if the address size is 16 bits, 
 and Intel manual, May ' 18, Volume 2, Table 2-2 otherwise. 
 However, when Mod is not 11b and R/M is 100b, 
 the notation [--][--] in Table 2-2 indicates the use of a SIB byte: 
 according to Intel manual, May ' 18, Volume 2, Table 2-3, 
 when the Base field of the SIB byte is 100b, 
 the base register is rSP, 
 and thus in this case the default segment register is SS. 
 </p> 
 <p> 
 Note that here we may recalculate the address size 
 even if that has already been calculated as part of 
 the decoding of the instruction whose operand we are accessing. 
 Thus, it may be possible to optimize the overall code. 
 </p>*/
val select_segment_register : (range(0, 4), bits(8), bool, bits(2), bits(3), bits(8)) -> {|0, 1, 2, 3, 4, 5|} effect {escape}
function select_segment_register (proc_mode, p2, p4?, mod_var, r_m, sib) =
(match p2 {
46 => 1,
54 => 2,
62 => 3,
38 => 0,
100 => 4,
101 => 5,
_ => let addr_size = (select_address_size(proc_mode, p4?)) : {|8, 2, 4|} in
if (the_range(0, 5, addr_size)) == (2)
then if (not_bool((unsigned(mod_var)) == (3))) & (((the_range(0, 5, unsigned(r_m))) == (2)) | ((the_range(0, 5, unsigned(r_m))) == (3)))
then 2
else 3
else if ((((unsigned(mod_var)) == (1)) | ((unsigned(mod_var)) == (2))) & ((the_range(0, 5, unsigned(r_m))) == (5))) | ((not_bool((unsigned(mod_var)) == (3))) & (((the_range(0, 5, unsigned(r_m))) == (4)) & ((the_range(0, 5, unsigned(sib_get_base(sib)))) == (4))))
then 2
else 3
}) : {|0, 1, 2, 3, 4, 5|}

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
val check_instruction_length : (sbits(48), sbits(48), bits(3)) -> option(int) effect {escape}
function check_instruction_length (start_rip, temp_rip, delta_rip) =
let start_rip = (signed(start_rip)) : int in
let temp_rip = (signed(temp_rip)) : int in
let delta_rip = (unsigned(delta_rip)) : int in
let end_rip = (the_sbits(49, (signed(the_sbits(48, temp_rip))) + (unsigned(the_bits(3, delta_rip))))) : sbits(49) in
let length = (the_sbits(50, (signed(end_rip)) - (signed(the_sbits(48, start_rip))))) : sbits(50) in
if (15) < (signed(length))
then Some(signed(length))
else None()

