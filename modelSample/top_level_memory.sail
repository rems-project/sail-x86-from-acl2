$include <prelude.sail>
$include <string.sail>
$include "segmentation.sail"
/*Check the alignment of a linear address.*/
/*<p> 
 Besides the address to check for alignment, 
 this function takes as argument the operand size 
 ( from which the alignment to check is determined ) 
 and a flag indicating whether the address to check for alignment 
 contains a memory operand of the form m16:16, m16:32, or m16:64 
 ( see Intel manual, Mar ' 17, Volume 2, Section 3.1.1.3 ) . 
 </p> 
 <p> 
 Words, doublewords, quadwords, and double quadwords 
 must be aligned at boundaries of 2, 4, 8, or 16 bytes. 
 Memory pointers of the form m16:xx must be aligned so that 
 their xx portion is aligned as a word, doubleword, or quadword; 
 this automatically guarantees that their m16 portion is aligned as a word. 
 See Intel manual, Mar ' 17, Volume 1, Section 4.1.1. 
 See AMD manual, Dec ' 17, Volume 2, Table 8-7 
 ( note that the table does not mention explicitly 
 memory pointers of the form m16:64 ) . 
 </p> 
 <p> 
 If the operand size is 6, the operand must be an m16:32 pointer. 
 If the operand size is 10, the operand must an m16:64 pointer. 
 If the operand size is 4, it may be either an m16:16 pointer or not; 
 in this case, the @ ( ' memory-ptr? ' ) argument is used to 
 determine whether the address should be aligned 
 at a word or doubleword boundary. 
 If the operand size is 1, 2, 8, or 16, 
 it cannot be a memory pointer of the form m16:xx. 
 </p>*/
val address_aligned_p : (sbits(48), {|1, 2, 4, 6, 8, 10, 16|}, bool) -> bool effect {escape}
function address_aligned_p (addr, operand_size, memory_ptr?) =
let addr = (addr) : sbits(48) in
let operand_size = (the_range(0, 16, operand_size)) : {|1, 2, 4, 6, 8, 10, 16|} in
(match operand_size {
1 => true,
6 => (the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 3, 0)))) == (get_slice_int(8, 0, 0)),
10 => (the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 7, 0)))) == (get_slice_int(8, 0, 0)),
_ => if (memory_ptr?) & ((get_slice_int(8, operand_size, 0)) == (get_slice_int(8, 4, 0)))
then (the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 1, 0)))) == (get_slice_int(8, 0, 0))
else (the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, the_range(0, 15, (operand_size) - (1)), 0)))) == (get_slice_int(8, 0, 0))
}) : bool

/*Read an unsigned 8-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML08 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), int) effect {escape, rmem, rreg}
function rme08 (proc_mode, eff_addr, seg_reg, r_x) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else rml08(the_sbits(48, lin_addr), r_x)

/*Read a signed 8-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML08 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rime08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), sbits(10)) effect {escape, rmem, rreg}
function rime08 (proc_mode, eff_addr, seg_reg, r_x) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(10, 0, 0))
else let (elem0, elem1) = (let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, get_slice_int(9, 0, 0))
else let (elem0, elem1) = (riml08(the_sbits(48, lin_addr), r_x)) : (option(string), sbits(8)) in
(elem0, the_sbits(9, elem1))) : (option(string), sbits(9)) in
(elem0, the_sbits(10, elem1))

/*Read an unsigned 16-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML16 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int) effect {escape, rmem, rreg}
function rme16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml16(the_sbits(48, lin_addr), r_x)

/*Read a signed 16-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML16 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rime16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(19)) effect {escape, rmem, rreg}
function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(19, 0, 0))
else let (elem0, elem1) = (let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, get_slice_int(18, 0, 0))
else let (elem0, elem1) = (if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(17, 0, 0))
else let (elem0, elem1) = (riml16(the_sbits(48, lin_addr), r_x)) : (option(string), sbits(16)) in
(elem0, the_sbits(17, elem1))) : (option(string), sbits(17)) in
(elem0, the_sbits(18, elem1))) : (option(string), sbits(18)) in
(elem0, the_sbits(19, elem1))

/*Read an unsigned 32-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML32 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
struct struct_rme32 = {
	mem_ptr? : bool
}
val rme32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rme32) -> (option(string), int) effect {escape, rmem, rreg}
function rme32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml32(the_sbits(48, lin_addr), r_x)

/*Read a signed 32-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML32 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
struct struct_rime32 = {
	mem_ptr? : bool
}
val rime32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rime32) -> (option(string), sbits(35)) effect {escape, rmem, rreg}
function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(35, 0, 0))
else let (elem0, elem1) = (let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, get_slice_int(34, 0, 0))
else let (elem0, elem1) = (if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(33, 0, 0))
else let (elem0, elem1) = (riml32(the_sbits(48, lin_addr), r_x)) : (option(string), sbits(32)) in
(elem0, the_sbits(33, elem1))) : (option(string), sbits(33)) in
(elem0, the_sbits(34, elem1))) : (option(string), sbits(34)) in
(elem0, the_sbits(35, elem1))

/*Read an unsigned 48-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML48 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme48 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int) effect {escape, rmem, rreg}
function rme48 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 6, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml48(the_sbits(48, lin_addr), r_x)

/*Read an unsigned 64-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML64 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int) effect {escape, rmem, rreg}
function rme64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml64(the_sbits(48, lin_addr), r_x)

/*Read a signed 64-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML64 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rime64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(67)) effect {escape, rmem, rreg}
function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(67, 0, 0))
else let (elem0, elem1) = (let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, get_slice_int(66, 0, 0))
else let (elem0, elem1) = (if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(65, 0, 0))
else let (elem0, elem1) = (riml64(the_sbits(48, lin_addr), r_x)) : (option(string), sbits(64)) in
(elem0, the_sbits(65, elem1))) : (option(string), sbits(65)) in
(elem0, the_sbits(66, elem1))) : (option(string), sbits(66)) in
(elem0, the_sbits(67, elem1))

/*Read an unsigned 80-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML80 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme80 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int) effect {escape, rmem, rreg}
function rme80 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 10, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml80(the_sbits(48, lin_addr), r_x)

/*Read an unsigned 128-bit value from memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML128 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p>*/
val rme128 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int) effect {escape, rmem, rreg}
function rme128 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 16, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml128(the_sbits(48, lin_addr), r_x)

/*Read an unsigned value with the specified number of bytes 
 from memory via an effective address.*/
/*<p>The effective address is translated to a canonical linear address using 
 @ ( see ea-to-la ) . If this translation is successful and no other errors ( like 
 alignment errors ) occur, then @ ( see rml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored; see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1</p>*/
struct struct_rme_size = {
	mem_ptr? : bool
}
val rme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), string, bool, struct_rme_size) -> (option(string), int) effect {escape, rmem, rreg}
function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, 0 : {|0|})
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
else rml_size(nbytes, the_sbits(48, lin_addr), r_x)

/*Read a signed value with the specified number of bytes 
 from memory via an effective address.*/
/*<p>The effective address is translated to a canonical linear address using 
 @ ( see ea-to-la ) . If this translation is successful and no other errors ( like 
 alignment errors ) occur, then @ ( see riml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored; see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1</p>*/
struct struct_rime_size = {
	mem_ptr? : bool
}
val rime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), string, bool, struct_rime_size) -> (option(string), sbits(68)) effect {escape, rmem, rreg}
function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(r)) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(68, 0, 0))
else let (elem0, elem1) = (let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) : (option(string), sbits(67)) in
if is_some(flg)
then (flg, get_slice_int(67, 0, 0))
else let (elem0, elem1) = (if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(66, 0, 0))
else let (elem0, elem1) = (riml_size(nbytes, the_sbits(48, lin_addr), r_x)) : (option(string), sbits(65)) in
(elem0, the_sbits(66, elem1))) : (option(string), sbits(66)) in
(elem0, the_sbits(67, elem1))) : (option(string), sbits(67)) in
(elem0, the_sbits(68, elem1))

/*Write an unsigned 8-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML08 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme08 : (range(0, 4), sbits(64), range(0, 5), bits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wme08 (proc_mode, eff_addr, seg_reg, sailval) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else wml08(the_sbits(48, lin_addr), sailval)

/*Write a signed 8-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WIML08 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wime08 : (range(0, 4), sbits(64), range(0, 5), sbits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wime08 (proc_mode, eff_addr, seg_reg, sailval) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else wiml08(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 16-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML16 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme16 : (range(0, 4), sbits(64), range(0, 5), bits(16), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wme16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 2, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml16(the_sbits(48, lin_addr), sailval)

/*Write a signed 16-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WIML16 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wime16 : (range(0, 4), sbits(64), range(0, 5), sbits(16), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wime16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 2, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml16(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 32-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML32 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
struct struct_wme32 = {
	mem_ptr? : bool
}
val wme32 : (range(0, 4), sbits(64), range(0, 5), bits(32), bool, struct_wme32) -> option(string) effect {eamem, escape, rreg, wmv}
function wme32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 4, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml32(the_sbits(48, lin_addr), sailval)

/*Write a signed 32-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WIML32 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
struct struct_wime32 = {
	mem_ptr? : bool
}
val wime32 : (range(0, 4), sbits(64), range(0, 5), sbits(32), bool, struct_wime32) -> option(string) effect {eamem, escape, rreg, wmv}
function wime32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 4, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml32(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 48-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML48 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme48 : (range(0, 4), sbits(64), range(0, 5), bits(48), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wme48 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 6, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml48(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 64-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML64 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme64 : (range(0, 4), sbits(64), range(0, 5), bits(64), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wme64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 8, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml64(the_sbits(48, lin_addr), sailval)

/*Write a signed 64-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WIML64 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wime64 : (range(0, 4), sbits(64), range(0, 5), sbits(64), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wime64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 8, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml64(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 80-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML80 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme80 : (range(0, 4), sbits(64), range(0, 5), bits(80), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wme80 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 10, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml80(the_sbits(48, lin_addr), sailval)

/*Write an unsigned 128-bit value to memory via an effective address.*/
/*<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address. If this translation is successful and no other error 
 occurs ( like alignment errors ) , then @ ( see WML128 ) is 
 called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
val wme128 : (range(0, 4), sbits(64), range(0, 5), bits(128), bool) -> option(string) effect {eamem, escape, rreg, wmv}
function wme128 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), 16, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml128(the_sbits(48, lin_addr), sailval)

/*Write an unsigned value with the specified number of bytes 
 to memory via an effective address.*/
/*<p>The effective address is translated to a canonical linear address. If 
 this translation is successful and no other errors occur ( like alignment 
 errors ) , then @ ( see wml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
struct struct_wme_size = {
	mem_ptr? : bool
}
val wme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), nat, bool, struct_wme_size) -> option(string) effect {eamem, escape, rreg, wmv}
function wme_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), nbytes, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml_size(nbytes, the_sbits(48, lin_addr), sailval)

/*Write a signed value with the specified number of bytes 
 to memory via an effective address.*/
/*<p>The effective address is translated to a canonical linear address. If 
 this translation is successful and no other errors occur ( like alignment 
 errors ) , then @ ( see wiml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether write 
 access is allowed. In 32-bit mode, write access is allowed in data 
 segments ( DS, ES, FS, GS, and SS ) if the W bit in the segment 
 descriptor is 1; write access is disallowed in code segments ( this is 
 not explicitly mentioned in Intel manual, May ' 18, Volume 3, Section 
 3.4.5.1, but it seems reasonable ) . In 64-bit mode, the W bit is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) ; by 
 analogy, we allow write access to the code segment as well. 
 These checks may be slightly optimized using the invariant that 
 SS.W must be 1 when SS is loaded.</p>*/
struct struct_wime_size = {
	mem_ptr? : bool
}
val wime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), int, bool, struct_wime_size) -> option(string) effect {eamem, escape, rreg, wmv}
function wime_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0))) : bits(1) in
(unsigned(w)) == (0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) : (option(string), sbits(67)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(the_sbits(48, lin_addr), nbytes, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml_size(nbytes, the_sbits(48, lin_addr), sailval)

