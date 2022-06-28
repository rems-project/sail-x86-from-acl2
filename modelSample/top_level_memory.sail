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
val address_aligned_p : (sbits(48), {|1, 2, 4, 6, 8, 10, 16, 32, 64|}, bool) -> bool
function address_aligned_p (addr, operand_size, memory_ptr?) =
(match operand_size {
1 => true,
6 => (logand(addr, 0x0000_0000_0003)) == (0x0000_0000_0000),
10 => (logand(addr, 0x0000_0000_0007)) == (0x0000_0000_0000),
_ => { if (memory_ptr?) & ((operand_size) == (4))
then (logand(addr, 0x0000_0000_0001)) == (0x0000_0000_0000)
else (logand(addr, bits_of_int(check_range(0, 65, (operand_size) - (1)), 48))) == (0x0000_0000_0000) }
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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) in
if is_some(flg)
then (flg, 0)
else rml08(truncate(lin_addr, 48), r_x)

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
val rime08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), sbits(8)) effect {escape, rmem, rreg}
function rime08 (proc_mode, eff_addr, seg_reg, r_x) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) in
if is_some(flg)
then (flg, 0x00)
else riml08(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml16(truncate(lin_addr, 48), r_x)

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
val rime16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(16)) effect {escape, rmem, rreg}
function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) in
if is_some(flg)
then (flg, 0x0000)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000)
else riml16(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml32(truncate(lin_addr, 48), r_x)

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
val rime32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rime32) -> (option(string), sbits(32)) effect {escape, rmem, rreg}
function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000_0000)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) in
if is_some(flg)
then (flg, 0x0000_0000)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000_0000)
else riml32(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 6, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml48(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml64(truncate(lin_addr, 48), r_x)

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
val rime64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(64)) effect {escape, rmem, rreg}
function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000_0000_0000_0000)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) in
if is_some(flg)
then (flg, 0x0000_0000_0000_0000)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000_0000_0000_0000)
else riml64(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 10, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml80(truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 16, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml128(truncate(lin_addr, 48), r_x)

/*Read an unsigned value with the specified number of bytes 
 from memory via an effective address.*/
/*<p>The effective address is translated to a canonical linear address using 
 @ ( see ea-to-la ) . If this translation is successful and no other errors ( like 
 alignment errors ) occur, then @ ( see rml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored; see Intel manual, Dec ' 17, Volume 2, Section 4.8.1</p>*/
struct struct_rme_size = {
	mem_ptr? : bool
}
val rme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), string, bool, struct_rme_size) -> (option(string), int) effect {escape, rmem, rreg}
function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) in
if is_some(flg)
then (flg, 0)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0)
else rml_size(nbytes, truncate(lin_addr, 48), r_x)

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
val rime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), string, bool, struct_rime_size) -> (option(string), sbits(64)) effect {escape, rmem, rreg}
function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let r : bits(1) = (code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16))) in
(r) == (0b0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000_0000_0000_0000)
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) in
if is_some(flg)
then (flg, 0x0000_0000_0000_0000)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000_0000_0000_0000)
else riml_size(nbytes, truncate(lin_addr, 48), r_x)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) in
if is_some(flg)
then flg
else wml08(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1)) in
if is_some(flg)
then flg
else wiml08(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 2, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml16(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 2, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml16(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 4, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml32(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 4, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml32(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 6, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml48(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 8, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml64(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 8, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml64(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 10, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml80(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), 16, false)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml128(truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), nbytes, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wml_size(nbytes, truncate(lin_addr, 48), sailval)

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
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr : int = (loghead(16, unsigned(seg_hidden_attrs[seg_reg]))) in
let w : bits(1) = (data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16))) in
(w) == (0b0)))
then Some(":NON-WRITABLE-SEGMENT")
else let (flg, lin_addr) : (option(string), sbits(64)) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes)) in
if is_some(flg)
then flg
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(truncate(lin_addr, 48), nbytes, (keywords).mem_ptr?)))
then Some(":UNALIGNED-LINEAR-ADDRESS")
else wiml_size(nbytes, truncate(lin_addr, 48), sailval)

