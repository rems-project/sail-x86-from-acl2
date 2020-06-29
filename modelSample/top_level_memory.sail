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
val address_aligned_p : (int, int, bool) -> bool
function address_aligned_p (addr, operand_size, memory_ptr?) =
let addr = (addr) : int in
let operand_size = (operand_size) : int in
(match operand_size {
1 => true,
6 => (binary_logand(addr, 3)) == (0),
10 => (binary_logand(addr, 7)) == (0),
_ => if (memory_ptr?) & ((operand_size) == (4))
then (binary_logand(addr, 1)) == (0)
else (binary_logand(addr, (operand_size) - (1))) == (0)
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
val rme08 : (int, int, int, string, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme08 (proc_mode, eff_addr, seg_reg, r_x, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else rml08(lin_addr, r_x, 0)

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
val rime08 : (int, int, int, string, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rime08 (proc_mode, eff_addr, seg_reg, r_x, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else riml08(lin_addr, r_x, 0)

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
val rme16 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml16(lin_addr, r_x, 0)

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
val rime16 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else riml16(lin_addr, r_x, 0)

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
val rme32 : (int, int, int, string, bool, int, struct_rme32) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml32(lin_addr, r_x, 0)

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
val rime32 : (int, int, int, string, bool, int, struct_rime32) -> (option(string), int, int) effect {rreg, escape, rmem}
function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else riml32(lin_addr, r_x, 0)

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
val rme48 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme48 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 6, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml48(lin_addr, r_x, 0)

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
val rme64 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml64(lin_addr, r_x, 0)

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
val rime64 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else riml64(lin_addr, r_x, 0)

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
val rme80 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme80 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 10, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml80(lin_addr, r_x, 0)

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
val rme128 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme128 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 16, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml128(lin_addr, r_x, 0)

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
val rme_size : (int, int, int, int, string, bool, int, struct_rme_size) -> (option(string), int, int) effect {rreg, escape, rmem}
function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else rml_size(nbytes, lin_addr, r_x, 0)

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
val rime_size : (int, int, int, int, string, bool, int, struct_rime_size) -> (option(string), int, int) effect {rreg, escape, rmem}
function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) & (((r_x) == (":R")) & (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let r = (code_segment_descriptor_attributesbits_get_r(attr)) : int in
(r) == (0))))
then (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
else riml_size(nbytes, lin_addr, r_x, 0)

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
val wme08 : (int, int, int, int, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme08 (proc_mode, eff_addr, seg_reg, sailval, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else wml08(lin_addr, sailval, 0)

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
val wime08 : (int, int, int, int, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wime08 (proc_mode, eff_addr, seg_reg, sailval, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else wiml08(lin_addr, sailval, 0)

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
val wme16 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml16(lin_addr, sailval, 0)

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
val wime16 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wime16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 2, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wiml16(lin_addr, sailval, 0)

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
val wme32 : (int, int, int, int, bool, int, struct_wme32) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml32(lin_addr, sailval, 0)

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
val wime32 : (int, int, int, int, bool, int, struct_wime32) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wime32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 4, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wiml32(lin_addr, sailval, 0)

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
val wme48 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme48 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 6, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 6, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml48(lin_addr, sailval, 0)

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
val wme64 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml64(lin_addr, sailval, 0)

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
val wime64 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wime64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 8, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wiml64(lin_addr, sailval, 0)

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
val wme80 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme80 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 10, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 10, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml80(lin_addr, sailval, 0)

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
val wme128 : (int, int, int, int, bool, int) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme128 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, 16, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, 16, false)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml128(lin_addr, sailval, 0)

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
val wme_size : (int, int, int, int, int, bool, int, struct_wme_size) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wme_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wml_size(nbytes, lin_addr, sailval, 0)

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
val wime_size : (int, int, int, int, int, bool, int, struct_wime_size) -> (option(string), int) effect {rreg, escape, eamem, wmv}
function wime_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) =
if ((proc_mode) != (0)) & (((seg_reg) == (1)) | (let attr = (loghead(16, seg_hidden_attri(seg_reg, 0))) : int in
let w = (data_segment_descriptor_attributesbits_get_w(attr)) : int in
(w) == (0)))
then (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
else let (flg, lin_addr) = (ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0)) : (option(string), int) in
if is_some(flg)
then (flg, 0 : int)
else if not_bool((not_bool(check_alignment?)) | (address_aligned_p(lin_addr, nbytes, (keywords).mem_ptr?)))
then (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
else wiml_size(nbytes, lin_addr, sailval, 0)

