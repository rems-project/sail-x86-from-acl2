
/*! Check the alignment of a linear address.
<p> 
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
 </p> */
function address_aligned_p (addr, operand_size, memory_ptr?) = {
    (match operand_size {
      1 => true,
      6 => logand(addr, 0x000000000003) == 0x000000000000,
      10 => logand(addr, 0x000000000007) == 0x000000000000,
      _ => {
          if memory_ptr? & operand_size == 4 then {
              logand(addr, 0x000000000001) == 0x000000000000
          } else {
              logand(addr, bits_of_int(check_range(0, 65, operand_size - 1), 48)) == 0x000000000000
          }
      }
    }) : bool
}

/*! Read an unsigned 8-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML08 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme08 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string) -> bits(8)

function rme08 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x) = {
    load_bytes_from_ea(proc_mode, addr_size, 1, eff_addr, base_reg, seg_reg, r_x, false, false)
}

/* Read a signed 8-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML08 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
overload rime08 = {rme08}

/*! Read an unsigned 16-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML16 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme16 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(16)

function rme16 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 2, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

/* Read a signed 16-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML16 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
overload rime16 = {rme16}

/*! Read an unsigned 32-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML32 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme32 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> bits(32)

function rme32 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    load_bytes_from_ea(proc_mode, addr_size, 4, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?)
}

/* Read a signed 32-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML32 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
overload rime32 = {rme32}

/*! Read an unsigned 48-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML48 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme48 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(48)

function rme48 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 6, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

/*! Read an unsigned 64-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML64 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme64 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(64)

function rme64 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 8, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

/* Read a signed 64-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RIML64 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
overload rime64 = {rme64}

/*! Read an unsigned 80-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML80 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme80 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(80)

function rme80 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 10, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

/*! Read an unsigned 128-bit value from memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
 linear address using @ ( see ea-to-la ) . If this translation is 
 successful and no other errors ( like alignment errors ) occur, then 
 @ ( see RML128 ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored ( see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1 ) .</p> */
val rme128 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(128)

function rme128 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 16, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

/*! Read an unsigned value with the specified number of bytes 
 from memory via an effective address.
<p>The effective address is translated to a canonical linear address using 
 @ ( see ea-to-la ) . If this translation is successful and no other errors ( like 
 alignment errors ) occur, then @ ( see rml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored; see Intel manual, Dec ' 17, Volume 2, Section 4.8.1</p> */
val rme_size : (proc_mode, address_size, {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> bits(128)

function rme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    sail_zero_extend(load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?), 128)
}

/*! Read a signed value with the specified number of bytes 
 from memory via an effective address.
<p>The effective address is translated to a canonical linear address using 
 @ ( see ea-to-la ) . If this translation is successful and no other errors ( like 
 alignment errors ) occur, then @ ( see riml-size ) is called.</p> 
 <p>Prior to the effective address translation, we check whether read 
 access is allowed. The only case in which it is not allowed is when a 
 read access is attempted on an execute-only code segment, in 32-bit 
 mode. In 64-bit mode, the R bit of the code segment descriptor is 
 ignored; see Atmel manual, Dec ' 17, Volume 2, Section 4.8.1</p> */
val rime_size : (proc_mode, address_size, {|1, 2, 4, 6, 8|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> bits(64)

function rime_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    sail_sign_extend(load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?), 64)
}

/*! Write an unsigned 8-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme08 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(8)) -> unit

function wme08 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data) = {
    store_bytes_to_ea(proc_mode, addr_size, 1, eff_addr, base_reg, seg_reg, data, false, false)
}

/* Write a signed 8-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
overload wime08 = {wme08}

/*! Write an unsigned 16-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme16 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(16), bool) -> unit

function wme16 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 2, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

/* Write a signed 16-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
overload wime16 = {wme16}

/*! Write an unsigned 32-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme32 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(32), bool, bool) -> unit

function wme32 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    store_bytes_to_ea(proc_mode, addr_size, 4, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?)
}

/* Write a signed 32-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
overload wime32 = {wme32}

/*! Write an unsigned 48-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme48 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(48), bool) -> unit

function wme48 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 6, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

/*! Write an unsigned 64-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme64 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(64), bool) -> unit

function wme64 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 8, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

/* Write a signed 64-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
overload wime64 = {wme64}

/*! Write an unsigned 80-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme80 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(80), bool) -> unit

function wme80 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 10, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

/*! Write an unsigned 128-bit value to memory via an effective address.
<p>The effective address @ ( ' eff-addr ' ) is translated to a canonical 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme128 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(128), bool) -> unit

function wme128 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 16, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

/*! Write an unsigned value with the specified number of bytes 
 to memory via an effective address.
<p>The effective address is translated to a canonical linear address. If 
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
 SS.W must be 1 when SS is loaded.</p> */
val wme_size : forall 'nbytes, 'nbytes in {1, 2, 4, 6, 8, 10, 16}.
  (proc_mode, address_size, int('nbytes), sbits(64), option(base_reg_idx), seg_reg_idx, bits(128), bool, bool) -> unit

function wme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    store_bytes_to_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, truncate(data, 8 * nbytes), check_alignment?, mem_ptr?)
}

/* Write a signed value with the specified number of bytes 
 to memory via an effective address.
<p>The effective address is translated to a canonical linear address. If 
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
 SS.W must be 1 when SS is loaded.</p> */
overload wime_size = {wme_size}
