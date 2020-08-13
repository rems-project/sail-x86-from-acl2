$include <prelude.sail>
$include <string.sail>

/*Functions that enable the use of extended GPRs using the 
 @ ( ' rex ' ) byte*/
/*Using the REX prefix to access general-purpose registers*/
/*<p>In 64-bit mode, in addition to generating 64-bit operand sizes, 
 the REX prefix is used to reference registers R8 to R15. Instructions that 
 include REX prefixes can access these registers if the relevant W, R, X, or 
 B bit in the REX prefix is set. E.g., let R be the relevant bit in the REX 
 prefix and let R be set --- so @ ( ' index ' ) = @ ( ` *r* ` ) for this function. If 
 @ ( ' reg ' ) = 0 ( which, in the non-REX world, would refer to rAX ) , 
 @ ( ' reg-index ' ) would give us the register index corresponding to the 
 register r8. If R is not set, @ ( ' reg-index ' ) will give us the index 
 corresponding to rAX.</p> 
 
 <p>In 32-bit mode, the REX prefix is absent. This function can be used in 
 32-bit mode, by passing 0 as REX.</p>*/
val reg_index : (int, int, int) -> int effect {escape}
function reg_index (reg, rex_byte, index) =
if logbitp(index, rex_byte)
then binary_logior(8, n03(reg))
else n03(reg)

/*Functions to read/write 8/16/32/64 bits into the registers*/
/*<p>@ ( see rr08 ) , @ ( see rr16 ) , @ ( see rr32 ) , and @ ( see rr64 ) 
 will read the contents of the GPRs as natural numbers. Remember that 
 @ ( ' rgfi ' ) will return an @ ( see i64p ) value.</p> 
 
 <p>Here is an example:</p> 
 <br/> 
 <tt> 
 \ ( !rgfi 0 -1 x86\ ) ;; Write -1 to register 0 
 </tt> 
 
 <p><tt>\ ( rr64 0 x86\ ) </tt> returns @ ( ' 18446744073709551615 ' ) and 
 <tt> ( rgfi 0 x86 ) </tt> returns @ ( ' -1 ' ) . Note that 
 @ ( ' 18446744073709551615 ' ) is a bignum in CCL. This is precisely the 
 reason why we declared registers as @ ( ' i64p ' ) instead of @ ( ' n64p ' ) ; 
 @ ( ' -1 ' ) would be stored as a bignum if the registers were 
 @ ( ' n64p ' ) . </p> 
 
 <p>Similarly, @ ( see wr08 ) , @ ( see wr16 ) , @ ( see wr32 ) , and @ ( see wr64 ) 
 are used to write natural numbers into the GPRs.</p>*/
/*Read from byte general-purpose registers*/
/*<p><i>Source: Intel Manuals, Vol. 1, Section 
 3.4.1.1 ( General-Purpose Registers in 64-bit Mode ) </i></p> 
 
 <blockquote>In 64-bit mode, there are limitations on accessing byte 
 registers. An instruction cannot reference legacy high-bytes ( for example: AH, 
 BH, CH, DH ) and one of the new byte registers at the same time ( for example: 
 the low byte of the RAX register ) . However, instructions may reference legacy 
 low-bytes ( for example: AL, BL, CL or DL ) and new byte registers at the same 
 time ( for example: the low byte of the R8 register, or R8L ) . The architecture 
 enforces this limitation by changing high-byte references ( AH, BH, CH, DH ) to 
 low byte references ( BPL, SPL, DIL, SIL: the low 8 bits for RBP, RSP, RDI and 
 RSI ) for instructions using a REX prefix.</blockquote> 
 
 <p>In other words, without the REX prefix, indices 0-7 refer to byte registers 
 AL, CL, DL, BL, AH, CH, DH, and BH, whereas with the REX prefix, indices 0-15 
 refer to AL, CL, DL, BL, SPL, BPL, SIL, DIL, R8L, R9L, R10L, R11L, R12L, R13L, 
 R14L, R15L. This applies to 64-bit mode.</p> 
 
 <p>In 32-bit mode, this function is called with 0 as REX.</p>*/
val rr08 : (int, int, int) -> int effect {escape, rreg}
function rr08 (reg, rex, x86) =
if (not_bool((rex) == (0))) | ((reg) < (4))
then let qword = (rgfi(reg, 0)) : int in
n08(qword)
else let qword = (rgfi((reg) - (4), 0)) : int in
unsigned(get_slice_int(8, qword, 8))

/*Read from word general-purpose registers*/
val rr16 : (int, int) -> int effect {escape, rreg}
function rr16 (reg, x86) =
n16(rgfi(reg, 0))

/*Read from doubleword general-purpose registers*/
val rr32 : (int, int) -> int effect {escape, rreg}
function rr32 (reg, x86) =
n32(rgfi(reg, 0))

/*Read from quadword general-purpose registers*/
/*<p>This function is used only in 64-bit mode.</p>*/
val rr64 : (int, int) -> int effect {escape, rreg}
function rr64 (reg, x86) =
n64(rgfi(reg, 0))

/*Writing to byte general-purpose registers*/
/*<p>Note Intel Vol. 1 Sec. 3.4.1.1 
 p. 3-17, which says the following about 64-bit mode:</p> 
 
 <p><em>8-bit and 16-bit operands generate an 8-bit or 16-bit result. 
 The upper 56 or 48 bits ( respectively ) of the destination general-purpose 
 register are not modified by the operation.</em></p> 
 
 <p>This is also confirmed by AMD manual, Jun ' 15, Vol. 3, App. B.1, under 
 &lsquo;No Extension of 8-Bit and 16-Bit Results&rsquo;.</p> 
 
 <p>In 32-bit mode, the upper 32 bits are undefined, as specified by 
 the following quote from the same page as above:</p> 
 
 <p><em>Because the upper 32 bits of 64-bit general-purpose registers are 
 undefined in 32-bit modes, the upper 32 bits of any general-purpose 
 register are not preserved when switching from 64-bit mode to a 32-bit 
 mode ( to protected mode or compatibility mode ) . Software must not depend on 
 these bits to maintain a value after a 64-bit to 32-bit mode 
 switch.</em></p> 
 
 <p>In 32-bit mode, this function is called with 0 as REX. Since in 32-bit 
 mode the high 32 bits of general-purpose registers are not accessible, it 
 is fine for this function to leave those bits unchanged, as opposed to, for 
 example, setting them to undefined values as done by the semantic functions 
 of certain instructions for certain flags. The switching from 32-bit mode 
 to 64-bit mode ( when modeled ) will set the high 32 bits of general-purpose 
 registers to undefined values.</p>*/
val wr08 : (int, int, int, int) -> int effect {escape, rreg, wreg}
function wr08 (reg, rex, byte, x86) =
if (not_bool((rex) == (0))) | ((reg) < (4))
then let qword = (rgfi(reg, 0)) : int in
write_rgfi(reg, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 0, 8, byte)), 0)
else let index = ((reg) - (4)) : int in
let qword = (rgfi(index, 0)) : int in
write_rgfi(index, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 8, 8, byte)), 0)

/*Write to word general-purpose registers*/
/*<p>Note Intel Vol. 1 Sec. 3.4.1.1 
 p. 3-17, which says the following about 64-bit mode:</p> 
 
 <p><em>8-bit and 16-bit operands generate an 8-bit or 16-bit result. 
 The upper 56 or 48 bits ( respectively ) of the destination general-purpose 
 register are not modified by the operation.</em></p> 
 
 <p>This is also confirmed by AMD manual, Jun ' 15, Vol. 3, App. B.1, under 
 &lsquo;No Extension of 8-Bit and 16-Bit Results&rsquo;.</p> 
 
 <p>In 32-bit mode, the upper 32 bits are undefined, as specified by 
 the following quote from the same page as above:</p> 
 
 <p><em>Because the upper 32 bits of 64-bit general-purpose registers are 
 undefined in 32-bit modes, the upper 32 bits of any general-purpose 
 register are not preserved when switching from 64-bit mode to a 32-bit 
 mode ( to protected mode or compatibility mode ) . Software must not depend on 
 these bits to maintain a value after a 64-bit to 32-bit mode 
 switch.</em></p> 
 
 <p>This function is used both in 64-bit mode and in 32-bit mode. Since in 
 32-bit mode the high 32 bits of general-purpose registers are not 
 accessible, it is fine for this function to leave those bits unchanged, as 
 opposed to, for example, setting them to undefined values as done by the 
 semantic functions of certain instructions for certain flags. The switching 
 from 32-bit mode to 64-bit mode ( when modeled ) will set the high 32 bits of 
 general-purpose registers to undefined values.</p>*/
val wr16 : (int, int, int) -> int effect {escape, rreg, wreg}
function wr16 (reg, sailval, x86) =
let qword = (rgfi(reg, 0)) : int in
write_rgfi(reg, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 0, 16, sailval)), 0)

/*Write to doubleword general-purpose registers*/
/*<p>Note Intel Vol. 1 Sec. 3.4.1.1 
 p. 3-17, which says the following about 64-bit mode:</p> 
 
 <p><em>32-bit operands generate a 32-bit result, zero-extended to a 
 64-bit result in the destination general-purpose 
 register.</em></p> 
 
 <p>This is also confirmed by AMD manual, Jun ' 15, Vol. 3, App. B.1, under 
 ` No Extension of 8-Bit and 16-Bit Results ' .</p> 
 
 <p>In 32-bit mode, the upper 32 bits are undefined, as specified by the 
 following quote from the same page as above:</p> 
 
 <p><em>Because the upper 32 bits of 64-bit general-purpose registers are 
 undefined in 32-bit modes, the upper 32 bits of any general-purpose 
 register are not preserved when switching from 64-bit mode to a 32-bit 
 mode ( to protected mode or compatibility mode ) . Software must not depend on 
 these bits to maintain a value after a 64-bit to 32-bit mode 
 switch.</em></p> 
 
 <p>This function is used both in 64-bit mode and in 32-bit mode. Since in 
 32-bit mode the high 32 bits of general-purpose registers are not 
 accessible, it is fine for this function to set those bits to 0, as opposed 
 to, for example, setting them to undefined values as done by the semantic 
 functions of certain instructions for certain flags. The switching from 
 32-bit mode to 64-bit mode ( when modeled ) will set the high 32 bits of 
 general-purpose registers to undefined values.</p>*/
val wr32 : (int, int, int) -> int effect {escape, wreg}
function wr32 (reg, sailval, x86) =
write_rgfi(reg, n32(sailval), 0)

/*Write to quadword general-purpose registers*/
/*<p>This function is used only in 64-bit mode.</p>*/
val wr64 : (int, int, int) -> int effect {escape, wreg}
function wr64 (reg, sailval, x86) =
write_rgfi(reg, n64_to_i64(sailval), 0)

/*Read form byte, word, doubleword, or quadword 
 general-purpose register*/
val rgfi_size : (int, int, int, int) -> int effect {escape, rreg}
function rgfi_size (nbytes, index, rex, x86) =
(match nbytes {
1 => rr08(index, rex, 0),
2 => rr16(index, 0),
4 => rr32(index, 0),
8 => rr64(index, 0),
_ => 0
}) : int

/*Write to byte, word, doubleword, or quadword 
 general-purpose register*/
val write_rgfi_size : (int, int, int, int, int) -> int effect {escape, rreg, wreg}
function write_rgfi_size (nbytes, index, sailval, rex, x86) =
(match nbytes {
1 => wr08(index, rex, sailval, 0),
2 => wr16(index, sailval, 0),
4 => wr32(index, sailval, 0),
8 => wr64(index, sailval, 0),
_ => 0
}) : int

/*Functions to read/write into the MMX registers*/
/*<p>From Section 12.2, Intel Manual, Volume 3 \ ( System 
 Programming\ ) :</p> 
 
 <p><em>The MMX state consists of eight 64-bit registers \ ( MM0 
 through MM7\ ) . These registers are aliased to the low 64-bits \ ( bits 0 
 through 63\ ) of floating-point registers R0 through R7 \ ( see Figure 
 12-1\ ) . Note that the MMX registers are mapped to the physical 
 locations of the floating-point registers \ ( R0 through R7\ ) , not to 
 the relative locations of the registers in the floating-point register 
 stack \ ( ST0 through ST7\ ) . As a result, the MMX register mapping is 
 fixed and is not affected by value in the Top Of Stack \ ( TOS\ ) field 
 in the floating-point status word \ ( bits 11 through 13\ ) .</em></p> 
 
 <p>From Section 12.2, Intel Manual, Volume 3 \ ( System Programming\ ) </p>: 
 
 <p><em>When a value is written into an MMX register using an MMX 
 instruction, the value also appears in the corresponding 
 floating-point register in bits 0 through 63. Likewise, when a 
 floating-point value written into a floating-point register by a x87 
 FPU, the low 64 bits of that value also appears in a the corresponding 
 MMX register.</em></p> 
 
 <p><em>The execution of MMX instructions have several side effects on 
 the x87 FPU state contained in the floating-point registers, the x87 
 FPU tag word, and the x87 FPU status word. These side effects are as 
 follows:</em></p> 
 
 <p><em>* When an MMX instruction writes a value into an MMX register, 
 at the same time, bits 64 through 79 of the corresponding 
 floating-point register are set to all 1s.</em></p> 
 
 <p><em>* When an MMX instruction \ ( other than the EMMS instruction\ ) is 
 executed, each of the tag fields in the x87 FPU tag word is set to 00B 
 \ ( valid\ ) . \ ( See also Section 12.2.1, Effect of MMX, x87 FPU, FXSAVE, 
 and FXRSTOR Instructions on the x87 FPU Tag Word.\ ) </em></p> 
 
 <p><em>* When the EMMS instruction is executed, each tag field in the 
 x87 FPU tag word is set to 11B \ ( empty ) \.</em></p> 
 
 <p><em>* Each time an MMX instruction is executed, the TOS value is 
 set to 000B. Execution of MMX instructions does not affect the other 
 bits in the x87 FPU status word \ ( bits 0 through 10 and bits 14 and 
 15\ ) or the contents of the other x87 FPU registers that comprise the 
 x87 FPU state \ ( the x87 FPU control word, instruction pointer, data 
 pointer, or opcode registers\ ) .</em></p>*/
/*Functions to read/write 32/64/128/256/512 bits into the 
 XMM/YMM/ZMM registers*/
/*<p>Source: Intel Vol. 1, Section 15.5: ACCESSING XMM, YMM AND ZMM 
 REGISTERS</p> 
 
 <blockquote>The lower 128 bits of a YMM register is aliased to the 
 corresponding XMM register. Legacy SSE instructions ( i.e., SIMD 
 instructions operating on XMM state but not using the VEX prefix, 
 also referred to non-VEX encoded SIMD instructions ) will not access 
 the upper bits ( MAXVL-1:128 ) of the YMM registers. AVX and FMA 
 instructions with a VEX prefix and vector length of 128-bits zeroes 
 the upper 128 bits of the YMM register.</blockquote> 
 
 <blockquote>Upper bits of YMM registers ( 255:128 ) can be read and 
 written to by many instructions with a VEX.256 prefix. XSAVE and 
 XRSTOR may be used to save and restore the upper bits of the YMM 
 registers.</blockquote> 
 
 <blockquote>The lower 256 bits of a ZMM register are aliased to the 
 corresponding YMM register. Legacy SSE instructions ( i.e., SIMD 
 instructions operating on XMM state but not using the VEX prefix, 
 also referred to non-VEX encoded SIMD instructions ) will not access 
 the upper bits ( MAXVL-1:128 ) of the ZMM registers, where MAXVL is 
 maximum vector length ( currently 512 bits ) . AVX and FMA instructions 
 with a VEX prefix and vector length of 128-bits zero the upper 384 
 bits of the ZMM register, while the VEX prefix and vector length of 
 256-bits zeroes the upper 256 bits of the ZMM register. Upper bits 
 of ZMM registers ( 511:256 ) can be read and written to by instructions 
 with an EVEX.512 prefix.</blockquote> 
 
 <p>@ ( see rz32 ) , @ ( see rz64 ) , @ ( see rz128 ) , @ ( see rz256 ) , and @ ( see 
 rz512 ) will read the contents of the ZMMs as natural numbers.</p> 
 
 <p>Similarly, @ ( see wz32 ) , @ ( see wz64 ) , @ ( see wz128 ) , @ ( see wz256 ) , 
 and @ ( see wz512 ) are used to write natural numbers into the 
 ZMMs.</p>*/
val rz32 : (int, int) -> int effect {escape, rreg}
function rz32 (reg, x86) =
n32(zmmi(reg, 0))

val rz64 : (int, int) -> int effect {escape, rreg}
function rz64 (reg, x86) =
n64(zmmi(reg, 0))

val rz128 : (int, int) -> int effect {escape, rreg}
function rz128 (reg, x86) =
n128(zmmi(reg, 0))

/*Write @ ( ' val ' ) to low 32 bits of a ZMM register*/
/*<p><i>Upper bits</i>: For XMM registers, upper @ ( ` ( - 512 
 128 ) ` ) bits are preserved if @ ( ' regtype ' ) is @ ( ' *xmm-access* ' ) ; 
 for @ ( ' *vex-xmm-access* ' ) , these bits are zeroed out. For 
 @ ( ' *ymm-access* ' ) , upper @ ( ` ( - 512 256 ) ` ) bits are zeroed 
 out. For @ ( ' *zmm-access* ' ) , no upper bits are zeroed out.</p>*/
struct struct_wz32 = {
	regtype : int
}
val wz32 : (int, int, int, struct_wz32) -> int effect {escape, rreg, wreg}
function wz32 (reg, sailval, x86, keywords) =
let data = (zmmi(reg, 0)) : int in
let regtype = ((keywords).regtype) : int in
let data = ((match regtype {
2 => loghead(128, data),
3 => loghead(256, data),
_ => data
}) : int) : int in
let sailval = (n32(sailval)) : int in
write_zmmi(reg, changeBits(data, 0, 32, sailval), 0)

/*Write @ ( ' val ' ) to low 64 bits of a ZMM register*/
/*<p><i>Upper bits</i>: For XMM registers, upper @ ( ` ( - 512 
 128 ) ` ) bits are preserved if @ ( ' regtype ' ) is @ ( ' *xmm-access* ' ) ; 
 for @ ( ' *vex-xmm-access* ' ) , these bits are zeroed out. For 
 @ ( ' *ymm-access* ' ) , upper @ ( ` ( - 512 256 ) ` ) bits are zeroed 
 out. For @ ( ' *zmm-access* ' ) , no upper bits are zeroed out.</p>*/
struct struct_wz64 = {
	regtype : int
}
val wz64 : (int, int, int, struct_wz64) -> int effect {escape, rreg, wreg}
function wz64 (reg, sailval, x86, keywords) =
let data = (zmmi(reg, 0)) : int in
let regtype = ((keywords).regtype) : int in
let data = ((match regtype {
2 => loghead(128, data),
3 => loghead(256, data),
_ => data
}) : int) : int in
let sailval = (n64(sailval)) : int in
write_zmmi(reg, changeBits(data, 0, 64, sailval), 0)

/*Write @ ( ' val ' ) to low 128 bits of a ZMM register*/
/*<p><i>Upper bits</i>: For XMM registers, upper @ ( ` ( - 512 
 128 ) ` ) bits are preserved if @ ( ' regtype ' ) is @ ( ' *xmm-access* ' ) ; 
 for @ ( ' *vex-xmm-access* ' ) , these bits are zeroed out. For 
 @ ( ' *ymm-access* ' ) , upper @ ( ` ( - 512 256 ) ` ) bits are zeroed 
 out. For @ ( ' *zmm-access* ' ) , no upper bits are zeroed out.</p>*/
struct struct_wz128 = {
	regtype : int
}
val wz128 : (int, int, int, struct_wz128) -> int effect {escape, rreg, wreg}
function wz128 (reg, sailval, x86, keywords) =
let data = (zmmi(reg, 0)) : int in
let regtype = ((keywords).regtype) : int in
let data = ((match regtype {
2 => loghead(128, data),
3 => loghead(256, data),
_ => data
}) : int) : int in
let sailval = (n128(sailval)) : int in
write_zmmi(reg, changeBits(data, 0, 128, sailval), 0)

/*Functions to read/write 32/64/128 bits into the XMM 
 registers ( to be used by non-VEX encoded 128-bit SIMD instructions )*/
/*<p>These functions are meant to be used by instructions that 
 do not use a VEX or EVEX prefix --- these functions preserve the 
 upper bits of ZMM registers. For instructions that use these 
 prefixes and zero out these bits instead, see @ ( see 
 ZMMs-Reads-and-Writes ) .</p> 
 
 <p>Note that the index for accessing the XMM registers is 4-bits 
 wide ( as opposed to the 5-bit index for ZMM registers --- see @ ( see 
 ZMMs-Reads-and-Writes ) ) because only 16 XMM registers were supported 
 initially in the 64-bit mode ( and 8 in the 32-bit mode ) .</p> 
 
 <p>Source: Intel Vol. 2, Section 2.3.10 --- AVX Instructions 
 and Upper 128-bits of YMM registers:</p> 
 
 <blockquote>If an instruction with a destination XMM register is 
 encoded with a VEX prefix, the processor zeroes the upper 
 bits ( above bit 128 ) of the equivalent YMM register. Legacy SSE 
 instructions without VEX preserve the upper bits.</blockquote> 
 
 <p>Functions @ ( see rx32 ) , @ ( see rx64 ) , and @ ( see rx128 ) will read the 
 contents of the XMMs ( i.e., low 128 bits of ZMMs ) as natural 
 numbers.</p> 
 
 <p>Similarly, functions @ ( see wx32 ) , @ ( see wx64 ) , and @ ( see wx128 ) 
 are used to write natural numbers into the XMMs ( and preserve the 
 upper bits of ZMMs ) .</p>*/
val rx32 : (int, int) -> int effect {escape, rreg}
function rx32 (reg, x86) =
rz32(reg, 0)

val rx64 : (int, int) -> int effect {escape, rreg}
function rx64 (reg, x86) =
rz64(reg, 0)

val rx128 : (int, int) -> int effect {escape, rreg}
function rx128 (reg, x86) =
rz128(reg, 0)

/*Write @ ( ' val ' ) to low 32 bits of a ZMM register; upper 
 bits are preserved.*/
val wx32 : (int, int, int) -> int effect {escape, rreg, wreg}
function wx32 (reg, sailval, x86) =
wz32(reg, sailval, 0, struct {regtype = 1})

/*Write @ ( ' val ' ) to low 64 bits of a ZMM register; upper 
 bits are preserved.*/
val wx64 : (int, int, int) -> int effect {escape, rreg, wreg}
function wx64 (reg, sailval, x86) =
wz64(reg, sailval, 0, struct {regtype = 1})

/*Write @ ( ' val ' ) to low 128 bits of a ZMM register; upper 
 bits are preserved.*/
val wx128 : (int, int, int) -> int effect {escape, rreg, wreg}
function wx128 (reg, sailval, x86) =
wz128(reg, sailval, 0, struct {regtype = 1})

val xmmi_size : (int, int, int) -> int effect {escape, rreg}
function xmmi_size (nbytes, index, x86) =
(match nbytes {
4 => rx32(index, 0),
8 => rx64(index, 0),
16 => rx128(index, 0),
_ => 0
}) : int

/*Write @ ( ' val ' ) to low @ ( ' nbytes ' ) of an XMM register; 
 upper bits of the ZMM register are preserved.*/
val write_xmmi_size : (int, int, int, int) -> int effect {escape, rreg, wreg}
function write_xmmi_size (nbytes, index, sailval, x86) =
(match nbytes {
4 => wx32(index, sailval, 0),
8 => wx64(index, sailval, 0),
16 => wx128(index, sailval, 0),
_ => 0
}) : int

/*Get a unique unknown to be used when reasoning about 
 undefined values in the processor*/
/*<p>See @ ( see characterizing-undefined-behavior ) for more 
 details.</p> 
 
 <p>The accessor and updater functions of the @ ( ' undef ' ) field are 
 untouchable so that the only way to create a new seed for unknowns 
 is via this function.</p>*/
val undef_read : (int) -> (int, int) effect {undef}
function undef_read (x86) =
undef_read_logic(0)

/*Reading from and writing to the @ ( ' rflags ' ) register in the @ ( ' x86 ' ) state*/
/*<p>We define convenient macros @ ( ' flgi ' ) and @ ( ' !flgi ' ) to read a 
 flag ' s value and to write a flag ' s value into the @ ( ' rflags ' ) field in the 
 @ ( ' x86 ' ) state, respectively. Additionally, @ ( ' !flgi-undefined ' ) can be 
 used to write an undefined value into a particular flag.</p>*/
val undef_flg_logic : (int) -> (int, int) effect {undef}
function undef_flg_logic (x86) =
undef_read(0)

val undef_flg : (int) -> (int, int) effect {undef}
function undef_flg (x86) =
let (sailval, x86) = (undef_flg_logic(0)) : (int, int) in
(n01(sailval), 0 : int)

/*Writing user rflags \ ( CF, PF, AF, ZF, SF, and OF\ ) , 
 including undefined ones, to the x86 state*/
/*<p>We set the undefined flags, which are indicated by 
 @ ( ' mask ' ) , to the value returned by @ ( see undef-read ) .</p>*/
val write_user_rflags : (int, int, int) -> int effect {escape, rreg, undef, wreg}
function write_user_rflags (user_flags_vector, undefined_mask, x86) =
let user_flags_vector = (n32(user_flags_vector)) : int in
let undefined_mask = (n32(undefined_mask)) : int in
let input_rflags = (n32(r_rflags(0))) : int in
let x86 = (if (rflagsbits_get_cf(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_cf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_cf(rflagsbits_get_cf(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (if (rflagsbits_get_pf(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_pf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_pf(rflagsbits_get_pf(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (if (rflagsbits_get_af(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_af(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_af(rflagsbits_get_af(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (if (rflagsbits_get_zf(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_zf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_zf(rflagsbits_get_zf(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (if (rflagsbits_get_sf(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_sf(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_sf(rflagsbits_get_sf(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
let x86 = (if (rflagsbits_get_of(undefined_mask)) == (1)
then let rflags_var = (r_rflags(0)) : int in
let (sailval, x86) = (undef_flg(0)) : (int, int) in
let new_rflags = (set_rflagsbits_get_of(sailval, rflags_var)) : int in
write_rflags(new_rflags, 0)
else let rflags_var = (r_rflags(0)) : int in
let new_rflags = (set_rflagsbits_get_of(rflagsbits_get_of(user_flags_vector), rflags_var)) : int in
write_rflags(new_rflags, 0)) : int in
0

