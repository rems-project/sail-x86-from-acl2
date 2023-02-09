
/*! <p>In 64-bit mode, in addition to generating 64-bit operand sizes, 
 the REX prefix is used to reference registers R8 to R15. Instructions that 
 include REX prefixes can access these registers if the relevant W, R, X, or 
 B bit in the REX prefix is set. E.g., let R be the relevant bit in the REX 
 prefix and let R be set --- so @ ( ' index ' ) = @ ( ` *r* ` ) for this function. If 
 @ ( ' reg ' ) = 0 ( which, in the non-REX world, would refer to rAX ) , 
 @ ( ' reg-index ' ) would give us the register index corresponding to the 
 register r8. If R is not set, @ ( ' reg-index ' ) will give us the index 
 corresponding to rAX.</p> 
 
 <p>In 32-bit mode, the REX prefix is absent. This function can be used in 
 32-bit mode, by passing 0 as REX.</p> */
val reg_index : (bits(3), bits(8), bits(2)) -> bits(4)

function reg_index (reg, rex_byte, index) = {
    if logbitp(unsigned(index), rex_byte) then {
        logior(0x8, sail_zero_extend(reg, 4))
    } else {
        sail_zero_extend(reg, 4)
    }
}

/*! Read from byte general-purpose registers
<p><i>Source: Intel Manuals, Vol. 1, Section 
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
 
 <p>In 32-bit mode, this function is called with 0 as REX.</p> */
val rr08 : (bits(4), bits(8)) -> bits(8)

function rr08 (reg, rex) = {
    let reg : nat = unsigned(reg);
    if not_bool(rex == 0x00) | reg < 4 then {
        let qword : sbits(64) = rgfi(reg);
        truncate(qword, 8)
    } else {
        let qword : sbits(64) = rgfi(unsigned(bits_of_int(reg - 4, 4)));
        qword[15 .. 8]
    }
}

/*! Writing to byte general-purpose registers
<p>Note Intel Vol. 1 Sec. 3.4.1.1 
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
 registers to undefined values.</p> */
val wr08 : (bits(4), bits(8), bits(8)) -> unit

function wr08 (reg, rex, byte) = {
    let reg : nat = unsigned(reg);
    if not_bool(rex == 0x00) | reg < 4 then {
        let qword : sbits(64) = rgfi(reg);
        write_rgfi(reg, changeSlice(qword[63 .. 0], 0, 8, byte))
    } else {
        let index : bits(4) = bits_of_int(unsigned(bits_of_int(reg, 4)) - 4, 4);
        let qword : sbits(64) = rgfi(unsigned(index));
        write_rgfi(unsigned(index), changeSlice(qword[63 .. 0], 8, 8, byte))
    }
}

/*! Read from word general-purpose registers */
val rr16 : bits(4) -> bits(16)

function rr16 reg = {
    truncate(rgfi(unsigned(reg)), 16)
}

/*! Write to word general-purpose registers
<p>Note Intel Vol. 1 Sec. 3.4.1.1 
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
 general-purpose registers to undefined values.</p> */
val wr16 : (bits(4), bits(16)) -> unit

function wr16 (reg, sailval) = {
    let qword : sbits(64) = rgfi(unsigned(reg));
    write_rgfi(unsigned(reg), changeSlice(qword[63 .. 0], 0, 16, sailval))
}

/*! Read from doubleword general-purpose registers */
val rr32 : bits(4) -> bits(32)

function rr32 reg = {
    truncate(rgfi(unsigned(reg)), 32)
}

/*! Write to doubleword general-purpose registers
<p>Note Intel Vol. 1 Sec. 3.4.1.1 
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
 general-purpose registers to undefined values.</p> */
val wr32 : (bits(4), bits(32)) -> unit

function wr32 (reg, sailval) = {
    write_rgfi(unsigned(reg), sail_zero_extend(sailval, 64))
}

/*! Read from quadword general-purpose registers
<p>This function is used only in 64-bit mode.</p> */
val rr64 : bits(4) -> bits(64)

function rr64 reg = {
    rgfi(unsigned(reg))
}

/*! Write to quadword general-purpose registers
<p>This function is used only in 64-bit mode.</p> */
val wr64 : (bits(4), bits(64)) -> unit

function wr64 (reg, sailval) = {
    write_rgfi(unsigned(reg), sailval)
}

/*! Read form byte, word, doubleword, or quadword 
 general-purpose register */
val rgfi_size : (bits(4), bits(4), bits(8)) -> bits(64)

function rgfi_size (nbytes, index, rex) = {
    (match nbytes {
      1 => sail_zero_extend(rr08(index, rex), 64),
      2 => sail_zero_extend(rr16(index), 64),
      4 => sail_zero_extend(rr32(index), 64),
      8 => rr64(index),
      _ => 0x0000000000000000
    }) : bits(64)
}

/*! Write to byte, word, doubleword, or quadword 
 general-purpose register */
val write_rgfi_size : (bits(4), bits(4), bits(64), bits(8)) -> unit

function write_rgfi_size (nbytes, index, sailval, rex) = {
    (match nbytes {
      1 => wr08(index, rex, truncate(sailval, 8)),
      2 => wr16(index, truncate(sailval, 16)),
      4 => wr32(index, truncate(sailval, 32)),
      8 => wr64(index, sailval),
      _ => ()
    }) : unit
}

/*! Functions to read/write 32/64/128/256/512 bits into the 
 XMM/YMM/ZMM registers
<p>Source: Intel Vol. 1, Section 15.5: ACCESSING XMM, YMM AND ZMM 
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
 ZMMs.</p> */
val rz32 : bits(5) -> bits(32)

function rz32 reg = {
    truncate(zmms[unsigned(reg)], 32)
}

val rz64 : bits(5) -> bits(64)

function rz64 reg = {
    truncate(zmms[unsigned(reg)], 64)
}

val rz128 : bits(5) -> bits(128)

function rz128 reg = {
    truncate(zmms[unsigned(reg)], 128)
}

struct struct_wz32 = {regtype : int}

val wz32 : (bits(5), bits(32), struct_wz32) -> unit

function wz32 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : bits(512) = {
        match regtype {
          2 => sail_zero_extend(loghead(128, data), 512),
          3 => sail_zero_extend(loghead(256, data), 512),
          _ => data
        }
    };
    zmms[unsigned(reg)] = changeSlice(data, 0, 32, sailval)
}

struct struct_wz64 = {regtype : int}

val wz64 : (bits(5), bits(64), struct_wz64) -> unit

function wz64 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : bits(512) = {
        match regtype {
          2 => sail_zero_extend(loghead(128, data), 512),
          3 => sail_zero_extend(loghead(256, data), 512),
          _ => data
        }
    };
    zmms[unsigned(reg)] = changeSlice(data, 0, 64, sailval)
}

struct struct_wz128 = {regtype : int}

val wz128 : (bits(5), bits(128), struct_wz128) -> unit

function wz128 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : bits(512) = {
        match regtype {
          2 => sail_zero_extend(loghead(128, data), 512),
          3 => sail_zero_extend(loghead(256, data), 512),
          _ => data
        }
    };
    zmms[unsigned(reg)] = changeSlice(data, 0, 128, sailval)
}

/*! Functions to read/write 32/64/128 bits into the XMM 
 registers ( to be used by non-VEX encoded 128-bit SIMD instructions )
<p>These functions are meant to be used by instructions that 
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
 upper bits of ZMMs ) .</p> */
val rx32 : bits(4) -> bits(32)

function rx32 reg = {
    rz32(sail_zero_extend(reg, 5))
}

val rx64 : bits(4) -> bits(64)

function rx64 reg = {
    rz64(sail_zero_extend(reg, 5))
}

val rx128 : bits(4) -> bits(128)

function rx128 reg = {
    rz128(sail_zero_extend(reg, 5))
}

/*! Write @ ( ' val ' ) to low 32 bits of a ZMM register; upper 
 bits are preserved. */
val wx32 : (bits(4), bits(32)) -> unit

function wx32 (reg, sailval) = {
    wz32(sail_zero_extend(reg, 5), sailval, struct { regtype = 1 })
}

/*! Write @ ( ' val ' ) to low 64 bits of a ZMM register; upper 
 bits are preserved. */
val wx64 : (bits(4), bits(64)) -> unit

function wx64 (reg, sailval) = {
    wz64(sail_zero_extend(reg, 5), sailval, struct { regtype = 1 })
}

/*! Write @ ( ' val ' ) to low 128 bits of a ZMM register; upper 
 bits are preserved. */
val wx128 : (bits(4), bits(128)) -> unit

function wx128 (reg, sailval) = {
    wz128(sail_zero_extend(reg, 5), sailval, struct { regtype = 1 })
}

val xmmi_size : (bits(5), bits(4)) -> bits(128)

function xmmi_size (nbytes, index) = {
    (match nbytes {
      4 => sail_zero_extend(rx32(index), 128),
      8 => sail_zero_extend(rx64(index), 128),
      16 => rx128(index),
      _ => 0x00000000000000000000000000000000
    }) : bits(128)
}

/*! Write @ ( ' val ' ) to low @ ( ' nbytes ' ) of an XMM register; 
 upper bits of the ZMM register are preserved. */
val write_xmmi_size : (bits(5), bits(4), int) -> unit

function write_xmmi_size (nbytes, index, sailval) = {
    (match nbytes {
      4 => wx32(index, bits_of_int(sailval, 32)),
      8 => wx64(index, bits_of_int(sailval, 64)),
      16 => wx128(index, bits_of_int(sailval, 128)),
      _ => ()
    }) : unit
}

/*! Get a unique unknown to be used when reasoning about 
 undefined values in the processor
<p>See @ ( see characterizing-undefined-behavior ) for more 
 details.</p> 
 
 <p>The accessor and updater functions of the @ ( ' undef ' ) field are 
 untouchable so that the only way to create a new seed for unknowns 
 is via this function.</p> */
val undef_read : unit -> nat

function undef_read () = {
    undef_read_logic()
}

/*! Reading from and writing to the @ ( ' rflags ' ) register in the @ ( ' x86 ' ) state
<p>We define convenient macros @ ( ' flgi ' ) and @ ( ' !flgi ' ) to read a 
 flag ' s value and to write a flag ' s value into the @ ( ' rflags ' ) field in the 
 @ ( ' x86 ' ) state, respectively. Additionally, @ ( ' !flgi-undefined ' ) can be 
 used to write an undefined value into a particular flag.</p> */
val undef_flg_logic : unit -> nat

function undef_flg_logic () = {
    undef_read()
}

val undef_flg : unit -> bits(1)

function undef_flg () = {
    let sailval : nat = undef_flg_logic();
    bits_of_int(sailval, 1)
}

/*! Writing user rflags \ ( CF, PF, AF, ZF, SF, and OF\ ) , 
 including undefined ones, to the x86 state
<p>We set the undefined flags, which are indicated by 
 @ ( ' mask ' ) , to the value returned by @ ( see undef-read ) .</p> */
val write_user_rflags : (rflagsbits, rflagsbits) -> unit

function write_user_rflags (user_flags_vector, undefined_mask) = {
    let input_rflags : rflagsbits = rflags;
    if undefined_mask[cf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with cf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with cf = user_flags_vector[cf]];
        rflags = new_rflags
    };
    if undefined_mask[pf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with pf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with pf = user_flags_vector[pf]];
        rflags = new_rflags
    };
    if undefined_mask[af] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with af = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with af = user_flags_vector[af]];
        rflags = new_rflags
    };
    if undefined_mask[zf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with zf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with zf = user_flags_vector[zf]];
        rflags = new_rflags
    };
    if undefined_mask[sf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with sf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with sf = user_flags_vector[sf]];
        rflags = new_rflags
    };
    if undefined_mask[of] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with of = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with of = user_flags_vector[of]];
        rflags = new_rflags
    };
    if log_register_writes then log_rflags_write();
    ()
}
