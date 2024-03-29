$ifndef __X86_STRUCTURES
$define __X86_STRUCTURES

/*! Functions to collect legacy prefix bytes from an x86 instruction
<p>The field @ ( ' num ' ) of @ ( ' prefixes ' ) not only includes the number of 
 legacy prefixes present in an instruction, but also the number of REX bytes, 
 even though REX bytes are not stored in this structure. See @ ( tsee 
 get-prefixes ) for details.</p>
Functions to decode and collect VEX prefix bytes from an x86 
 instruction */
bitfield vex_prefixes : bits(24) = {byte2 : 23..16, byte1 : 15..8, byte0 : 7..0}

/*! Returns @ ( ' t ' ) if byte0 of the @ ( ' vex-prefixes ' ) structure is 
 either @ ( ' *vex2-byte0* ' ) or @ ( ' *vex3-byte0* ' ) ; returns @ ( ' nil ' ) otherwise */
val vex_prefixes_byte0_p : vex_prefixes -> bool

function vex_prefixes_byte0_p vex_prefixes = {
    let byte0 : bits(8) = vex_prefixes[byte0];
    byte0 == 0xc5 | byte0 == 0xc4
}

bitfield vex2_byte1 : bits(8) = {r : 7..7, vvvv : 6..3, l : 2..2, pp : 1..0}

bitfield vex3_byte1 : bits(8) = {r : 7..7, x : 6..6, b : 5..5, m_mmmm : 4..0}

bitfield vex3_byte2 : bits(8) = {w : 7..7, vvvv : 6..3, l : 2..2, pp : 1..0}

/*! Returns @ ( ' t ' ) if the @ ( ' vex-prefixes ' ) , irrespective of whether 
 they are two- or three-byte form, indicate the map that begins with the 
 escape bytes @ ( ' bytes ' ) */
val vex_prefixes_map_p : (bits(16), vex_prefixes) -> bool

function vex_prefixes_map_p (bytes, vex_prefixes) = {
    let byte0 : bits(8) = vex_prefixes[byte0];
    let byte1 : bits(8) = vex_prefixes[byte1];
    (match bytes {
      15 => byte0 == 0xc5 | byte0 == 0xc4 & Mk_vex3_byte1(byte1)[m_mmmm] == 0b00001,
      _ => byte0 == 0xc4 & (if bytes == 0x0f38 then {
          Mk_vex3_byte1(byte1)[m_mmmm] == 0b00010
      } else {
          Mk_vex3_byte1(byte1)[m_mmmm] == 0b00011
      })
    }) : bool
}

/*! Get the @ ( ' VVVV ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- 
 or three-byte VEX prefixes form */
val vex_get_vvvv : vex_prefixes -> sbits(5)

function vex_get_vvvv vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[vvvv], 5),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[vvvv], 5),
      _ => 0b11111
    }) : sbits(5)
}

/*! Get the @ ( ' L ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form */
val vex_get_l : vex_prefixes -> sbits(2)

function vex_get_l vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[l], 2),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[l], 2),
      _ => 0b11
    }) : sbits(2)
}

/*! Get the @ ( ' PP ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form */
val vex_get_pp : vex_prefixes -> sbits(3)

function vex_get_pp vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[pp], 3),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[pp], 3),
      _ => 0b111
    }) : sbits(3)
}

/*! Get the @ ( ' W ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form */
val vex_get_w : vex_prefixes -> bits(1)

function vex_get_w vex_prefixes = {
    (match vex_prefixes[byte0] {
      196 => Mk_vex3_byte2(vex_prefixes[byte2])[w],
      _ => 0b0
    }) : bits(1)
}

/*! Functions to decode and collect EVEX prefix bytes from an x86 
 instruction */
bitfield evex_prefixes : bits(32) = {byte3 : 31..24, byte2 : 23..16, byte1 : 15..8, byte0 : 7..0}

bitfield evex_byte1 : bits(8) = {r : 7..7, x : 6..6, b : 5..5, r_prime : 4..4, res : 3..2, mm : 1..0}

bitfield evex_byte2 : bits(8) = {w : 7..7, vvvv : 6..3, res : 2..2, pp : 1..0}

bitfield evex_byte3 : bits(8) = {z : 7..7, vl_rc : 6..5, b : 4..4, v_prime : 3..3, aaa : 2..0}

/*! Get the @ ( ' aaa ' ) field ( embedded opmask ) of @ ( ' evex-prefixes ' ) */
val evex_get_aaa : evex_prefixes -> bits(3)

function evex_get_aaa evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[aaa]
}

/*! Get the @ ( ' z ' ) field ( embedded opmask ) of @ ( ' evex-prefixes ' ) */
val evex_get_z : evex_prefixes -> bits(1)

function evex_get_z evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[z]
}

/*! Get the @ ( ' VVVV ' ) field of @ ( ' evex-prefixes ' ) */
val evex_get_vvvv : evex_prefixes -> bits(4)

function evex_get_vvvv evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[vvvv]
}

/*! Get the @ ( ' v-prime ' ) field of @ ( ' evex-prefixes ' ) */
val evex_get_v_prime : evex_prefixes -> bits(1)

function evex_get_v_prime evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[v_prime]
}

/*! Get the @ ( ' vl/rc ' ) field of @ ( ' evex-prefixes ' ) */
val evex_get_vl_rc : evex_prefixes -> bits(2)

function evex_get_vl_rc evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[vl_rc]
}

/*! Get the @ ( ' PP ' ) field of @ ( ' evex-prefixes ' ) */
val evex_get_pp : evex_prefixes -> bits(2)

function evex_get_pp evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[pp]
}

/*! Get the @ ( ' W ' ) field of @ ( ' evex-prefixes ' ) */
val evex_get_w : evex_prefixes -> bits(1)

function evex_get_w evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[w]
}

/*! Bitstruct definitions to store a ModR/M byte and its fields */
bitfield modr_m : bits(8) = {mod : 7..6, reg : 5..3, r_m : 2..0}

/*! Bitstruct definitions to store a SIB byte and its fields */
bitfield sib : bits(8) = {scale : 7..6, index : 5..3, base : 2..0}

bitfield rflagsbits : bits(32) =
  {
    res5 : 31..22,
    id : 21..21,
    vip : 20..20,
    vif : 19..19,
    ac : 18..18,
    vm : 17..17,
    rf : 16..16,
    res4 : 15..15,
    nt : 14..14,
    iopl : 13..12,
    of : 11..11,
    df : 10..10,
    intf : 9..9,
    tf : 8..8,
    sf : 7..7,
    zf : 6..6,
    res3 : 5..5,
    af : 4..4,
    res2 : 3..3,
    pf : 2..2,
    res1 : 1..1,
    cf : 0..0
  }

bitfield cr0bits : bits(32) =
  {
    pg : 31..31,
    cd : 30..30,
    nw : 29..29,
    res3 : 28..19,
    am : 18..18,
    res2 : 17..17,
    wp : 16..16,
    res1 : 15..6,
    ne : 5..5,
    et : 4..4,
    ts : 3..3,
    em : 2..2,
    mp : 1..1,
    pe : 0..0
  }

bitfield cr3bits : bits(64) = {res3 : 63..52, pdb : 51..12, res2 : 11..5, pcd : 4..4, pwt : 3..3, res1 : 2..0}

bitfield cr4bits : bits(22) =
  {
    smap : 21..21,
    smep : 20..20,
    res2 : 19..19,
    osxsave : 18..18,
    pcide : 17..17,
    fsgsbase : 16..16,
    res1 : 15..15,
    smxe : 14..14,
    vmxe : 13..13,
    la57 : 12..12,
    umip : 11..11,
    osxmmexcpt : 10..10,
    osfxsr : 9..9,
    pce : 8..8,
    pge : 7..7,
    mce : 6..6,
    pae : 5..5,
    pse : 4..4,
    de : 3..3,
    tsd : 2..2,
    pvi : 1..1,
    vme : 0..0
  }

bitfield cr8bits : bits(4) = {cr8_trpl : 3..0}

bitfield xcr0bits : bits(64) =
  {
    res2 : 63..10,
    pkru_state : 9..9,
    res1 : 8..8,
    hi16_zmm_state : 7..7,
    zmm_hi256_state : 6..6,
    opmask_state : 5..5,
    bndcsr_state : 4..4,
    bndreg_state : 3..3,
    avx_state : 2..2,
    sse_state : 1..1,
    fpu_mmx_state : 0..0
  }

bitfield ia32_eferbits : bits(12) = {nxe : 11..11, lma : 10..10, res2 : 9..9, lme : 8..8, res1 : 7..1, sce : 0..0}
$endif
