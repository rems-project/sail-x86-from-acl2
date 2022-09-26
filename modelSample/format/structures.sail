
bitfield prefixes : bits(52) =
  {nxt : 51..44, adr : 43..36, opr : 35..28, seg : 27..20, rep : 19..12, lck : 11..4, num : 3..0}

bitfield vex_prefixes : bits(24) = {byte2 : 23..16, byte1 : 15..8, byte0 : 7..0}

val vex_prefixes_byte0_p : vex_prefixes -> bool

function vex_prefixes_byte0_p vex_prefixes = {
    let byte0 : bits(8) = vex_prefixes[byte0];
    byte0 == 0xc5 | byte0 == 0xc4
}

bitfield vex2_byte1 : bits(8) = {r : 7..7, vvvv : 6..3, l : 2..2, pp : 1..0}

bitfield vex3_byte1 : bits(8) = {r : 7..7, x : 6..6, b : 5..5, m_mmmm : 4..0}

bitfield vex3_byte2 : bits(8) = {w : 7..7, vvvv : 6..3, l : 2..2, pp : 1..0}

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

val vex_get_vvvv : vex_prefixes -> sbits(5)

function vex_get_vvvv vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[vvvv], 5),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[vvvv], 5),
      _ => 0b11111
    }) : sbits(5)
}

val vex_get_l : vex_prefixes -> sbits(2)

function vex_get_l vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[l], 2),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[l], 2),
      _ => 0b11
    }) : sbits(2)
}

val vex_get_pp : vex_prefixes -> sbits(3)

function vex_get_pp vex_prefixes = {
    (match vex_prefixes[byte0] {
      197 => sail_zero_extend(Mk_vex2_byte1(vex_prefixes[byte1])[pp], 3),
      196 => sail_zero_extend(Mk_vex3_byte2(vex_prefixes[byte2])[pp], 3),
      _ => 0b111
    }) : sbits(3)
}

val vex_get_w : vex_prefixes -> bits(1)

function vex_get_w vex_prefixes = {
    (match vex_prefixes[byte0] {
      196 => Mk_vex3_byte2(vex_prefixes[byte2])[w],
      _ => 0b0
    }) : bits(1)
}

bitfield evex_prefixes : bits(32) = {byte3 : 31..24, byte2 : 23..16, byte1 : 15..8, byte0 : 7..0}

bitfield evex_byte1 : bits(8) = {r : 7..7, x : 6..6, b : 5..5, r_prime : 4..4, res : 3..2, mm : 1..0}

bitfield evex_byte2 : bits(8) = {w : 7..7, vvvv : 6..3, res : 2..2, pp : 1..0}

bitfield evex_byte3 : bits(8) = {z : 7..7, vl_rc : 6..5, b : 4..4, v_prime : 3..3, aaa : 2..0}

val evex_get_aaa : evex_prefixes -> bits(3)

function evex_get_aaa evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[aaa]
}

val evex_get_z : evex_prefixes -> bits(1)

function evex_get_z evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[z]
}

val evex_get_vvvv : evex_prefixes -> bits(4)

function evex_get_vvvv evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[vvvv]
}

val evex_get_v_prime : evex_prefixes -> bits(1)

function evex_get_v_prime evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[v_prime]
}

val evex_get_vl_rc : evex_prefixes -> bits(2)

function evex_get_vl_rc evex_prefixes = {
    Mk_evex_byte3(evex_prefixes[byte3])[vl_rc]
}

val evex_get_pp : evex_prefixes -> bits(2)

function evex_get_pp evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[pp]
}

val evex_get_w : evex_prefixes -> bits(1)

function evex_get_w evex_prefixes = {
    Mk_evex_byte2(evex_prefixes[byte2])[w]
}

bitfield modr_m : bits(8) = {mod : 7..6, reg : 5..3, r_m : 2..0}

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
