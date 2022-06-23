
val prefixes_get_num : bits(52) -> bits(4)

function prefixes_get_num inputbits = {
    genericBitstructAccessor(4, inputbits, 0)
}

val prefixes_get_lck : bits(52) -> bits(8)

function prefixes_get_lck inputbits = {
    genericBitstructAccessor(8, inputbits, 4)
}

val prefixes_get_rep : bits(52) -> bits(8)

function prefixes_get_rep inputbits = {
    genericBitstructAccessor(8, inputbits, 12)
}

val prefixes_get_seg : bits(52) -> bits(8)

function prefixes_get_seg inputbits = {
    genericBitstructAccessor(8, inputbits, 20)
}

val prefixes_get_opr : bits(52) -> bits(8)

function prefixes_get_opr inputbits = {
    genericBitstructAccessor(8, inputbits, 28)
}

val prefixes_get_adr : bits(52) -> bits(8)

function prefixes_get_adr inputbits = {
    genericBitstructAccessor(8, inputbits, 36)
}

val prefixes_get_nxt : bits(52) -> bits(8)

function prefixes_get_nxt inputbits = {
    genericBitstructAccessor(8, inputbits, 44)
}

val set_prefixes_get_num : (bits(4), bits(52)) -> bits(52)

function set_prefixes_get_num (splicebits, inputbits) = {
    genericBitstructUpdater(4, splicebits, 0, inputbits)
}

val set_prefixes_get_lck : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_lck (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 4, inputbits)
}

val set_prefixes_get_rep : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_rep (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 12, inputbits)
}

val set_prefixes_get_seg : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_seg (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 20, inputbits)
}

val set_prefixes_get_opr : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_opr (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 28, inputbits)
}

val set_prefixes_get_adr : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_adr (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 36, inputbits)
}

val set_prefixes_get_nxt : (bits(8), bits(52)) -> bits(52)

function set_prefixes_get_nxt (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 44, inputbits)
}

val change_prefixes : (bits(52), option(bits(4)), option(bits(8)), option(bits(8)), option(bits(8)), option(bits(8)), option(bits(8)), option(bits(8))) -> bits(52)

function change_prefixes (input_bits, input_num, input_lck, input_rep, input_seg, input_opr, input_adr, input_nxt) = {
    let output_num : bits(52) = match input_num {
      Some(input_num) => set_prefixes_get_num(input_num, input_bits),
      _ => input_bits
    };
    let output_lck : bits(52) = match input_lck {
      Some(input_lck) => set_prefixes_get_lck(input_lck, output_num),
      _ => output_num
    };
    let output_rep : bits(52) = match input_rep {
      Some(input_rep) => set_prefixes_get_rep(input_rep, output_lck),
      _ => output_lck
    };
    let output_seg : bits(52) = match input_seg {
      Some(input_seg) => set_prefixes_get_seg(input_seg, output_rep),
      _ => output_rep
    };
    let output_opr : bits(52) = match input_opr {
      Some(input_opr) => set_prefixes_get_opr(input_opr, output_seg),
      _ => output_seg
    };
    let output_adr : bits(52) = match input_adr {
      Some(input_adr) => set_prefixes_get_adr(input_adr, output_opr),
      _ => output_opr
    };
    let output_nxt : bits(52) = match input_nxt {
      Some(input_nxt) => set_prefixes_get_nxt(input_nxt, output_adr),
      _ => output_adr
    };
    output_nxt
}

val vex_prefixes_get_byte0 : bits(24) -> bits(8)

function vex_prefixes_get_byte0 inputbits = {
    genericBitstructAccessor(8, inputbits, 0)
}

val vex_prefixes_get_byte1 : bits(24) -> bits(8)

function vex_prefixes_get_byte1 inputbits = {
    genericBitstructAccessor(8, inputbits, 8)
}

val vex_prefixes_get_byte2 : bits(24) -> bits(8)

function vex_prefixes_get_byte2 inputbits = {
    genericBitstructAccessor(8, inputbits, 16)
}

val set_vex_prefixes_get_byte0 : (bits(8), bits(24)) -> bits(24)

function set_vex_prefixes_get_byte0 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 0, inputbits)
}

val set_vex_prefixes_get_byte1 : (bits(8), bits(24)) -> bits(24)

function set_vex_prefixes_get_byte1 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 8, inputbits)
}

val set_vex_prefixes_get_byte2 : (bits(8), bits(24)) -> bits(24)

function set_vex_prefixes_get_byte2 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 16, inputbits)
}

val change_vex_prefixes : (bits(24), option(bits(8)), option(bits(8)), option(bits(8))) -> bits(24)

function change_vex_prefixes (input_bits, input_byte0, input_byte1, input_byte2) = {
    let output_byte0 : bits(24) = match input_byte0 {
      Some(input_byte0) => set_vex_prefixes_get_byte0(input_byte0, input_bits),
      _ => input_bits
    };
    let output_byte1 : bits(24) = match input_byte1 {
      Some(input_byte1) => set_vex_prefixes_get_byte1(input_byte1, output_byte0),
      _ => output_byte0
    };
    let output_byte2 : bits(24) = match input_byte2 {
      Some(input_byte2) => set_vex_prefixes_get_byte2(input_byte2, output_byte1),
      _ => output_byte1
    };
    output_byte2
}

val vex_prefixes_byte0_p : bits(24) -> bool

function vex_prefixes_byte0_p vex_prefixes = {
    let byte0 : bits(8) = vex_prefixes_get_byte0(vex_prefixes);
    byte0 == 0xc5 | byte0 == 0xc4
}

val vex2_byte1_get_pp : bits(8) -> bits(2)

function vex2_byte1_get_pp inputbits = {
    genericBitstructAccessor(2, inputbits, 0)
}

val vex2_byte1_get_l : bits(8) -> bits(1)

function vex2_byte1_get_l inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val vex2_byte1_get_vvvv : bits(8) -> bits(4)

function vex2_byte1_get_vvvv inputbits = {
    genericBitstructAccessor(4, inputbits, 3)
}

val vex2_byte1_get_r : bits(8) -> bits(1)

function vex2_byte1_get_r inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_vex2_byte1_get_pp : (bits(2), bits(8)) -> bits(8)

function set_vex2_byte1_get_pp (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 0, inputbits)
}

val set_vex2_byte1_get_l : (bits(1), bits(8)) -> bits(8)

function set_vex2_byte1_get_l (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_vex2_byte1_get_vvvv : (bits(4), bits(8)) -> bits(8)

function set_vex2_byte1_get_vvvv (splicebits, inputbits) = {
    genericBitstructUpdater(4, splicebits, 3, inputbits)
}

val set_vex2_byte1_get_r : (bits(1), bits(8)) -> bits(8)

function set_vex2_byte1_get_r (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_vex2_byte1 : (bits(8), option(bits(2)), option(bits(1)), option(bits(4)), option(bits(1))) -> bits(8)

function change_vex2_byte1 (input_bits, input_pp, input_l, input_vvvv, input_r) = {
    let output_pp : bits(8) = match input_pp {
      Some(input_pp) => set_vex2_byte1_get_pp(input_pp, input_bits),
      _ => input_bits
    };
    let output_l : bits(8) = match input_l {
      Some(input_l) => set_vex2_byte1_get_l(input_l, output_pp),
      _ => output_pp
    };
    let output_vvvv : bits(8) = match input_vvvv {
      Some(input_vvvv) => set_vex2_byte1_get_vvvv(input_vvvv, output_l),
      _ => output_l
    };
    let output_r : bits(8) = match input_r {
      Some(input_r) => set_vex2_byte1_get_r(input_r, output_vvvv),
      _ => output_vvvv
    };
    output_r
}

val vex3_byte1_get_m_mmmm : bits(8) -> bits(5)

function vex3_byte1_get_m_mmmm inputbits = {
    genericBitstructAccessor(5, inputbits, 0)
}

val vex3_byte1_get_b : bits(8) -> bits(1)

function vex3_byte1_get_b inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val vex3_byte1_get_x : bits(8) -> bits(1)

function vex3_byte1_get_x inputbits = {
    genericBitstructAccessor(1, inputbits, 6)
}

val vex3_byte1_get_r : bits(8) -> bits(1)

function vex3_byte1_get_r inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_vex3_byte1_get_m_mmmm : (bits(5), bits(8)) -> bits(8)

function set_vex3_byte1_get_m_mmmm (splicebits, inputbits) = {
    genericBitstructUpdater(5, splicebits, 0, inputbits)
}

val set_vex3_byte1_get_b : (bits(1), bits(8)) -> bits(8)

function set_vex3_byte1_get_b (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_vex3_byte1_get_x : (bits(1), bits(8)) -> bits(8)

function set_vex3_byte1_get_x (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 6, inputbits)
}

val set_vex3_byte1_get_r : (bits(1), bits(8)) -> bits(8)

function set_vex3_byte1_get_r (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_vex3_byte1 : (bits(8), option(bits(5)), option(bits(1)), option(bits(1)), option(bits(1))) -> bits(8)

function change_vex3_byte1 (input_bits, input_m_mmmm, input_b, input_x, input_r) = {
    let output_m_mmmm : bits(8) = match input_m_mmmm {
      Some(input_m_mmmm) => set_vex3_byte1_get_m_mmmm(input_m_mmmm, input_bits),
      _ => input_bits
    };
    let output_b : bits(8) = match input_b {
      Some(input_b) => set_vex3_byte1_get_b(input_b, output_m_mmmm),
      _ => output_m_mmmm
    };
    let output_x : bits(8) = match input_x {
      Some(input_x) => set_vex3_byte1_get_x(input_x, output_b),
      _ => output_b
    };
    let output_r : bits(8) = match input_r {
      Some(input_r) => set_vex3_byte1_get_r(input_r, output_x),
      _ => output_x
    };
    output_r
}

val vex3_byte2_get_pp : bits(8) -> bits(2)

function vex3_byte2_get_pp inputbits = {
    genericBitstructAccessor(2, inputbits, 0)
}

val vex3_byte2_get_l : bits(8) -> bits(1)

function vex3_byte2_get_l inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val vex3_byte2_get_vvvv : bits(8) -> bits(4)

function vex3_byte2_get_vvvv inputbits = {
    genericBitstructAccessor(4, inputbits, 3)
}

val vex3_byte2_get_w : bits(8) -> bits(1)

function vex3_byte2_get_w inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_vex3_byte2_get_pp : (bits(2), bits(8)) -> bits(8)

function set_vex3_byte2_get_pp (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 0, inputbits)
}

val set_vex3_byte2_get_l : (bits(1), bits(8)) -> bits(8)

function set_vex3_byte2_get_l (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_vex3_byte2_get_vvvv : (bits(4), bits(8)) -> bits(8)

function set_vex3_byte2_get_vvvv (splicebits, inputbits) = {
    genericBitstructUpdater(4, splicebits, 3, inputbits)
}

val set_vex3_byte2_get_w : (bits(1), bits(8)) -> bits(8)

function set_vex3_byte2_get_w (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_vex3_byte2 : (bits(8), option(bits(2)), option(bits(1)), option(bits(4)), option(bits(1))) -> bits(8)

function change_vex3_byte2 (input_bits, input_pp, input_l, input_vvvv, input_w) = {
    let output_pp : bits(8) = match input_pp {
      Some(input_pp) => set_vex3_byte2_get_pp(input_pp, input_bits),
      _ => input_bits
    };
    let output_l : bits(8) = match input_l {
      Some(input_l) => set_vex3_byte2_get_l(input_l, output_pp),
      _ => output_pp
    };
    let output_vvvv : bits(8) = match input_vvvv {
      Some(input_vvvv) => set_vex3_byte2_get_vvvv(input_vvvv, output_l),
      _ => output_l
    };
    let output_w : bits(8) = match input_w {
      Some(input_w) => set_vex3_byte2_get_w(input_w, output_vvvv),
      _ => output_vvvv
    };
    output_w
}

val vex_prefixes_map_p : (bits(16), bits(24)) -> bool

function vex_prefixes_map_p (bytes, vex_prefixes) = {
    let byte0 : bits(8) = vex_prefixes_get_byte0(vex_prefixes);
    let byte1 : bits(8) = vex_prefixes_get_byte1(vex_prefixes);
    (match bytes {
      15 => byte0 == 0xc5 | byte0 == 0xc4 & (vex3_byte1_get_m_mmmm(byte1) == 0b00001) : bool,
      _ => byte0 == 0xc4 & (if bytes == 0x0f38 then {
          (vex3_byte1_get_m_mmmm(byte1) == 0b00010) : bool
      } else {
          (vex3_byte1_get_m_mmmm(byte1) == 0b00011) : bool
      })
    }) : bool
}

val vex_get_vvvv : bits(24) -> sbits(5)

function vex_get_vvvv vex_prefixes = {
    (match vex_prefixes_get_byte0(vex_prefixes) {
      197 => sail_zero_extend(vex2_byte1_get_vvvv(vex_prefixes_get_byte1(vex_prefixes)), 5),
      196 => sail_zero_extend(vex3_byte2_get_vvvv(vex_prefixes_get_byte2(vex_prefixes)), 5),
      _ => 0b11111
    }) : sbits(5)
}

val vex_get_l : bits(24) -> sbits(2)

function vex_get_l vex_prefixes = {
    (match vex_prefixes_get_byte0(vex_prefixes) {
      197 => sail_zero_extend(vex2_byte1_get_l(vex_prefixes_get_byte1(vex_prefixes)), 2),
      196 => sail_zero_extend(vex3_byte2_get_l(vex_prefixes_get_byte2(vex_prefixes)), 2),
      _ => 0b11
    }) : sbits(2)
}

val vex_get_pp : bits(24) -> sbits(3)

function vex_get_pp vex_prefixes = {
    (match vex_prefixes_get_byte0(vex_prefixes) {
      197 => sail_zero_extend(vex2_byte1_get_pp(vex_prefixes_get_byte1(vex_prefixes)), 3),
      196 => sail_zero_extend(vex3_byte2_get_pp(vex_prefixes_get_byte2(vex_prefixes)), 3),
      _ => 0b111
    }) : sbits(3)
}

val vex_get_w : bits(24) -> bits(1)

function vex_get_w vex_prefixes = {
    (match vex_prefixes_get_byte0(vex_prefixes) {
      196 => vex3_byte2_get_w(vex_prefixes_get_byte2(vex_prefixes)),
      _ => 0b0
    }) : bits(1)
}

val evex_prefixes_get_byte0 : bits(32) -> bits(8)

function evex_prefixes_get_byte0 inputbits = {
    genericBitstructAccessor(8, inputbits, 0)
}

val evex_prefixes_get_byte1 : bits(32) -> bits(8)

function evex_prefixes_get_byte1 inputbits = {
    genericBitstructAccessor(8, inputbits, 8)
}

val evex_prefixes_get_byte2 : bits(32) -> bits(8)

function evex_prefixes_get_byte2 inputbits = {
    genericBitstructAccessor(8, inputbits, 16)
}

val evex_prefixes_get_byte3 : bits(32) -> bits(8)

function evex_prefixes_get_byte3 inputbits = {
    genericBitstructAccessor(8, inputbits, 24)
}

val set_evex_prefixes_get_byte0 : (bits(8), bits(32)) -> bits(32)

function set_evex_prefixes_get_byte0 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 0, inputbits)
}

val set_evex_prefixes_get_byte1 : (bits(8), bits(32)) -> bits(32)

function set_evex_prefixes_get_byte1 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 8, inputbits)
}

val set_evex_prefixes_get_byte2 : (bits(8), bits(32)) -> bits(32)

function set_evex_prefixes_get_byte2 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 16, inputbits)
}

val set_evex_prefixes_get_byte3 : (bits(8), bits(32)) -> bits(32)

function set_evex_prefixes_get_byte3 (splicebits, inputbits) = {
    genericBitstructUpdater(8, splicebits, 24, inputbits)
}

val change_evex_prefixes : (bits(32), option(bits(8)), option(bits(8)), option(bits(8)), option(bits(8))) -> bits(32)

function change_evex_prefixes (input_bits, input_byte0, input_byte1, input_byte2, input_byte3) = {
    let output_byte0 : bits(32) = match input_byte0 {
      Some(input_byte0) => set_evex_prefixes_get_byte0(input_byte0, input_bits),
      _ => input_bits
    };
    let output_byte1 : bits(32) = match input_byte1 {
      Some(input_byte1) => set_evex_prefixes_get_byte1(input_byte1, output_byte0),
      _ => output_byte0
    };
    let output_byte2 : bits(32) = match input_byte2 {
      Some(input_byte2) => set_evex_prefixes_get_byte2(input_byte2, output_byte1),
      _ => output_byte1
    };
    let output_byte3 : bits(32) = match input_byte3 {
      Some(input_byte3) => set_evex_prefixes_get_byte3(input_byte3, output_byte2),
      _ => output_byte2
    };
    output_byte3
}

val evex_byte1_get_mm : bits(8) -> bits(2)

function evex_byte1_get_mm inputbits = {
    genericBitstructAccessor(2, inputbits, 0)
}

val evex_byte1_get_res : bits(8) -> bits(2)

function evex_byte1_get_res inputbits = {
    genericBitstructAccessor(2, inputbits, 2)
}

val evex_byte1_get_r_prime : bits(8) -> bits(1)

function evex_byte1_get_r_prime inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val evex_byte1_get_b : bits(8) -> bits(1)

function evex_byte1_get_b inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val evex_byte1_get_x : bits(8) -> bits(1)

function evex_byte1_get_x inputbits = {
    genericBitstructAccessor(1, inputbits, 6)
}

val evex_byte1_get_r : bits(8) -> bits(1)

function evex_byte1_get_r inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_evex_byte1_get_mm : (bits(2), bits(8)) -> bits(8)

function set_evex_byte1_get_mm (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 0, inputbits)
}

val set_evex_byte1_get_res : (bits(2), bits(8)) -> bits(8)

function set_evex_byte1_get_res (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 2, inputbits)
}

val set_evex_byte1_get_r_prime : (bits(1), bits(8)) -> bits(8)

function set_evex_byte1_get_r_prime (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_evex_byte1_get_b : (bits(1), bits(8)) -> bits(8)

function set_evex_byte1_get_b (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_evex_byte1_get_x : (bits(1), bits(8)) -> bits(8)

function set_evex_byte1_get_x (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 6, inputbits)
}

val set_evex_byte1_get_r : (bits(1), bits(8)) -> bits(8)

function set_evex_byte1_get_r (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_evex_byte1 : (bits(8), option(bits(2)), option(bits(2)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1))) -> bits(8)

function change_evex_byte1 (input_bits, input_mm, input_res, input_r_prime, input_b, input_x, input_r) = {
    let output_mm : bits(8) = match input_mm {
      Some(input_mm) => set_evex_byte1_get_mm(input_mm, input_bits),
      _ => input_bits
    };
    let output_res : bits(8) = match input_res {
      Some(input_res) => set_evex_byte1_get_res(input_res, output_mm),
      _ => output_mm
    };
    let output_r_prime : bits(8) = match input_r_prime {
      Some(input_r_prime) => set_evex_byte1_get_r_prime(input_r_prime, output_res),
      _ => output_res
    };
    let output_b : bits(8) = match input_b {
      Some(input_b) => set_evex_byte1_get_b(input_b, output_r_prime),
      _ => output_r_prime
    };
    let output_x : bits(8) = match input_x {
      Some(input_x) => set_evex_byte1_get_x(input_x, output_b),
      _ => output_b
    };
    let output_r : bits(8) = match input_r {
      Some(input_r) => set_evex_byte1_get_r(input_r, output_x),
      _ => output_x
    };
    output_r
}

val evex_byte2_get_pp : bits(8) -> bits(2)

function evex_byte2_get_pp inputbits = {
    genericBitstructAccessor(2, inputbits, 0)
}

val evex_byte2_get_res : bits(8) -> bits(1)

function evex_byte2_get_res inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val evex_byte2_get_vvvv : bits(8) -> bits(4)

function evex_byte2_get_vvvv inputbits = {
    genericBitstructAccessor(4, inputbits, 3)
}

val evex_byte2_get_w : bits(8) -> bits(1)

function evex_byte2_get_w inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_evex_byte2_get_pp : (bits(2), bits(8)) -> bits(8)

function set_evex_byte2_get_pp (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 0, inputbits)
}

val set_evex_byte2_get_res : (bits(1), bits(8)) -> bits(8)

function set_evex_byte2_get_res (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_evex_byte2_get_vvvv : (bits(4), bits(8)) -> bits(8)

function set_evex_byte2_get_vvvv (splicebits, inputbits) = {
    genericBitstructUpdater(4, splicebits, 3, inputbits)
}

val set_evex_byte2_get_w : (bits(1), bits(8)) -> bits(8)

function set_evex_byte2_get_w (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_evex_byte2 : (bits(8), option(bits(2)), option(bits(1)), option(bits(4)), option(bits(1))) -> bits(8)

function change_evex_byte2 (input_bits, input_pp, input_res, input_vvvv, input_w) = {
    let output_pp : bits(8) = match input_pp {
      Some(input_pp) => set_evex_byte2_get_pp(input_pp, input_bits),
      _ => input_bits
    };
    let output_res : bits(8) = match input_res {
      Some(input_res) => set_evex_byte2_get_res(input_res, output_pp),
      _ => output_pp
    };
    let output_vvvv : bits(8) = match input_vvvv {
      Some(input_vvvv) => set_evex_byte2_get_vvvv(input_vvvv, output_res),
      _ => output_res
    };
    let output_w : bits(8) = match input_w {
      Some(input_w) => set_evex_byte2_get_w(input_w, output_vvvv),
      _ => output_vvvv
    };
    output_w
}

val evex_byte3_get_aaa : bits(8) -> bits(3)

function evex_byte3_get_aaa inputbits = {
    genericBitstructAccessor(3, inputbits, 0)
}

val evex_byte3_get_v_prime : bits(8) -> bits(1)

function evex_byte3_get_v_prime inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val evex_byte3_get_b : bits(8) -> bits(1)

function evex_byte3_get_b inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val evex_byte3_get_vl_rc : bits(8) -> bits(2)

function evex_byte3_get_vl_rc inputbits = {
    genericBitstructAccessor(2, inputbits, 5)
}

val evex_byte3_get_z : bits(8) -> bits(1)

function evex_byte3_get_z inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val set_evex_byte3_get_aaa : (bits(3), bits(8)) -> bits(8)

function set_evex_byte3_get_aaa (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 0, inputbits)
}

val set_evex_byte3_get_v_prime : (bits(1), bits(8)) -> bits(8)

function set_evex_byte3_get_v_prime (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_evex_byte3_get_b : (bits(1), bits(8)) -> bits(8)

function set_evex_byte3_get_b (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_evex_byte3_get_vl_rc : (bits(2), bits(8)) -> bits(8)

function set_evex_byte3_get_vl_rc (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 5, inputbits)
}

val set_evex_byte3_get_z : (bits(1), bits(8)) -> bits(8)

function set_evex_byte3_get_z (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val change_evex_byte3 : (bits(8), option(bits(3)), option(bits(1)), option(bits(1)), option(bits(2)), option(bits(1))) -> bits(8)

function change_evex_byte3 (input_bits, input_aaa, input_v_prime, input_b, input_vl_rc, input_z) = {
    let output_aaa : bits(8) = match input_aaa {
      Some(input_aaa) => set_evex_byte3_get_aaa(input_aaa, input_bits),
      _ => input_bits
    };
    let output_v_prime : bits(8) = match input_v_prime {
      Some(input_v_prime) => set_evex_byte3_get_v_prime(input_v_prime, output_aaa),
      _ => output_aaa
    };
    let output_b : bits(8) = match input_b {
      Some(input_b) => set_evex_byte3_get_b(input_b, output_v_prime),
      _ => output_v_prime
    };
    let output_vl_rc : bits(8) = match input_vl_rc {
      Some(input_vl_rc) => set_evex_byte3_get_vl_rc(input_vl_rc, output_b),
      _ => output_b
    };
    let output_z : bits(8) = match input_z {
      Some(input_z) => set_evex_byte3_get_z(input_z, output_vl_rc),
      _ => output_vl_rc
    };
    output_z
}

val evex_get_vvvv : bits(32) -> bits(4)

function evex_get_vvvv evex_prefixes = {
    evex_byte2_get_vvvv(evex_prefixes_get_byte2(evex_prefixes))
}

val evex_get_v_prime : bits(32) -> bits(1)

function evex_get_v_prime evex_prefixes = {
    evex_byte3_get_v_prime(evex_prefixes_get_byte3(evex_prefixes))
}

val evex_get_vl_rc : bits(32) -> bits(2)

function evex_get_vl_rc evex_prefixes = {
    evex_byte3_get_vl_rc(evex_prefixes_get_byte3(evex_prefixes))
}

val evex_get_pp : bits(32) -> bits(2)

function evex_get_pp evex_prefixes = {
    evex_byte2_get_pp(evex_prefixes_get_byte2(evex_prefixes))
}

val evex_get_w : bits(32) -> bits(1)

function evex_get_w evex_prefixes = {
    evex_byte2_get_w(evex_prefixes_get_byte2(evex_prefixes))
}

val modr_m_get_r_m : bits(8) -> bits(3)

function modr_m_get_r_m inputbits = {
    genericBitstructAccessor(3, inputbits, 0)
}

val modr_m_get_reg : bits(8) -> bits(3)

function modr_m_get_reg inputbits = {
    genericBitstructAccessor(3, inputbits, 3)
}

val modr_m_get_mod : bits(8) -> bits(2)

function modr_m_get_mod inputbits = {
    genericBitstructAccessor(2, inputbits, 6)
}

val set_modr_m_get_r_m : (bits(3), bits(8)) -> bits(8)

function set_modr_m_get_r_m (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 0, inputbits)
}

val set_modr_m_get_reg : (bits(3), bits(8)) -> bits(8)

function set_modr_m_get_reg (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 3, inputbits)
}

val set_modr_m_get_mod : (bits(2), bits(8)) -> bits(8)

function set_modr_m_get_mod (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 6, inputbits)
}

val change_modr_m : (bits(8), option(bits(3)), option(bits(3)), option(bits(2))) -> bits(8)

function change_modr_m (input_bits, input_r_m, input_reg, input_mod) = {
    let output_r_m : bits(8) = match input_r_m {
      Some(input_r_m) => set_modr_m_get_r_m(input_r_m, input_bits),
      _ => input_bits
    };
    let output_reg : bits(8) = match input_reg {
      Some(input_reg) => set_modr_m_get_reg(input_reg, output_r_m),
      _ => output_r_m
    };
    let output_mod : bits(8) = match input_mod {
      Some(input_mod) => set_modr_m_get_mod(input_mod, output_reg),
      _ => output_reg
    };
    output_mod
}

val sib_get_base : bits(8) -> bits(3)

function sib_get_base inputbits = {
    genericBitstructAccessor(3, inputbits, 0)
}

val sib_get_index : bits(8) -> bits(3)

function sib_get_index inputbits = {
    genericBitstructAccessor(3, inputbits, 3)
}

val sib_get_scale : bits(8) -> bits(2)

function sib_get_scale inputbits = {
    genericBitstructAccessor(2, inputbits, 6)
}

val set_sib_get_base : (bits(3), bits(8)) -> bits(8)

function set_sib_get_base (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 0, inputbits)
}

val set_sib_get_index : (bits(3), bits(8)) -> bits(8)

function set_sib_get_index (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 3, inputbits)
}

val set_sib_get_scale : (bits(2), bits(8)) -> bits(8)

function set_sib_get_scale (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 6, inputbits)
}

val change_sib : (bits(8), option(bits(3)), option(bits(3)), option(bits(2))) -> bits(8)

function change_sib (input_bits, input_base, input_index, input_scale) = {
    let output_base : bits(8) = match input_base {
      Some(input_base) => set_sib_get_base(input_base, input_bits),
      _ => input_bits
    };
    let output_index : bits(8) = match input_index {
      Some(input_index) => set_sib_get_index(input_index, output_base),
      _ => output_base
    };
    let output_scale : bits(8) = match input_scale {
      Some(input_scale) => set_sib_get_scale(input_scale, output_index),
      _ => output_index
    };
    output_scale
}

val rflagsbits_get_cf : bits(32) -> bits(1)

function rflagsbits_get_cf inputbits = {
    genericBitstructAccessor(1, inputbits, 0)
}

val rflagsbits_get_res1 : bits(32) -> bits(1)

function rflagsbits_get_res1 inputbits = {
    genericBitstructAccessor(1, inputbits, 1)
}

val rflagsbits_get_pf : bits(32) -> bits(1)

function rflagsbits_get_pf inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val rflagsbits_get_res2 : bits(32) -> bits(1)

function rflagsbits_get_res2 inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val rflagsbits_get_af : bits(32) -> bits(1)

function rflagsbits_get_af inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val rflagsbits_get_res3 : bits(32) -> bits(1)

function rflagsbits_get_res3 inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val rflagsbits_get_zf : bits(32) -> bits(1)

function rflagsbits_get_zf inputbits = {
    genericBitstructAccessor(1, inputbits, 6)
}

val rflagsbits_get_sf : bits(32) -> bits(1)

function rflagsbits_get_sf inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val rflagsbits_get_tf : bits(32) -> bits(1)

function rflagsbits_get_tf inputbits = {
    genericBitstructAccessor(1, inputbits, 8)
}

val rflagsbits_get_intf : bits(32) -> bits(1)

function rflagsbits_get_intf inputbits = {
    genericBitstructAccessor(1, inputbits, 9)
}

val rflagsbits_get_df : bits(32) -> bits(1)

function rflagsbits_get_df inputbits = {
    genericBitstructAccessor(1, inputbits, 10)
}

val rflagsbits_get_of : bits(32) -> bits(1)

function rflagsbits_get_of inputbits = {
    genericBitstructAccessor(1, inputbits, 11)
}

val rflagsbits_get_iopl : bits(32) -> bits(2)

function rflagsbits_get_iopl inputbits = {
    genericBitstructAccessor(2, inputbits, 12)
}

val rflagsbits_get_nt : bits(32) -> bits(1)

function rflagsbits_get_nt inputbits = {
    genericBitstructAccessor(1, inputbits, 14)
}

val rflagsbits_get_res4 : bits(32) -> bits(1)

function rflagsbits_get_res4 inputbits = {
    genericBitstructAccessor(1, inputbits, 15)
}

val rflagsbits_get_rf : bits(32) -> bits(1)

function rflagsbits_get_rf inputbits = {
    genericBitstructAccessor(1, inputbits, 16)
}

val rflagsbits_get_vm : bits(32) -> bits(1)

function rflagsbits_get_vm inputbits = {
    genericBitstructAccessor(1, inputbits, 17)
}

val rflagsbits_get_ac : bits(32) -> bits(1)

function rflagsbits_get_ac inputbits = {
    genericBitstructAccessor(1, inputbits, 18)
}

val rflagsbits_get_vif : bits(32) -> bits(1)

function rflagsbits_get_vif inputbits = {
    genericBitstructAccessor(1, inputbits, 19)
}

val rflagsbits_get_vip : bits(32) -> bits(1)

function rflagsbits_get_vip inputbits = {
    genericBitstructAccessor(1, inputbits, 20)
}

val rflagsbits_get_id : bits(32) -> bits(1)

function rflagsbits_get_id inputbits = {
    genericBitstructAccessor(1, inputbits, 21)
}

val rflagsbits_get_res5 : bits(32) -> bits(10)

function rflagsbits_get_res5 inputbits = {
    genericBitstructAccessor(10, inputbits, 22)
}

val set_rflagsbits_get_cf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_cf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 0, inputbits)
}

val set_rflagsbits_get_res1 : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 1, inputbits)
}

val set_rflagsbits_get_pf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_pf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_rflagsbits_get_res2 : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_rflagsbits_get_af : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_af (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_rflagsbits_get_res3 : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_res3 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_rflagsbits_get_zf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_zf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 6, inputbits)
}

val set_rflagsbits_get_sf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_sf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val set_rflagsbits_get_tf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_tf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 8, inputbits)
}

val set_rflagsbits_get_intf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_intf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 9, inputbits)
}

val set_rflagsbits_get_df : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_df (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 10, inputbits)
}

val set_rflagsbits_get_of : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_of (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 11, inputbits)
}

val set_rflagsbits_get_iopl : (bits(2), bits(32)) -> bits(32)

function set_rflagsbits_get_iopl (splicebits, inputbits) = {
    genericBitstructUpdater(2, splicebits, 12, inputbits)
}

val set_rflagsbits_get_nt : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_nt (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 14, inputbits)
}

val set_rflagsbits_get_res4 : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_res4 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 15, inputbits)
}

val set_rflagsbits_get_rf : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_rf (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 16, inputbits)
}

val set_rflagsbits_get_vm : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_vm (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 17, inputbits)
}

val set_rflagsbits_get_ac : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_ac (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 18, inputbits)
}

val set_rflagsbits_get_vif : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_vif (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 19, inputbits)
}

val set_rflagsbits_get_vip : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_vip (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 20, inputbits)
}

val set_rflagsbits_get_id : (bits(1), bits(32)) -> bits(32)

function set_rflagsbits_get_id (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 21, inputbits)
}

val set_rflagsbits_get_res5 : (bits(10), bits(32)) -> bits(32)

function set_rflagsbits_get_res5 (splicebits, inputbits) = {
    genericBitstructUpdater(10, splicebits, 22, inputbits)
}

val change_rflagsbits : (bits(32), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(2)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(10))) -> bits(32)

function change_rflagsbits (input_bits, input_cf, input_res1, input_pf, input_res2, input_af, input_res3, input_zf, input_sf, input_tf, input_intf, input_df, input_of, input_iopl, input_nt, input_res4, input_rf, input_vm, input_ac, input_vif, input_vip, input_id, input_res5) = {
    let output_cf : bits(32) = match input_cf {
      Some(input_cf) => set_rflagsbits_get_cf(input_cf, input_bits),
      _ => input_bits
    };
    let output_res1 : bits(32) = match input_res1 {
      Some(input_res1) => set_rflagsbits_get_res1(input_res1, output_cf),
      _ => output_cf
    };
    let output_pf : bits(32) = match input_pf {
      Some(input_pf) => set_rflagsbits_get_pf(input_pf, output_res1),
      _ => output_res1
    };
    let output_res2 : bits(32) = match input_res2 {
      Some(input_res2) => set_rflagsbits_get_res2(input_res2, output_pf),
      _ => output_pf
    };
    let output_af : bits(32) = match input_af {
      Some(input_af) => set_rflagsbits_get_af(input_af, output_res2),
      _ => output_res2
    };
    let output_res3 : bits(32) = match input_res3 {
      Some(input_res3) => set_rflagsbits_get_res3(input_res3, output_af),
      _ => output_af
    };
    let output_zf : bits(32) = match input_zf {
      Some(input_zf) => set_rflagsbits_get_zf(input_zf, output_res3),
      _ => output_res3
    };
    let output_sf : bits(32) = match input_sf {
      Some(input_sf) => set_rflagsbits_get_sf(input_sf, output_zf),
      _ => output_zf
    };
    let output_tf : bits(32) = match input_tf {
      Some(input_tf) => set_rflagsbits_get_tf(input_tf, output_sf),
      _ => output_sf
    };
    let output_intf : bits(32) = match input_intf {
      Some(input_intf) => set_rflagsbits_get_intf(input_intf, output_tf),
      _ => output_tf
    };
    let output_df : bits(32) = match input_df {
      Some(input_df) => set_rflagsbits_get_df(input_df, output_intf),
      _ => output_intf
    };
    let output_of : bits(32) = match input_of {
      Some(input_of) => set_rflagsbits_get_of(input_of, output_df),
      _ => output_df
    };
    let output_iopl : bits(32) = match input_iopl {
      Some(input_iopl) => set_rflagsbits_get_iopl(input_iopl, output_of),
      _ => output_of
    };
    let output_nt : bits(32) = match input_nt {
      Some(input_nt) => set_rflagsbits_get_nt(input_nt, output_iopl),
      _ => output_iopl
    };
    let output_res4 : bits(32) = match input_res4 {
      Some(input_res4) => set_rflagsbits_get_res4(input_res4, output_nt),
      _ => output_nt
    };
    let output_rf : bits(32) = match input_rf {
      Some(input_rf) => set_rflagsbits_get_rf(input_rf, output_res4),
      _ => output_res4
    };
    let output_vm : bits(32) = match input_vm {
      Some(input_vm) => set_rflagsbits_get_vm(input_vm, output_rf),
      _ => output_rf
    };
    let output_ac : bits(32) = match input_ac {
      Some(input_ac) => set_rflagsbits_get_ac(input_ac, output_vm),
      _ => output_vm
    };
    let output_vif : bits(32) = match input_vif {
      Some(input_vif) => set_rflagsbits_get_vif(input_vif, output_ac),
      _ => output_ac
    };
    let output_vip : bits(32) = match input_vip {
      Some(input_vip) => set_rflagsbits_get_vip(input_vip, output_vif),
      _ => output_vif
    };
    let output_id : bits(32) = match input_id {
      Some(input_id) => set_rflagsbits_get_id(input_id, output_vip),
      _ => output_vip
    };
    let output_res5 : bits(32) = match input_res5 {
      Some(input_res5) => set_rflagsbits_get_res5(input_res5, output_id),
      _ => output_id
    };
    output_res5
}

val cr0bits_get_pe : bits(32) -> bits(1)

function cr0bits_get_pe inputbits = {
    genericBitstructAccessor(1, inputbits, 0)
}

val cr0bits_get_mp : bits(32) -> bits(1)

function cr0bits_get_mp inputbits = {
    genericBitstructAccessor(1, inputbits, 1)
}

val cr0bits_get_em : bits(32) -> bits(1)

function cr0bits_get_em inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val cr0bits_get_ts : bits(32) -> bits(1)

function cr0bits_get_ts inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val cr0bits_get_et : bits(32) -> bits(1)

function cr0bits_get_et inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val cr0bits_get_ne : bits(32) -> bits(1)

function cr0bits_get_ne inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val cr0bits_get_res1 : bits(32) -> bits(10)

function cr0bits_get_res1 inputbits = {
    genericBitstructAccessor(10, inputbits, 6)
}

val cr0bits_get_wp : bits(32) -> bits(1)

function cr0bits_get_wp inputbits = {
    genericBitstructAccessor(1, inputbits, 16)
}

val cr0bits_get_res2 : bits(32) -> bits(1)

function cr0bits_get_res2 inputbits = {
    genericBitstructAccessor(1, inputbits, 17)
}

val cr0bits_get_am : bits(32) -> bits(1)

function cr0bits_get_am inputbits = {
    genericBitstructAccessor(1, inputbits, 18)
}

val cr0bits_get_res3 : bits(32) -> bits(10)

function cr0bits_get_res3 inputbits = {
    genericBitstructAccessor(10, inputbits, 19)
}

val cr0bits_get_nw : bits(32) -> bits(1)

function cr0bits_get_nw inputbits = {
    genericBitstructAccessor(1, inputbits, 29)
}

val cr0bits_get_cd : bits(32) -> bits(1)

function cr0bits_get_cd inputbits = {
    genericBitstructAccessor(1, inputbits, 30)
}

val cr0bits_get_pg : bits(32) -> bits(1)

function cr0bits_get_pg inputbits = {
    genericBitstructAccessor(1, inputbits, 31)
}

val set_cr0bits_get_pe : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_pe (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 0, inputbits)
}

val set_cr0bits_get_mp : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_mp (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 1, inputbits)
}

val set_cr0bits_get_em : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_em (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_cr0bits_get_ts : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_ts (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_cr0bits_get_et : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_et (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_cr0bits_get_ne : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_ne (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_cr0bits_get_res1 : (bits(10), bits(32)) -> bits(32)

function set_cr0bits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(10, splicebits, 6, inputbits)
}

val set_cr0bits_get_wp : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_wp (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 16, inputbits)
}

val set_cr0bits_get_res2 : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 17, inputbits)
}

val set_cr0bits_get_am : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_am (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 18, inputbits)
}

val set_cr0bits_get_res3 : (bits(10), bits(32)) -> bits(32)

function set_cr0bits_get_res3 (splicebits, inputbits) = {
    genericBitstructUpdater(10, splicebits, 19, inputbits)
}

val set_cr0bits_get_nw : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_nw (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 29, inputbits)
}

val set_cr0bits_get_cd : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_cd (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 30, inputbits)
}

val set_cr0bits_get_pg : (bits(1), bits(32)) -> bits(32)

function set_cr0bits_get_pg (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 31, inputbits)
}

val change_cr0bits : (bits(32), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(10)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(10)), option(bits(1)), option(bits(1)), option(bits(1))) -> bits(32)

function change_cr0bits (input_bits, input_pe, input_mp, input_em, input_ts, input_et, input_ne, input_res1, input_wp, input_res2, input_am, input_res3, input_nw, input_cd, input_pg) = {
    let output_pe : bits(32) = match input_pe {
      Some(input_pe) => set_cr0bits_get_pe(input_pe, input_bits),
      _ => input_bits
    };
    let output_mp : bits(32) = match input_mp {
      Some(input_mp) => set_cr0bits_get_mp(input_mp, output_pe),
      _ => output_pe
    };
    let output_em : bits(32) = match input_em {
      Some(input_em) => set_cr0bits_get_em(input_em, output_mp),
      _ => output_mp
    };
    let output_ts : bits(32) = match input_ts {
      Some(input_ts) => set_cr0bits_get_ts(input_ts, output_em),
      _ => output_em
    };
    let output_et : bits(32) = match input_et {
      Some(input_et) => set_cr0bits_get_et(input_et, output_ts),
      _ => output_ts
    };
    let output_ne : bits(32) = match input_ne {
      Some(input_ne) => set_cr0bits_get_ne(input_ne, output_et),
      _ => output_et
    };
    let output_res1 : bits(32) = match input_res1 {
      Some(input_res1) => set_cr0bits_get_res1(input_res1, output_ne),
      _ => output_ne
    };
    let output_wp : bits(32) = match input_wp {
      Some(input_wp) => set_cr0bits_get_wp(input_wp, output_res1),
      _ => output_res1
    };
    let output_res2 : bits(32) = match input_res2 {
      Some(input_res2) => set_cr0bits_get_res2(input_res2, output_wp),
      _ => output_wp
    };
    let output_am : bits(32) = match input_am {
      Some(input_am) => set_cr0bits_get_am(input_am, output_res2),
      _ => output_res2
    };
    let output_res3 : bits(32) = match input_res3 {
      Some(input_res3) => set_cr0bits_get_res3(input_res3, output_am),
      _ => output_am
    };
    let output_nw : bits(32) = match input_nw {
      Some(input_nw) => set_cr0bits_get_nw(input_nw, output_res3),
      _ => output_res3
    };
    let output_cd : bits(32) = match input_cd {
      Some(input_cd) => set_cr0bits_get_cd(input_cd, output_nw),
      _ => output_nw
    };
    let output_pg : bits(32) = match input_pg {
      Some(input_pg) => set_cr0bits_get_pg(input_pg, output_cd),
      _ => output_cd
    };
    output_pg
}

val cr3bits_get_res1 : bits(64) -> bits(3)

function cr3bits_get_res1 inputbits = {
    genericBitstructAccessor(3, inputbits, 0)
}

val cr3bits_get_pwt : bits(64) -> bits(1)

function cr3bits_get_pwt inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val cr3bits_get_pcd : bits(64) -> bits(1)

function cr3bits_get_pcd inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val cr3bits_get_res2 : bits(64) -> bits(7)

function cr3bits_get_res2 inputbits = {
    genericBitstructAccessor(7, inputbits, 5)
}

val cr3bits_get_pdb : bits(64) -> bits(40)

function cr3bits_get_pdb inputbits = {
    genericBitstructAccessor(40, inputbits, 12)
}

val cr3bits_get_res3 : bits(64) -> bits(12)

function cr3bits_get_res3 inputbits = {
    genericBitstructAccessor(12, inputbits, 52)
}

val set_cr3bits_get_res1 : (bits(3), bits(64)) -> bits(64)

function set_cr3bits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(3, splicebits, 0, inputbits)
}

val set_cr3bits_get_pwt : (bits(1), bits(64)) -> bits(64)

function set_cr3bits_get_pwt (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_cr3bits_get_pcd : (bits(1), bits(64)) -> bits(64)

function set_cr3bits_get_pcd (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_cr3bits_get_res2 : (bits(7), bits(64)) -> bits(64)

function set_cr3bits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(7, splicebits, 5, inputbits)
}

val set_cr3bits_get_pdb : (bits(40), bits(64)) -> bits(64)

function set_cr3bits_get_pdb (splicebits, inputbits) = {
    genericBitstructUpdater(40, splicebits, 12, inputbits)
}

val set_cr3bits_get_res3 : (bits(12), bits(64)) -> bits(64)

function set_cr3bits_get_res3 (splicebits, inputbits) = {
    genericBitstructUpdater(12, splicebits, 52, inputbits)
}

val change_cr3bits : (bits(64), option(bits(3)), option(bits(1)), option(bits(1)), option(bits(7)), option(bits(40)), option(bits(12))) -> bits(64)

function change_cr3bits (input_bits, input_res1, input_pwt, input_pcd, input_res2, input_pdb, input_res3) = {
    let output_res1 : bits(64) = match input_res1 {
      Some(input_res1) => set_cr3bits_get_res1(input_res1, input_bits),
      _ => input_bits
    };
    let output_pwt : bits(64) = match input_pwt {
      Some(input_pwt) => set_cr3bits_get_pwt(input_pwt, output_res1),
      _ => output_res1
    };
    let output_pcd : bits(64) = match input_pcd {
      Some(input_pcd) => set_cr3bits_get_pcd(input_pcd, output_pwt),
      _ => output_pwt
    };
    let output_res2 : bits(64) = match input_res2 {
      Some(input_res2) => set_cr3bits_get_res2(input_res2, output_pcd),
      _ => output_pcd
    };
    let output_pdb : bits(64) = match input_pdb {
      Some(input_pdb) => set_cr3bits_get_pdb(input_pdb, output_res2),
      _ => output_res2
    };
    let output_res3 : bits(64) = match input_res3 {
      Some(input_res3) => set_cr3bits_get_res3(input_res3, output_pdb),
      _ => output_pdb
    };
    output_res3
}

val cr4bits_get_vme : bits(22) -> bits(1)

function cr4bits_get_vme inputbits = {
    genericBitstructAccessor(1, inputbits, 0)
}

val cr4bits_get_pvi : bits(22) -> bits(1)

function cr4bits_get_pvi inputbits = {
    genericBitstructAccessor(1, inputbits, 1)
}

val cr4bits_get_tsd : bits(22) -> bits(1)

function cr4bits_get_tsd inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val cr4bits_get_de : bits(22) -> bits(1)

function cr4bits_get_de inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val cr4bits_get_pse : bits(22) -> bits(1)

function cr4bits_get_pse inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val cr4bits_get_pae : bits(22) -> bits(1)

function cr4bits_get_pae inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val cr4bits_get_mce : bits(22) -> bits(1)

function cr4bits_get_mce inputbits = {
    genericBitstructAccessor(1, inputbits, 6)
}

val cr4bits_get_pge : bits(22) -> bits(1)

function cr4bits_get_pge inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val cr4bits_get_pce : bits(22) -> bits(1)

function cr4bits_get_pce inputbits = {
    genericBitstructAccessor(1, inputbits, 8)
}

val cr4bits_get_osfxsr : bits(22) -> bits(1)

function cr4bits_get_osfxsr inputbits = {
    genericBitstructAccessor(1, inputbits, 9)
}

val cr4bits_get_osxmmexcpt : bits(22) -> bits(1)

function cr4bits_get_osxmmexcpt inputbits = {
    genericBitstructAccessor(1, inputbits, 10)
}

val cr4bits_get_umip : bits(22) -> bits(1)

function cr4bits_get_umip inputbits = {
    genericBitstructAccessor(1, inputbits, 11)
}

val cr4bits_get_la57 : bits(22) -> bits(1)

function cr4bits_get_la57 inputbits = {
    genericBitstructAccessor(1, inputbits, 12)
}

val cr4bits_get_vmxe : bits(22) -> bits(1)

function cr4bits_get_vmxe inputbits = {
    genericBitstructAccessor(1, inputbits, 13)
}

val cr4bits_get_smxe : bits(22) -> bits(1)

function cr4bits_get_smxe inputbits = {
    genericBitstructAccessor(1, inputbits, 14)
}

val cr4bits_get_res1 : bits(22) -> bits(1)

function cr4bits_get_res1 inputbits = {
    genericBitstructAccessor(1, inputbits, 15)
}

val cr4bits_get_fsgsbase : bits(22) -> bits(1)

function cr4bits_get_fsgsbase inputbits = {
    genericBitstructAccessor(1, inputbits, 16)
}

val cr4bits_get_pcide : bits(22) -> bits(1)

function cr4bits_get_pcide inputbits = {
    genericBitstructAccessor(1, inputbits, 17)
}

val cr4bits_get_osxsave : bits(22) -> bits(1)

function cr4bits_get_osxsave inputbits = {
    genericBitstructAccessor(1, inputbits, 18)
}

val cr4bits_get_res2 : bits(22) -> bits(1)

function cr4bits_get_res2 inputbits = {
    genericBitstructAccessor(1, inputbits, 19)
}

val cr4bits_get_smep : bits(22) -> bits(1)

function cr4bits_get_smep inputbits = {
    genericBitstructAccessor(1, inputbits, 20)
}

val cr4bits_get_smap : bits(22) -> bits(1)

function cr4bits_get_smap inputbits = {
    genericBitstructAccessor(1, inputbits, 21)
}

val set_cr4bits_get_vme : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_vme (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 0, inputbits)
}

val set_cr4bits_get_pvi : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pvi (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 1, inputbits)
}

val set_cr4bits_get_tsd : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_tsd (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_cr4bits_get_de : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_de (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_cr4bits_get_pse : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pse (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_cr4bits_get_pae : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pae (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_cr4bits_get_mce : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_mce (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 6, inputbits)
}

val set_cr4bits_get_pge : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pge (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val set_cr4bits_get_pce : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pce (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 8, inputbits)
}

val set_cr4bits_get_osfxsr : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_osfxsr (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 9, inputbits)
}

val set_cr4bits_get_osxmmexcpt : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_osxmmexcpt (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 10, inputbits)
}

val set_cr4bits_get_umip : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_umip (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 11, inputbits)
}

val set_cr4bits_get_la57 : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_la57 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 12, inputbits)
}

val set_cr4bits_get_vmxe : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_vmxe (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 13, inputbits)
}

val set_cr4bits_get_smxe : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_smxe (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 14, inputbits)
}

val set_cr4bits_get_res1 : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 15, inputbits)
}

val set_cr4bits_get_fsgsbase : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_fsgsbase (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 16, inputbits)
}

val set_cr4bits_get_pcide : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_pcide (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 17, inputbits)
}

val set_cr4bits_get_osxsave : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_osxsave (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 18, inputbits)
}

val set_cr4bits_get_res2 : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 19, inputbits)
}

val set_cr4bits_get_smep : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_smep (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 20, inputbits)
}

val set_cr4bits_get_smap : (bits(1), bits(22)) -> bits(22)

function set_cr4bits_get_smap (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 21, inputbits)
}

val change_cr4bits : (bits(22), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1))) -> bits(22)

function change_cr4bits (input_bits, input_vme, input_pvi, input_tsd, input_de, input_pse, input_pae, input_mce, input_pge, input_pce, input_osfxsr, input_osxmmexcpt, input_umip, input_la57, input_vmxe, input_smxe, input_res1, input_fsgsbase, input_pcide, input_osxsave, input_res2, input_smep, input_smap) = {
    let output_vme : bits(22) = match input_vme {
      Some(input_vme) => set_cr4bits_get_vme(input_vme, input_bits),
      _ => input_bits
    };
    let output_pvi : bits(22) = match input_pvi {
      Some(input_pvi) => set_cr4bits_get_pvi(input_pvi, output_vme),
      _ => output_vme
    };
    let output_tsd : bits(22) = match input_tsd {
      Some(input_tsd) => set_cr4bits_get_tsd(input_tsd, output_pvi),
      _ => output_pvi
    };
    let output_de : bits(22) = match input_de {
      Some(input_de) => set_cr4bits_get_de(input_de, output_tsd),
      _ => output_tsd
    };
    let output_pse : bits(22) = match input_pse {
      Some(input_pse) => set_cr4bits_get_pse(input_pse, output_de),
      _ => output_de
    };
    let output_pae : bits(22) = match input_pae {
      Some(input_pae) => set_cr4bits_get_pae(input_pae, output_pse),
      _ => output_pse
    };
    let output_mce : bits(22) = match input_mce {
      Some(input_mce) => set_cr4bits_get_mce(input_mce, output_pae),
      _ => output_pae
    };
    let output_pge : bits(22) = match input_pge {
      Some(input_pge) => set_cr4bits_get_pge(input_pge, output_mce),
      _ => output_mce
    };
    let output_pce : bits(22) = match input_pce {
      Some(input_pce) => set_cr4bits_get_pce(input_pce, output_pge),
      _ => output_pge
    };
    let output_osfxsr : bits(22) = match input_osfxsr {
      Some(input_osfxsr) => set_cr4bits_get_osfxsr(input_osfxsr, output_pce),
      _ => output_pce
    };
    let output_osxmmexcpt : bits(22) = match input_osxmmexcpt {
      Some(input_osxmmexcpt) => set_cr4bits_get_osxmmexcpt(input_osxmmexcpt, output_osfxsr),
      _ => output_osfxsr
    };
    let output_umip : bits(22) = match input_umip {
      Some(input_umip) => set_cr4bits_get_umip(input_umip, output_osxmmexcpt),
      _ => output_osxmmexcpt
    };
    let output_la57 : bits(22) = match input_la57 {
      Some(input_la57) => set_cr4bits_get_la57(input_la57, output_umip),
      _ => output_umip
    };
    let output_vmxe : bits(22) = match input_vmxe {
      Some(input_vmxe) => set_cr4bits_get_vmxe(input_vmxe, output_la57),
      _ => output_la57
    };
    let output_smxe : bits(22) = match input_smxe {
      Some(input_smxe) => set_cr4bits_get_smxe(input_smxe, output_vmxe),
      _ => output_vmxe
    };
    let output_res1 : bits(22) = match input_res1 {
      Some(input_res1) => set_cr4bits_get_res1(input_res1, output_smxe),
      _ => output_smxe
    };
    let output_fsgsbase : bits(22) = match input_fsgsbase {
      Some(input_fsgsbase) => set_cr4bits_get_fsgsbase(input_fsgsbase, output_res1),
      _ => output_res1
    };
    let output_pcide : bits(22) = match input_pcide {
      Some(input_pcide) => set_cr4bits_get_pcide(input_pcide, output_fsgsbase),
      _ => output_fsgsbase
    };
    let output_osxsave : bits(22) = match input_osxsave {
      Some(input_osxsave) => set_cr4bits_get_osxsave(input_osxsave, output_pcide),
      _ => output_pcide
    };
    let output_res2 : bits(22) = match input_res2 {
      Some(input_res2) => set_cr4bits_get_res2(input_res2, output_osxsave),
      _ => output_osxsave
    };
    let output_smep : bits(22) = match input_smep {
      Some(input_smep) => set_cr4bits_get_smep(input_smep, output_res2),
      _ => output_res2
    };
    let output_smap : bits(22) = match input_smap {
      Some(input_smap) => set_cr4bits_get_smap(input_smap, output_smep),
      _ => output_smep
    };
    output_smap
}

val cr8bits_get_cr8_trpl : bits(4) -> bits(4)

function cr8bits_get_cr8_trpl inputbits = {
    genericBitstructAccessor(4, inputbits, 0)
}

val set_cr8bits_get_cr8_trpl : (bits(4), bits(4)) -> bits(4)

function set_cr8bits_get_cr8_trpl (splicebits, inputbits) = {
    genericBitstructUpdater(4, splicebits, 0, inputbits)
}

val change_cr8bits : (bits(4), option(bits(4))) -> bits(4)

function change_cr8bits (input_bits, input_cr8_trpl) = {
    let output_cr8_trpl : bits(4) = match input_cr8_trpl {
      Some(input_cr8_trpl) => set_cr8bits_get_cr8_trpl(input_cr8_trpl, input_bits),
      _ => input_bits
    };
    output_cr8_trpl
}

val xcr0bits_get_fpu_mmx_state : bits(64) -> bits(1)

function xcr0bits_get_fpu_mmx_state inputbits = {
    genericBitstructAccessor(1, inputbits, 0)
}

val xcr0bits_get_sse_state : bits(64) -> bits(1)

function xcr0bits_get_sse_state inputbits = {
    genericBitstructAccessor(1, inputbits, 1)
}

val xcr0bits_get_avx_state : bits(64) -> bits(1)

function xcr0bits_get_avx_state inputbits = {
    genericBitstructAccessor(1, inputbits, 2)
}

val xcr0bits_get_bndreg_state : bits(64) -> bits(1)

function xcr0bits_get_bndreg_state inputbits = {
    genericBitstructAccessor(1, inputbits, 3)
}

val xcr0bits_get_bndcsr_state : bits(64) -> bits(1)

function xcr0bits_get_bndcsr_state inputbits = {
    genericBitstructAccessor(1, inputbits, 4)
}

val xcr0bits_get_opmask_state : bits(64) -> bits(1)

function xcr0bits_get_opmask_state inputbits = {
    genericBitstructAccessor(1, inputbits, 5)
}

val xcr0bits_get_zmm_hi256_state : bits(64) -> bits(1)

function xcr0bits_get_zmm_hi256_state inputbits = {
    genericBitstructAccessor(1, inputbits, 6)
}

val xcr0bits_get_hi16_zmm_state : bits(64) -> bits(1)

function xcr0bits_get_hi16_zmm_state inputbits = {
    genericBitstructAccessor(1, inputbits, 7)
}

val xcr0bits_get_res1 : bits(64) -> bits(1)

function xcr0bits_get_res1 inputbits = {
    genericBitstructAccessor(1, inputbits, 8)
}

val xcr0bits_get_pkru_state : bits(64) -> bits(1)

function xcr0bits_get_pkru_state inputbits = {
    genericBitstructAccessor(1, inputbits, 9)
}

val xcr0bits_get_res2 : bits(64) -> bits(54)

function xcr0bits_get_res2 inputbits = {
    genericBitstructAccessor(54, inputbits, 10)
}

val set_xcr0bits_get_fpu_mmx_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_fpu_mmx_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 0, inputbits)
}

val set_xcr0bits_get_sse_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_sse_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 1, inputbits)
}

val set_xcr0bits_get_avx_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_avx_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 2, inputbits)
}

val set_xcr0bits_get_bndreg_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_bndreg_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 3, inputbits)
}

val set_xcr0bits_get_bndcsr_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_bndcsr_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 4, inputbits)
}

val set_xcr0bits_get_opmask_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_opmask_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 5, inputbits)
}

val set_xcr0bits_get_zmm_hi256_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_zmm_hi256_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 6, inputbits)
}

val set_xcr0bits_get_hi16_zmm_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_hi16_zmm_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 7, inputbits)
}

val set_xcr0bits_get_res1 : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 8, inputbits)
}

val set_xcr0bits_get_pkru_state : (bits(1), bits(64)) -> bits(64)

function set_xcr0bits_get_pkru_state (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 9, inputbits)
}

val set_xcr0bits_get_res2 : (bits(54), bits(64)) -> bits(64)

function set_xcr0bits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(54, splicebits, 10, inputbits)
}

val change_xcr0bits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(54))) -> bits(64)

function change_xcr0bits (input_bits, input_fpu_mmx_state, input_sse_state, input_avx_state, input_bndreg_state, input_bndcsr_state, input_opmask_state, input_zmm_hi256_state, input_hi16_zmm_state, input_res1, input_pkru_state, input_res2) = {
    let output_fpu_mmx_state : bits(64) = match input_fpu_mmx_state {
      Some(input_fpu_mmx_state) => set_xcr0bits_get_fpu_mmx_state(input_fpu_mmx_state, input_bits),
      _ => input_bits
    };
    let output_sse_state : bits(64) = match input_sse_state {
      Some(input_sse_state) => set_xcr0bits_get_sse_state(input_sse_state, output_fpu_mmx_state),
      _ => output_fpu_mmx_state
    };
    let output_avx_state : bits(64) = match input_avx_state {
      Some(input_avx_state) => set_xcr0bits_get_avx_state(input_avx_state, output_sse_state),
      _ => output_sse_state
    };
    let output_bndreg_state : bits(64) = match input_bndreg_state {
      Some(input_bndreg_state) => set_xcr0bits_get_bndreg_state(input_bndreg_state, output_avx_state),
      _ => output_avx_state
    };
    let output_bndcsr_state : bits(64) = match input_bndcsr_state {
      Some(input_bndcsr_state) => set_xcr0bits_get_bndcsr_state(input_bndcsr_state, output_bndreg_state),
      _ => output_bndreg_state
    };
    let output_opmask_state : bits(64) = match input_opmask_state {
      Some(input_opmask_state) => set_xcr0bits_get_opmask_state(input_opmask_state, output_bndcsr_state),
      _ => output_bndcsr_state
    };
    let output_zmm_hi256_state : bits(64) = match input_zmm_hi256_state {
      Some(input_zmm_hi256_state) => set_xcr0bits_get_zmm_hi256_state(input_zmm_hi256_state, output_opmask_state),
      _ => output_opmask_state
    };
    let output_hi16_zmm_state : bits(64) = match input_hi16_zmm_state {
      Some(input_hi16_zmm_state) => set_xcr0bits_get_hi16_zmm_state(input_hi16_zmm_state, output_zmm_hi256_state),
      _ => output_zmm_hi256_state
    };
    let output_res1 : bits(64) = match input_res1 {
      Some(input_res1) => set_xcr0bits_get_res1(input_res1, output_hi16_zmm_state),
      _ => output_hi16_zmm_state
    };
    let output_pkru_state : bits(64) = match input_pkru_state {
      Some(input_pkru_state) => set_xcr0bits_get_pkru_state(input_pkru_state, output_res1),
      _ => output_res1
    };
    let output_res2 : bits(64) = match input_res2 {
      Some(input_res2) => set_xcr0bits_get_res2(input_res2, output_pkru_state),
      _ => output_pkru_state
    };
    output_res2
}

val ia32_eferbits_get_sce : bits(12) -> bits(1)

function ia32_eferbits_get_sce inputbits = {
    genericBitstructAccessor(1, inputbits, 0)
}

val ia32_eferbits_get_res1 : bits(12) -> bits(7)

function ia32_eferbits_get_res1 inputbits = {
    genericBitstructAccessor(7, inputbits, 1)
}

val ia32_eferbits_get_lme : bits(12) -> bits(1)

function ia32_eferbits_get_lme inputbits = {
    genericBitstructAccessor(1, inputbits, 8)
}

val ia32_eferbits_get_res2 : bits(12) -> bits(1)

function ia32_eferbits_get_res2 inputbits = {
    genericBitstructAccessor(1, inputbits, 9)
}

val ia32_eferbits_get_lma : bits(12) -> bits(1)

function ia32_eferbits_get_lma inputbits = {
    genericBitstructAccessor(1, inputbits, 10)
}

val ia32_eferbits_get_nxe : bits(12) -> bits(1)

function ia32_eferbits_get_nxe inputbits = {
    genericBitstructAccessor(1, inputbits, 11)
}

val set_ia32_eferbits_get_sce : (bits(1), bits(12)) -> bits(12)

function set_ia32_eferbits_get_sce (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 0, inputbits)
}

val set_ia32_eferbits_get_res1 : (bits(7), bits(12)) -> bits(12)

function set_ia32_eferbits_get_res1 (splicebits, inputbits) = {
    genericBitstructUpdater(7, splicebits, 1, inputbits)
}

val set_ia32_eferbits_get_lme : (bits(1), bits(12)) -> bits(12)

function set_ia32_eferbits_get_lme (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 8, inputbits)
}

val set_ia32_eferbits_get_res2 : (bits(1), bits(12)) -> bits(12)

function set_ia32_eferbits_get_res2 (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 9, inputbits)
}

val set_ia32_eferbits_get_lma : (bits(1), bits(12)) -> bits(12)

function set_ia32_eferbits_get_lma (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 10, inputbits)
}

val set_ia32_eferbits_get_nxe : (bits(1), bits(12)) -> bits(12)

function set_ia32_eferbits_get_nxe (splicebits, inputbits) = {
    genericBitstructUpdater(1, splicebits, 11, inputbits)
}

val change_ia32_eferbits : (bits(12), option(bits(1)), option(bits(7)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1))) -> bits(12)

function change_ia32_eferbits (input_bits, input_sce, input_res1, input_lme, input_res2, input_lma, input_nxe) = {
    let output_sce : bits(12) = match input_sce {
      Some(input_sce) => set_ia32_eferbits_get_sce(input_sce, input_bits),
      _ => input_bits
    };
    let output_res1 : bits(12) = match input_res1 {
      Some(input_res1) => set_ia32_eferbits_get_res1(input_res1, output_sce),
      _ => output_sce
    };
    let output_lme : bits(12) = match input_lme {
      Some(input_lme) => set_ia32_eferbits_get_lme(input_lme, output_res1),
      _ => output_res1
    };
    let output_res2 : bits(12) = match input_res2 {
      Some(input_res2) => set_ia32_eferbits_get_res2(input_res2, output_lme),
      _ => output_lme
    };
    let output_lma : bits(12) = match input_lma {
      Some(input_lma) => set_ia32_eferbits_get_lma(input_lma, output_res2),
      _ => output_res2
    };
    let output_nxe : bits(12) = match input_nxe {
      Some(input_nxe) => set_ia32_eferbits_get_nxe(input_nxe, output_lma),
      _ => output_lma
    };
    output_nxe
}
