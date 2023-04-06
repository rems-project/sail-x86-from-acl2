$include "./paging.sail"

/*! Definitions of @ ( see rb ) and @ ( see wb )
<p>The functions @ ( ' rb ' ) ( read bytes ) and @ ( ' wb ' ) ( write 
 bytes ) are used in reasoning about memory reads and writes. Functions 
 like @ ( ' rml08 ' ) , @ ( ' rml16 ' ) , @ ( ' rml32 ' ) , and @ ( ' rml64 ' ) are reduced to 
 @ ( ' rb ' ) , and @ ( ' wml08 ' ) , @ ( ' wml16 ' ) , @ ( ' wml32 ' ) , and @ ( ' wml64 ' ) to 
 @ ( ' wb ' ) during reasoning.</p> */
val rml08 : (sbits(64), string) -> bits(8)

function rml08 (lin_addr, r_x) = {
    rb(1, lin_addr, r_x)
}

val riml08 : (sbits(64), string) -> sbits(8)

function riml08 (lin_addr, r_x) = {
    let sailval : bits(8) = rml08(lin_addr, r_x);
    sailval
}

val wml08 : (sbits(64), bits(8)) -> unit

function wml08 (lin_addr, sailval) = {
    wb(1, lin_addr, ":W", sailval)
}

val wiml08 : (sbits(64), sbits(8)) -> unit

function wiml08 (lin_addr, sailval) = {
    wml08(lin_addr, sailval)
}

val rml16 : (sbits(64), string) -> bits(16)

function rml16 (lin_addr, r_x) = {
    let one__plus_lin_addr : sbits(65) = bits_of_int(1 + signed(lin_addr), 65);
    if canonical_address_p(signed(one__plus_lin_addr)) then {
        rb(2, lin_addr, r_x)
    } else {
        x86_model_error("rml16")
    }
}

val riml16 : (sbits(64), string) -> sbits(16)

function riml16 (lin_addr, r_x) = {
    let sailval : bits(16) = rml16(lin_addr, r_x);
    sailval
}

val wml16 : (sbits(64), bits(16)) -> unit

function wml16 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one__plus_lin_addr : sbits(65) = bits_of_int(1 + signed(lin_addr), 65);
    if canonical_address_p(signed(one__plus_lin_addr)) then {
        wb(2, lin_addr, ":W", sailval)
    } else {
        x86_model_error("wml16")
    }
}

val wiml16 : (sbits(64), sbits(16)) -> unit

function wiml16 (lin_addr, sailval) = {
    wml16(lin_addr, sailval)
}

val rml32 : (sbits(64), string) -> bits(32)

function rml32 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let three__plus_lin_addr : sbits(65) = bits_of_int(3 + signed(lin_addr), 65);
    if canonical_address_p(signed(three__plus_lin_addr)) then {
        rb(4, lin_addr, r_x)
    } else {
        x86_model_error("rml32")
    }
}

val riml32 : (sbits(64), string) -> sbits(32)

function riml32 (lin_addr, r_x) = {
    let sailval : bits(32) = rml32(lin_addr, r_x);
    sailval
}

val wml32 : (sbits(64), bits(32)) -> unit

function wml32 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let three__plus_lin_addr : sbits(65) = bits_of_int(3 + signed(lin_addr), 65);
    if canonical_address_p(signed(three__plus_lin_addr)) then {
        wb(4, lin_addr, ":W", sailval)
    } else {
        x86_model_error("wml32")
    }
}

val wiml32 : (sbits(64), sbits(32)) -> unit

function wiml32 (lin_addr, sailval) = {
    wml32(lin_addr, sailval)
}

val rml48 : (sbits(64), string) -> bits(48)

function rml48 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let five__plus_lin_addr : sbits(65) = bits_of_int(5 + signed(lin_addr), 65);
    if canonical_address_p(signed(five__plus_lin_addr)) then {
        rb(6, lin_addr, r_x)
    } else {
        x86_model_error("rml48")
    }
}

val wml48 : (sbits(64), bits(48)) -> unit

function wml48 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let five__plus_lin_addr : sbits(65) = bits_of_int(5 + signed(lin_addr), 65);
    if canonical_address_p(signed(five__plus_lin_addr)) then {
        wb(6, lin_addr, ":W", sailval)
    } else {
        x86_model_error("wml48")
    }
}

val rml64 : (sbits(64), string) -> bits(64)

function rml64 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let seven__plus_lin_addr : sbits(65) = bits_of_int(7 + signed(lin_addr), 65);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        rb(8, lin_addr, r_x)
    } else {
        x86_model_error("rml64")
    }
}

val riml64 : (sbits(64), string) -> sbits(64)

function riml64 (lin_addr, r_x) = {
    let sailval : bits(64) = rml64(lin_addr, r_x);
    sailval
}

val wml64 : (sbits(64), bits(64)) -> unit

function wml64 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let seven__plus_lin_addr : sbits(66) = bits_of_int(7 + signed(lin_addr), 66);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        wb(8, lin_addr, ":W", sailval)
    } else {
        x86_model_error("wml64")
    }
}

val wiml64 : (sbits(64), sbits(64)) -> unit

function wiml64 (lin_addr, sailval) = {
    wml64(lin_addr, sailval)
}

val rml80 : (sbits(64), string) -> bits(80)

function rml80 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let nine__plus_lin_addr : sbits(65) = bits_of_int(9 + signed(lin_addr), 65);
    if canonical_address_p(signed(nine__plus_lin_addr)) then {
        rb(10, lin_addr, r_x)
    } else {
        x86_model_error("rml80")
    }
}

val wml80 : (sbits(64), bits(80)) -> unit

function wml80 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let nine__plus_lin_addr : sbits(65) = bits_of_int(9 + signed(lin_addr), 65);
    if canonical_address_p(signed(nine__plus_lin_addr)) then {
        wb(10, lin_addr, ":W", sailval)
    } else {
        x86_model_error("wml80")
    }
}

val rml128 : (sbits(64), string) -> bits(128)

function rml128 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one_5_plus_lin_addr : sbits(65) = bits_of_int(15 + signed(lin_addr), 65);
    if canonical_address_p(signed(one_5_plus_lin_addr)) then {
        if app_view then {
            rb(16, lin_addr, r_x)
        } else {
            let p_addr0 : bits(52) = la_to_pa(lin_addr, r_x);
            let one__plus_lin_addr : sbits(65) = bits_of_int(1 + signed(lin_addr), 65);
            let p_addr1 : bits(52) = la_to_pa(truncate(one__plus_lin_addr, 64), r_x);
            let _2_plus_lin_addr : sbits(66) = bits_of_int(2 + signed(lin_addr), 66);
            let p_addr2 : bits(52) = la_to_pa(truncate(_2_plus_lin_addr, 64), r_x);
            let three__plus_lin_addr : sbits(67) = bits_of_int(3 + signed(lin_addr), 67);
            let p_addr3 : bits(52) = la_to_pa(truncate(three__plus_lin_addr, 64), r_x);
            let _4_plus_lin_addr : sbits(68) = bits_of_int(4 + signed(lin_addr), 68);
            let p_addr4 : bits(52) = la_to_pa(truncate(_4_plus_lin_addr, 64), r_x);
            let five__plus_lin_addr : sbits(69) = bits_of_int(5 + signed(lin_addr), 69);
            let p_addr5 : bits(52) = la_to_pa(truncate(five__plus_lin_addr, 64), r_x);
            let _6_plus_lin_addr : sbits(70) = bits_of_int(6 + signed(lin_addr), 70);
            let p_addr6 : bits(52) = la_to_pa(truncate(_6_plus_lin_addr, 64), r_x);
            let seven__plus_lin_addr : sbits(71) = bits_of_int(7 + signed(lin_addr), 71);
            let p_addr7 : bits(52) = la_to_pa(truncate(seven__plus_lin_addr, 64), r_x);
            let _8_plus_lin_addr : sbits(72) = bits_of_int(8 + signed(lin_addr), 72);
            let p_addr8 : bits(52) = la_to_pa(truncate(_8_plus_lin_addr, 64), r_x);
            let nine__plus_lin_addr : sbits(73) = bits_of_int(9 + signed(lin_addr), 73);
            let p_addr9 : bits(52) = la_to_pa(truncate(nine__plus_lin_addr, 64), r_x);
            let one_0_plus_lin_addr : sbits(74) = bits_of_int(10 + signed(lin_addr), 74);
            let p_addr10 : bits(52) = la_to_pa(truncate(one_0_plus_lin_addr, 64), r_x);
            let one_1_plus_lin_addr : sbits(75) = bits_of_int(11 + signed(lin_addr), 75);
            let p_addr11 : bits(52) = la_to_pa(truncate(one_1_plus_lin_addr, 64), r_x);
            let one_2_plus_lin_addr : sbits(76) = bits_of_int(12 + signed(lin_addr), 76);
            let p_addr12 : bits(52) = la_to_pa(truncate(one_2_plus_lin_addr, 64), r_x);
            let one_3_plus_lin_addr : sbits(77) = bits_of_int(13 + signed(lin_addr), 77);
            let p_addr13 : bits(52) = la_to_pa(truncate(one_3_plus_lin_addr, 64), r_x);
            let one_4_plus_lin_addr : sbits(78) = bits_of_int(14 + signed(lin_addr), 78);
            let p_addr14 : bits(52) = la_to_pa(truncate(one_4_plus_lin_addr, 64), r_x);
            let one_5_plus_lin_addr : sbits(79) = bits_of_int(15 + signed(lin_addr), 79);
            let p_addr15 : bits(52) = la_to_pa(truncate(one_5_plus_lin_addr, 64), r_x);
            let byte0 : bits(8) = memi(sail_zero_extend(p_addr0, 64));
            let byte1 : bits(8) = memi(sail_zero_extend(p_addr1, 64));
            let byte2 : bits(8) = memi(sail_zero_extend(p_addr2, 64));
            let byte3 : bits(8) = memi(sail_zero_extend(p_addr3, 64));
            let byte4 : bits(8) = memi(sail_zero_extend(p_addr4, 64));
            let byte5 : bits(8) = memi(sail_zero_extend(p_addr5, 64));
            let byte6 : bits(8) = memi(sail_zero_extend(p_addr6, 64));
            let byte7 : bits(8) = memi(sail_zero_extend(p_addr7, 64));
            let byte8 : bits(8) = memi(sail_zero_extend(p_addr8, 64));
            let byte9 : bits(8) = memi(sail_zero_extend(p_addr9, 64));
            let byte10 : bits(8) = memi(sail_zero_extend(p_addr10, 64));
            let byte11 : bits(8) = memi(sail_zero_extend(p_addr11, 64));
            let byte12 : bits(8) = memi(sail_zero_extend(p_addr12, 64));
            let byte13 : bits(8) = memi(sail_zero_extend(p_addr13, 64));
            let byte14 : bits(8) = memi(sail_zero_extend(p_addr14, 64));
            let byte15 : bits(8) = memi(sail_zero_extend(p_addr15, 64));
            let oword : bits(128) = byte15 @ (byte14 @ (byte13 @ (byte12 @ (byte11 @ (byte10 @ (byte9 @ (byte8 @ (byte7 @ (byte6 @ (byte5 @ (byte4 @ (byte3 @ (byte2 @ (byte1 @ byte0))))))))))))));
            oword
        }
    } else {
        x86_model_error("rml128")
    }
}

val wml128 : (sbits(64), bits(128)) -> unit

function wml128 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one_5_plus_lin_addr : sbits(65) = bits_of_int(15 + signed(lin_addr), 65);
    if canonical_address_p(signed(one_5_plus_lin_addr)) then {
        if app_view then {
            wb(16, lin_addr, ":W", sailval)
        } else {
            let p_addr0 : bits(52) = la_to_pa(lin_addr, ":W");
            let one__plus_lin_addr : sbits(65) = bits_of_int(1 + signed(lin_addr), 65);
            let p_addr1 : bits(52) = la_to_pa(truncate(one__plus_lin_addr, 64), ":W");
            let _2_plus_lin_addr : sbits(66) = bits_of_int(2 + signed(lin_addr), 66);
            let p_addr2 : bits(52) = la_to_pa(truncate(_2_plus_lin_addr, 64), ":W");
            let three__plus_lin_addr : sbits(67) = bits_of_int(3 + signed(lin_addr), 67);
            let p_addr3 : bits(52) = la_to_pa(truncate(three__plus_lin_addr, 64), ":W");
            let _4_plus_lin_addr : sbits(68) = bits_of_int(4 + signed(lin_addr), 68);
            let p_addr4 : bits(52) = la_to_pa(truncate(_4_plus_lin_addr, 64), ":W");
            let five__plus_lin_addr : sbits(69) = bits_of_int(5 + signed(lin_addr), 69);
            let p_addr5 : bits(52) = la_to_pa(truncate(five__plus_lin_addr, 64), ":W");
            let _6_plus_lin_addr : sbits(70) = bits_of_int(6 + signed(lin_addr), 70);
            let p_addr6 : bits(52) = la_to_pa(truncate(_6_plus_lin_addr, 64), ":W");
            let seven__plus_lin_addr : sbits(71) = bits_of_int(7 + signed(lin_addr), 71);
            let p_addr7 : bits(52) = la_to_pa(truncate(seven__plus_lin_addr, 64), ":W");
            let _8_plus_lin_addr : sbits(72) = bits_of_int(8 + signed(lin_addr), 72);
            let p_addr8 : bits(52) = la_to_pa(truncate(_8_plus_lin_addr, 64), ":W");
            let nine__plus_lin_addr : sbits(73) = bits_of_int(9 + signed(lin_addr), 73);
            let p_addr9 : bits(52) = la_to_pa(truncate(nine__plus_lin_addr, 64), ":W");
            let one_0_plus_lin_addr : sbits(74) = bits_of_int(10 + signed(lin_addr), 74);
            let p_addr10 : bits(52) = la_to_pa(truncate(one_0_plus_lin_addr, 64), ":W");
            let one_1_plus_lin_addr : sbits(75) = bits_of_int(11 + signed(lin_addr), 75);
            let p_addr11 : bits(52) = la_to_pa(truncate(one_1_plus_lin_addr, 64), ":W");
            let one_2_plus_lin_addr : sbits(76) = bits_of_int(12 + signed(lin_addr), 76);
            let p_addr12 : bits(52) = la_to_pa(truncate(one_2_plus_lin_addr, 64), ":W");
            let one_3_plus_lin_addr : sbits(77) = bits_of_int(13 + signed(lin_addr), 77);
            let p_addr13 : bits(52) = la_to_pa(truncate(one_3_plus_lin_addr, 64), ":W");
            let one_4_plus_lin_addr : sbits(78) = bits_of_int(14 + signed(lin_addr), 78);
            let p_addr14 : bits(52) = la_to_pa(truncate(one_4_plus_lin_addr, 64), ":W");
            let one_5_plus_lin_addr : sbits(79) = bits_of_int(15 + signed(lin_addr), 79);
            let p_addr15 : bits(52) = la_to_pa(truncate(one_5_plus_lin_addr, 64), ":W");
            let byte0 : bits(8) = sailval[7 .. 0];
            let byte1 : bits(8) = sailval[15 .. 8];
            let byte2 : bits(8) = sailval[23 .. 16];
            let byte3 : bits(8) = sailval[31 .. 24];
            let byte4 : bits(8) = sailval[39 .. 32];
            let byte5 : bits(8) = sailval[47 .. 40];
            let byte6 : bits(8) = sailval[55 .. 48];
            let byte7 : bits(8) = sailval[63 .. 56];
            let byte8 : bits(8) = sailval[71 .. 64];
            let byte9 : bits(8) = sailval[79 .. 72];
            let byte10 : bits(8) = sailval[87 .. 80];
            let byte11 : bits(8) = sailval[95 .. 88];
            let byte12 : bits(8) = sailval[103 .. 96];
            let byte13 : bits(8) = sailval[111 .. 104];
            let byte14 : bits(8) = sailval[119 .. 112];
            let byte15 : bits(8) = sailval[127 .. 120];
            bang_memi(sail_zero_extend(p_addr0, 64), byte0);
            bang_memi(sail_zero_extend(p_addr1, 64), byte1);
            bang_memi(sail_zero_extend(p_addr2, 64), byte2);
            bang_memi(sail_zero_extend(p_addr3, 64), byte3);
            bang_memi(sail_zero_extend(p_addr4, 64), byte4);
            bang_memi(sail_zero_extend(p_addr5, 64), byte5);
            bang_memi(sail_zero_extend(p_addr6, 64), byte6);
            bang_memi(sail_zero_extend(p_addr7, 64), byte7);
            bang_memi(sail_zero_extend(p_addr8, 64), byte8);
            bang_memi(sail_zero_extend(p_addr9, 64), byte9);
            bang_memi(sail_zero_extend(p_addr10, 64), byte10);
            bang_memi(sail_zero_extend(p_addr11, 64), byte11);
            bang_memi(sail_zero_extend(p_addr12, 64), byte12);
            bang_memi(sail_zero_extend(p_addr13, 64), byte13);
            bang_memi(sail_zero_extend(p_addr14, 64), byte14);
            bang_memi(sail_zero_extend(p_addr15, 64), byte15)
        }
    } else {
        x86_model_error("wml128")
    }
}

/*! Functions to read/write 8/16/32/64/128 bits into the memory: */
val rml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(64), string) -> bits(128)

function rml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => sail_zero_extend(rml08(addr, r_x), 128),
      2 => sail_zero_extend(rml16(addr, r_x), 128),
      4 => sail_zero_extend(rml32(addr, r_x), 128),
      6 => sail_zero_extend(rml48(addr, r_x), 128),
      8 => sail_zero_extend(rml64(addr, r_x), 128),
      10 => sail_zero_extend(rml80(addr, r_x), 128),
      16 => rml128(addr, r_x),
      _ => {
          if canonical_address_p(-1 + (nbytes + signed(addr))) then {
              sail_mask(128, rb(nbytes, addr, r_x))
          } else {
              x86_model_error("rml-size")
          }
      }
    }) : bits(128)
}

val riml_size : ({|1, 2, 4, 8|}, sbits(64), string) -> sbits(64)

function riml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => sail_sign_extend(riml08(addr, r_x), 64),
      2 => sail_sign_extend(riml16(addr, r_x), 64),
      4 => sail_sign_extend(riml32(addr, r_x), 64),
      8 => riml64(addr, r_x),
      _ => x86_model_error("riml-size")
    }) : sbits(64)
}

val wml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(64), nat) -> unit

function wml_size (nbytes, addr, sailval) = {
    (match nbytes {
      1 => wml08(addr, bits_of_int(sailval, 8)),
      2 => wml16(addr, bits_of_int(sailval, 16)),
      4 => wml32(addr, bits_of_int(sailval, 32)),
      6 => wml48(addr, bits_of_int(sailval, 48)),
      8 => wml64(addr, bits_of_int(sailval, 64)),
      10 => wml80(addr, bits_of_int(sailval, 80)),
      16 => wml128(addr, bits_of_int(sailval, 128)),
      _ => {
          if canonical_address_p(-1 + (nbytes + signed(addr))) then {
              wb(nbytes, addr, ":W", bits_of_int(sailval, 8 * nbytes))
          } else {
              x86_model_error("wml-size")
          }
      }
    }) : unit
}

val wiml_size : ({|1, 2, 4, 8|}, sbits(64), int) -> unit

function wiml_size (nbytes, addr, sailval) = {
    (match nbytes {
      1 => wiml08(addr, bits_of_int(sailval, 8)),
      2 => wiml16(addr, bits_of_int(sailval, 16)),
      4 => wiml32(addr, bits_of_int(sailval, 32)),
      8 => wiml64(addr, bits_of_int(sailval, 64)),
      _ => x86_model_error("wiml-size")
    }) : unit
}

val write_canonical_address_to_memory_user_exec : (sbits(64), sbits(64)) -> option(string)

function write_canonical_address_to_memory_user_exec (lin_addr, canonical_address) = {
    assert(app_view & canonical_address_p(7 + signed(lin_addr)));
    let canonical_address_low_nat : bits(32) = truncate(canonical_address, 32);
    let canonical_address_high_int : sbits(32) = sail_sign_extend(sail_zero_extend(canonical_address[47 .. 32], 16), 32);
    wml32(lin_addr, canonical_address_low_nat);
    let next_addr : sbits(65) = bits_of_int(4 + signed(lin_addr), 65);
    if not_bool(canonical_address_p(signed(next_addr))) then x86_model_error("wml64-canonical-address-user-view");
    wiml32(truncate(next_addr, 64), canonical_address_high_int);
    None() : option(string)
}

val write_canonical_address_to_memory : (sbits(64), sbits(64)) -> unit

function write_canonical_address_to_memory (lin_addr, canonical_address) = {
    let seven__plus_lin_addr : sbits(66) = bits_of_int(7 + signed(lin_addr), 66);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        if app_view then {
            wb(8, lin_addr, ":W", loghead(64, canonical_address))
        } else {
            let canonical_address_unsigned_val : bits(64) = loghead(64, canonical_address);
            wml64(lin_addr, canonical_address_unsigned_val)
        }
    } else {
        x86_model_error("write-canonical-address-to-memory-error")
    }
}
