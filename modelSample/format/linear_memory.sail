$include "./paging.sail"

val rb_1 : (nat, int, string) -> (option(string), int)

function rb_1 (n, addr, r_x) = {
    if n == 0 then {
        (None() : option(string), 0 : {|0|})
    } else if not_bool(canonical_address_p(get_slice_int(48, addr, 0))) then {
        (Some("rb-1"), 0 : {|0|})
    } else {
        let (flg0, byte0) = rvm08(get_slice_int(48, addr, 0)) : (option(string), bits(8));
        if is_some(flg0) then {
            (flg0, 0 : {|0|})
        } else {
            let (rest_flg, rest_bytes) = rb_1(the_nat(n - 1), 1 + addr, r_x) : (option(string), int);
            (rest_flg, logior(unsigned(byte0), ash(rest_bytes, 8)))
        }
    }
}

val rb : (nat, int, string) -> (option(string), int)

function rb (n, addr, r_x) = {
    rb_1(n, addr, r_x)
}

val wb_1 : (nat, int, string, int) -> option(string)

function wb_1 (n, addr, w, value) = {
    if n == 0 then {
        None() : option(string)
    } else if not_bool(canonical_address_p(get_slice_int(48, addr, 0))) then {
        Some("Empty error")
    } else {
        let flg0 = wvm08(get_slice_int(48, addr, 0), get_slice_int(8, loghead(8, value), 0)) : option(string);
        if is_some(flg0) then {
            flg0
        } else {
            let rest_flg = wb_1(the_nat(n - 1), 1 + addr, w, logtail(8, value)) : option(string);
            rest_flg
        }
    }
}

val wb : (nat, int, string, nat) -> option(string)

function wb (n, addr, w, value) = {
    wb_1(n, addr, w, value)
}

val rml08 : (sbits(48), string) -> (option(string), int)

function rml08 (lin_addr, r_x) = {
    rb(1, signed(lin_addr), r_x)
}

val riml08 : (sbits(48), string) -> (option(string), sbits(8))

function riml08 (lin_addr, r_x) = {
    let (flag, sailval) = rml08(lin_addr, r_x) : (option(string), int);
    (flag, n08_to_i08(get_slice_int(8, sailval, 0)))
}

val wml08 : (sbits(48), bits(8)) -> option(string)

function wml08 (lin_addr, sailval) = {
    wb(1, signed(lin_addr), ":W", unsigned(sailval))
}

val wiml08 : (sbits(48), sbits(8)) -> option(string)

function wiml08 (lin_addr, sailval) = {
    wml08(lin_addr, n08(sailval))
}

val rml16 : (sbits(48), string) -> (option(string), int)

function rml16 (lin_addr, r_x) = {
    let one__plus_lin_addr = the_sbits(49, 1 + signed(lin_addr)) : sbits(49);
    if canonical_address_p(the_sbits(48, one__plus_lin_addr)) then {
        rb(2, signed(lin_addr), r_x)
    } else {
        (Some("rml16"), 0 : {|0|})
    }
}

val riml16 : (sbits(48), string) -> (option(string), sbits(16))

function riml16 (lin_addr, r_x) = {
    let (flag, sailval) = rml16(lin_addr, r_x) : (option(string), int);
    (flag, n16_to_i16(get_slice_int(16, sailval, 0)))
}

val wml16 : (sbits(48), bits(16)) -> option(string)

function wml16 (lin_addr, sailval) = {
    if true then {
        let one__plus_lin_addr = the_sbits(49, 1 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, one__plus_lin_addr)) then {
            wb(2, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml16")
        }
    } else {
        Some("wml16")
    }
}

val wiml16 : (sbits(48), sbits(16)) -> option(string)

function wiml16 (lin_addr, sailval) = {
    wml16(lin_addr, n16(sailval))
}

val rml32 : (sbits(48), string) -> (option(string), int)

function rml32 (lin_addr, r_x) = {
    if true then {
        let three__plus_lin_addr = the_sbits(49, 3 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, three__plus_lin_addr)) then {
            rb(4, signed(lin_addr), r_x)
        } else {
            (Some("rml32"), 0 : {|0|})
        }
    } else {
        (Some("rml32"), 0 : {|0|})
    }
}

val riml32 : (sbits(48), string) -> (option(string), sbits(32))

function riml32 (lin_addr, r_x) = {
    let (flag, sailval) = rml32(lin_addr, r_x) : (option(string), int);
    (flag, n32_to_i32(get_slice_int(32, sailval, 0)))
}

val wml32 : (sbits(48), bits(32)) -> option(string)

function wml32 (lin_addr, sailval) = {
    if true then {
        let three__plus_lin_addr = the_sbits(49, 3 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, three__plus_lin_addr)) then {
            wb(4, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml32")
        }
    } else {
        Some("wml32")
    }
}

val wiml32 : (sbits(48), sbits(32)) -> option(string)

function wiml32 (lin_addr, sailval) = {
    wml32(lin_addr, n32(sailval))
}

val rml48 : (sbits(48), string) -> (option(string), int)

function rml48 (lin_addr, r_x) = {
    if true then {
        let five__plus_lin_addr = the_sbits(49, 5 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, five__plus_lin_addr)) then {
            rb(6, signed(lin_addr), r_x)
        } else {
            (Some("rml48"), 0 : {|0|})
        }
    } else {
        (Some("rml48"), 0 : {|0|})
    }
}

val wml48 : (sbits(48), bits(48)) -> option(string)

function wml48 (lin_addr, sailval) = {
    if true then {
        let five__plus_lin_addr = the_sbits(49, 5 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, five__plus_lin_addr)) then {
            wb(6, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml48")
        }
    } else {
        Some("wml48")
    }
}

val rml64 : (sbits(48), string) -> (option(string), int)

function rml64 (lin_addr, r_x) = {
    if true then {
        let seven__plus_lin_addr = the_sbits(49, 7 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, seven__plus_lin_addr)) then {
            rb(8, signed(lin_addr), r_x)
        } else {
            (Some("rml64"), 0 : {|0|})
        }
    } else {
        (Some("rml64"), 0 : {|0|})
    }
}

val riml64 : (sbits(48), string) -> (option(string), sbits(64))

function riml64 (lin_addr, r_x) = {
    let (flag, sailval) = rml64(lin_addr, r_x) : (option(string), int);
    (flag, n64_to_i64(get_slice_int(64, sailval, 0)))
}

val wml64 : (sbits(48), bits(64)) -> option(string)

function wml64 (lin_addr, sailval) = {
    if true then {
        let seven__plus_lin_addr = the_sbits(50, 7 + signed(lin_addr)) : sbits(50);
        if canonical_address_p(the_sbits(48, seven__plus_lin_addr)) then {
            wb(8, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml64")
        }
    } else {
        Some("wml64")
    }
}

val wiml64 : (sbits(48), sbits(64)) -> option(string)

function wiml64 (lin_addr, sailval) = {
    wml64(lin_addr, n64(sailval))
}

val rml80 : (sbits(48), string) -> (option(string), int)

function rml80 (lin_addr, r_x) = {
    if true then {
        let nine__plus_lin_addr = the_sbits(49, 9 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, nine__plus_lin_addr)) then {
            rb(10, signed(lin_addr), r_x)
        } else {
            (Some("rml80"), 0 : {|0|})
        }
    } else {
        (Some("rml80"), 0 : {|0|})
    }
}

val wml80 : (sbits(48), bits(80)) -> option(string)

function wml80 (lin_addr, sailval) = {
    if true then {
        let nine__plus_lin_addr = the_sbits(49, 9 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, nine__plus_lin_addr)) then {
            wb(10, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml80")
        }
    } else {
        Some("wml80")
    }
}

val rml128 : (sbits(48), string) -> (option(string), int)

function rml128 (lin_addr, r_x) = {
    if true then {
        let one_5_plus_lin_addr = the_sbits(49, 15 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, one_5_plus_lin_addr)) then {
            rb(16, signed(lin_addr), r_x)
        } else {
            (Some("rml128"), 0 : {|0|})
        }
    } else {
        (Some("rml128"), 0 : {|0|})
    }
}

val wml128 : (sbits(48), bits(128)) -> option(string)

function wml128 (lin_addr, sailval) = {
    if true then {
        let one_5_plus_lin_addr = the_sbits(49, 15 + signed(lin_addr)) : sbits(49);
        if canonical_address_p(the_sbits(48, one_5_plus_lin_addr)) then {
            wb(16, signed(lin_addr), ":W", unsigned(sailval))
        } else {
            Some("wml128")
        }
    } else {
        Some("wml128")
    }
}

val rml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), string) -> (option(string), int)

function rml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => rml08(addr, r_x),
      2 => rml16(addr, r_x),
      4 => rml32(addr, r_x),
      6 => rml48(addr, r_x),
      8 => rml64(addr, r_x),
      10 => rml80(addr, r_x),
      16 => rml128(addr, r_x),
      _ => if canonical_address_p(get_slice_int(48, -1 + (nbytes + signed(addr)), 0)) then {
          rb(nbytes, signed(addr), r_x)
      } else {
          (Some("rml-size"), 0 : {|0|})
      }
    }) : (option(string), int)
}

val riml_size : ({|1, 2, 4, 8|}, sbits(48), string) -> (option(string), sbits(65))

function riml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => let (elem0, elem1) = riml08(addr, r_x) : (option(string), sbits(8)) in (elem0, the_sbits(65, elem1)),
      2 => let (elem0, elem1) = riml16(addr, r_x) : (option(string), sbits(16)) in (elem0, the_sbits(65, elem1)),
      4 => let (elem0, elem1) = riml32(addr, r_x) : (option(string), sbits(32)) in (elem0, the_sbits(65, elem1)),
      8 => let (elem0, elem1) = riml64(addr, r_x) : (option(string), sbits(64)) in (elem0, the_sbits(65, elem1)),
      _ => (Some("riml-size"), get_slice_int(65, nbytes, 0))
    }) : (option(string), sbits(65))
}

val wml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), nat) -> option(string)

function wml_size (nbytes, addr, sailval) = {
    (match nbytes {
      1 => wml08(addr, get_slice_int(8, sailval, 0)),
      2 => wml16(addr, get_slice_int(16, sailval, 0)),
      4 => wml32(addr, get_slice_int(32, sailval, 0)),
      6 => wml48(addr, get_slice_int(48, sailval, 0)),
      8 => wml64(addr, get_slice_int(64, sailval, 0)),
      10 => wml80(addr, get_slice_int(80, sailval, 0)),
      16 => wml128(addr, get_slice_int(128, sailval, 0)),
      _ => if canonical_address_p(get_slice_int(48, -1 + (nbytes + signed(addr)), 0)) then {
          wb(nbytes, signed(addr), ":W", sailval)
      } else {
          Some("wml-size")
      }
    }) : option(string)
}

val wiml_size : ({|1, 2, 4, 8|}, sbits(48), int) -> option(string)

function wiml_size (nbytes, addr, sailval) = {
    (match nbytes {
      1 => wiml08(addr, get_slice_int(8, sailval, 0)),
      2 => wiml16(addr, get_slice_int(16, sailval, 0)),
      4 => wiml32(addr, get_slice_int(32, sailval, 0)),
      8 => wiml64(addr, get_slice_int(64, sailval, 0)),
      _ => Some("wiml-size")
    }) : option(string)
}
