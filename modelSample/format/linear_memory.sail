$include "./paging.sail"

val rml08 : (sbits(48), string) -> (option(string), bits(8))

function rml08 (lin_addr, r_x) = {
    rb(1, lin_addr, r_x)
}

val riml08 : (sbits(48), string) -> (option(string), sbits(8))

function riml08 (lin_addr, r_x) = {
    let (flag, sailval) : (option(string), bits(8)) = rml08(lin_addr, r_x);
    (flag, sailval)
}

val wml08 : (sbits(48), bits(8)) -> option(string)

function wml08 (lin_addr, sailval) = {
    wb(1, lin_addr, ":W", sailval)
}

val wiml08 : (sbits(48), sbits(8)) -> option(string)

function wiml08 (lin_addr, sailval) = {
    wml08(lin_addr, sailval)
}

val rml16 : (sbits(48), string) -> (option(string), bits(16))

function rml16 (lin_addr, r_x) = {
    let one__plus_lin_addr : sbits(49) = bits_of_int(1 + signed(lin_addr), 49);
    if canonical_address_p(signed(one__plus_lin_addr)) then {
        rb(2, lin_addr, r_x)
    } else {
        (Some("rml16"), 0x0000)
    }
}

val riml16 : (sbits(48), string) -> (option(string), sbits(16))

function riml16 (lin_addr, r_x) = {
    let (flag, sailval) : (option(string), bits(16)) = rml16(lin_addr, r_x);
    (flag, sailval)
}

val wml16 : (sbits(48), bits(16)) -> option(string)

function wml16 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one__plus_lin_addr : sbits(49) = bits_of_int(1 + signed(lin_addr), 49);
    if canonical_address_p(signed(one__plus_lin_addr)) then {
        wb(2, lin_addr, ":W", sailval)
    } else {
        Some("wml16")
    }
}

val wiml16 : (sbits(48), sbits(16)) -> option(string)

function wiml16 (lin_addr, sailval) = {
    wml16(lin_addr, sailval)
}

val rml32 : (sbits(48), string) -> (option(string), bits(32))

function rml32 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let three__plus_lin_addr : sbits(49) = bits_of_int(3 + signed(lin_addr), 49);
    if canonical_address_p(signed(three__plus_lin_addr)) then {
        rb(4, lin_addr, r_x)
    } else {
        (Some("rml32"), 0x00000000)
    }
}

val riml32 : (sbits(48), string) -> (option(string), sbits(32))

function riml32 (lin_addr, r_x) = {
    let (flag, sailval) : (option(string), bits(32)) = rml32(lin_addr, r_x);
    (flag, sailval)
}

val wml32 : (sbits(48), bits(32)) -> option(string)

function wml32 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let three__plus_lin_addr : sbits(49) = bits_of_int(3 + signed(lin_addr), 49);
    if canonical_address_p(signed(three__plus_lin_addr)) then {
        wb(4, lin_addr, ":W", sailval)
    } else {
        Some("wml32")
    }
}

val wiml32 : (sbits(48), sbits(32)) -> option(string)

function wiml32 (lin_addr, sailval) = {
    wml32(lin_addr, sailval)
}

val rml48 : (sbits(48), string) -> (option(string), bits(48))

function rml48 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let five__plus_lin_addr : sbits(49) = bits_of_int(5 + signed(lin_addr), 49);
    if canonical_address_p(signed(five__plus_lin_addr)) then {
        rb(6, lin_addr, r_x)
    } else {
        (Some("rml48"), 0x000000000000)
    }
}

val wml48 : (sbits(48), bits(48)) -> option(string)

function wml48 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let five__plus_lin_addr : sbits(49) = bits_of_int(5 + signed(lin_addr), 49);
    if canonical_address_p(signed(five__plus_lin_addr)) then {
        wb(6, lin_addr, ":W", sailval)
    } else {
        Some("wml48")
    }
}

val rml64 : (sbits(48), string) -> (option(string), bits(64))

function rml64 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let seven__plus_lin_addr : sbits(49) = bits_of_int(7 + signed(lin_addr), 49);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        rb(8, lin_addr, r_x)
    } else {
        (Some("rml64"), 0x0000000000000000)
    }
}

val riml64 : (sbits(48), string) -> (option(string), sbits(64))

function riml64 (lin_addr, r_x) = {
    let (flag, sailval) : (option(string), bits(64)) = rml64(lin_addr, r_x);
    (flag, sailval)
}

val wml64 : (sbits(48), bits(64)) -> option(string)

function wml64 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let seven__plus_lin_addr : sbits(50) = bits_of_int(7 + signed(lin_addr), 50);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        wb(8, lin_addr, ":W", sailval)
    } else {
        Some("wml64")
    }
}

val wiml64 : (sbits(48), sbits(64)) -> option(string)

function wiml64 (lin_addr, sailval) = {
    wml64(lin_addr, sailval)
}

val rml80 : (sbits(48), string) -> (option(string), bits(80))

function rml80 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let nine__plus_lin_addr : sbits(49) = bits_of_int(9 + signed(lin_addr), 49);
    if canonical_address_p(signed(nine__plus_lin_addr)) then {
        rb(10, lin_addr, r_x)
    } else {
        (Some("rml80"), 0x00000000000000000000)
    }
}

val wml80 : (sbits(48), bits(80)) -> option(string)

function wml80 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let nine__plus_lin_addr : sbits(49) = bits_of_int(9 + signed(lin_addr), 49);
    if canonical_address_p(signed(nine__plus_lin_addr)) then {
        wb(10, lin_addr, ":W", sailval)
    } else {
        Some("wml80")
    }
}

val rml128 : (sbits(48), string) -> (option(string), bits(128))

function rml128 (lin_addr, r_x) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one_5_plus_lin_addr : sbits(49) = bits_of_int(15 + signed(lin_addr), 49);
    if canonical_address_p(signed(one_5_plus_lin_addr)) then {
        if app_view then {
            rb(16, lin_addr, r_x)
        } else {
            let (flag, p_addr0) : (option(string), bits(52)) = la_to_pa(lin_addr, r_x);
            if is_some(flag) then {
                (flag, 0x00000000000000000000000000000000)
            } else {
                let one__plus_lin_addr : sbits(49) = bits_of_int(1 + signed(lin_addr), 49);
                let (flag, p_addr1) : (option(string), bits(52)) = la_to_pa(truncate(one__plus_lin_addr, 48), r_x);
                if is_some(flag) then {
                    (flag, 0x00000000000000000000000000000000)
                } else {
                    let _2_plus_lin_addr : sbits(50) = bits_of_int(2 + signed(lin_addr), 50);
                    let (flag, p_addr2) : (option(string), bits(52)) = la_to_pa(truncate(_2_plus_lin_addr, 48), r_x);
                    if is_some(flag) then {
                        (flag, 0x00000000000000000000000000000000)
                    } else {
                        let three__plus_lin_addr : sbits(51) = bits_of_int(3 + signed(lin_addr), 51);
                        let (flag, p_addr3) : (option(string), bits(52)) = la_to_pa(truncate(three__plus_lin_addr, 48), r_x);
                        if is_some(flag) then {
                            (flag, 0x00000000000000000000000000000000)
                        } else {
                            let _4_plus_lin_addr : sbits(52) = bits_of_int(4 + signed(lin_addr), 52);
                            let (flag, p_addr4) : (option(string), bits(52)) = la_to_pa(truncate(_4_plus_lin_addr, 48), r_x);
                            if is_some(flag) then {
                                (flag, 0x00000000000000000000000000000000)
                            } else {
                                let five__plus_lin_addr : sbits(53) = bits_of_int(5 + signed(lin_addr), 53);
                                let (flag, p_addr5) : (option(string), bits(52)) = la_to_pa(truncate(five__plus_lin_addr, 48), r_x);
                                if is_some(flag) then {
                                    (flag, 0x00000000000000000000000000000000)
                                } else {
                                    let _6_plus_lin_addr : sbits(54) = bits_of_int(6 + signed(lin_addr), 54);
                                    let (flag, p_addr6) : (option(string), bits(52)) = la_to_pa(truncate(_6_plus_lin_addr, 48), r_x);
                                    if is_some(flag) then {
                                        (flag, 0x00000000000000000000000000000000)
                                    } else {
                                        let seven__plus_lin_addr : sbits(55) = bits_of_int(7 + signed(lin_addr), 55);
                                        let (flag, p_addr7) : (option(string), bits(52)) = la_to_pa(truncate(seven__plus_lin_addr, 48), r_x);
                                        if is_some(flag) then {
                                            (flag, 0x00000000000000000000000000000000)
                                        } else {
                                            let _8_plus_lin_addr : sbits(56) = bits_of_int(8 + signed(lin_addr), 56);
                                            let (flag, p_addr8) : (option(string), bits(52)) = la_to_pa(truncate(_8_plus_lin_addr, 48), r_x);
                                            if is_some(flag) then {
                                                (flag, 0x00000000000000000000000000000000)
                                            } else {
                                                let nine__plus_lin_addr : sbits(57) = bits_of_int(9 + signed(lin_addr), 57);
                                                let (flag, p_addr9) : (option(string), bits(52)) = la_to_pa(truncate(nine__plus_lin_addr, 48), r_x);
                                                if is_some(flag) then {
                                                    (flag, 0x00000000000000000000000000000000)
                                                } else {
                                                    let one_0_plus_lin_addr : sbits(58) = bits_of_int(10 + signed(lin_addr), 58);
                                                    let (flag, p_addr10) : (option(string), bits(52)) = la_to_pa(truncate(one_0_plus_lin_addr, 48), r_x);
                                                    if is_some(flag) then {
                                                        (flag, 0x00000000000000000000000000000000)
                                                    } else {
                                                        let one_1_plus_lin_addr : sbits(59) = bits_of_int(11 + signed(lin_addr), 59);
                                                        let (flag, p_addr11) : (option(string), bits(52)) = la_to_pa(truncate(one_1_plus_lin_addr, 48), r_x);
                                                        if is_some(flag) then {
                                                            (flag, 0x00000000000000000000000000000000)
                                                        } else {
                                                            let one_2_plus_lin_addr : sbits(60) = bits_of_int(12 + signed(lin_addr), 60);
                                                            let (flag, p_addr12) : (option(string), bits(52)) = la_to_pa(truncate(one_2_plus_lin_addr, 48), r_x);
                                                            if is_some(flag) then {
                                                                (flag, 0x00000000000000000000000000000000)
                                                            } else {
                                                                let one_3_plus_lin_addr : sbits(61) = bits_of_int(13 + signed(lin_addr), 61);
                                                                let (flag, p_addr13) : (option(string), bits(52)) = la_to_pa(truncate(one_3_plus_lin_addr, 48), r_x);
                                                                if is_some(flag) then {
                                                                    (flag, 0x00000000000000000000000000000000)
                                                                } else {
                                                                    let one_4_plus_lin_addr : sbits(62) = bits_of_int(14 + signed(lin_addr), 62);
                                                                    let (flag, p_addr14) : (option(string), bits(52)) = la_to_pa(truncate(one_4_plus_lin_addr, 48), r_x);
                                                                    if is_some(flag) then {
                                                                        (flag, 0x00000000000000000000000000000000)
                                                                    } else {
                                                                        let one_5_plus_lin_addr : sbits(63) = bits_of_int(15 + signed(lin_addr), 63);
                                                                        let (flag, p_addr15) : (option(string), bits(52)) = la_to_pa(truncate(one_5_plus_lin_addr, 48), r_x);
                                                                        if is_some(flag) then {
                                                                            (flag, 0x00000000000000000000000000000000)
                                                                        } else {
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
                                                                            (None() : option(string), oword)
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        (Some("rml128"), 0x00000000000000000000000000000000)
    }
}

val wml128 : (sbits(48), bits(128)) -> option(string)

function wml128 (lin_addr, sailval) = {
    assert(canonical_address_p(signed(lin_addr)));
    let one_5_plus_lin_addr : sbits(49) = bits_of_int(15 + signed(lin_addr), 49);
    if canonical_address_p(signed(one_5_plus_lin_addr)) then {
        if app_view then {
            wb(16, lin_addr, ":W", sailval)
        } else {
            let (flag, p_addr0) : (option(string), bits(52)) = la_to_pa(lin_addr, ":W");
            if is_some(flag) then {
                flag
            } else {
                let one__plus_lin_addr : sbits(49) = bits_of_int(1 + signed(lin_addr), 49);
                let (flag, p_addr1) : (option(string), bits(52)) = la_to_pa(truncate(one__plus_lin_addr, 48), ":W");
                if is_some(flag) then {
                    flag
                } else {
                    let _2_plus_lin_addr : sbits(50) = bits_of_int(2 + signed(lin_addr), 50);
                    let (flag, p_addr2) : (option(string), bits(52)) = la_to_pa(truncate(_2_plus_lin_addr, 48), ":W");
                    if is_some(flag) then {
                        flag
                    } else {
                        let three__plus_lin_addr : sbits(51) = bits_of_int(3 + signed(lin_addr), 51);
                        let (flag, p_addr3) : (option(string), bits(52)) = la_to_pa(truncate(three__plus_lin_addr, 48), ":W");
                        if is_some(flag) then {
                            flag
                        } else {
                            let _4_plus_lin_addr : sbits(52) = bits_of_int(4 + signed(lin_addr), 52);
                            let (flag, p_addr4) : (option(string), bits(52)) = la_to_pa(truncate(_4_plus_lin_addr, 48), ":W");
                            if is_some(flag) then {
                                flag
                            } else {
                                let five__plus_lin_addr : sbits(53) = bits_of_int(5 + signed(lin_addr), 53);
                                let (flag, p_addr5) : (option(string), bits(52)) = la_to_pa(truncate(five__plus_lin_addr, 48), ":W");
                                if is_some(flag) then {
                                    flag
                                } else {
                                    let _6_plus_lin_addr : sbits(54) = bits_of_int(6 + signed(lin_addr), 54);
                                    let (flag, p_addr6) : (option(string), bits(52)) = la_to_pa(truncate(_6_plus_lin_addr, 48), ":W");
                                    if is_some(flag) then {
                                        flag
                                    } else {
                                        let seven__plus_lin_addr : sbits(55) = bits_of_int(7 + signed(lin_addr), 55);
                                        let (flag, p_addr7) : (option(string), bits(52)) = la_to_pa(truncate(seven__plus_lin_addr, 48), ":W");
                                        if is_some(flag) then {
                                            flag
                                        } else {
                                            let _8_plus_lin_addr : sbits(56) = bits_of_int(8 + signed(lin_addr), 56);
                                            let (flag, p_addr8) : (option(string), bits(52)) = la_to_pa(truncate(_8_plus_lin_addr, 48), ":W");
                                            if is_some(flag) then {
                                                flag
                                            } else {
                                                let nine__plus_lin_addr : sbits(57) = bits_of_int(9 + signed(lin_addr), 57);
                                                let (flag, p_addr9) : (option(string), bits(52)) = la_to_pa(truncate(nine__plus_lin_addr, 48), ":W");
                                                if is_some(flag) then {
                                                    flag
                                                } else {
                                                    let one_0_plus_lin_addr : sbits(58) = bits_of_int(10 + signed(lin_addr), 58);
                                                    let (flag, p_addr10) : (option(string), bits(52)) = la_to_pa(truncate(one_0_plus_lin_addr, 48), ":W");
                                                    if is_some(flag) then {
                                                        flag
                                                    } else {
                                                        let one_1_plus_lin_addr : sbits(59) = bits_of_int(11 + signed(lin_addr), 59);
                                                        let (flag, p_addr11) : (option(string), bits(52)) = la_to_pa(truncate(one_1_plus_lin_addr, 48), ":W");
                                                        if is_some(flag) then {
                                                            flag
                                                        } else {
                                                            let one_2_plus_lin_addr : sbits(60) = bits_of_int(12 + signed(lin_addr), 60);
                                                            let (flag, p_addr12) : (option(string), bits(52)) = la_to_pa(truncate(one_2_plus_lin_addr, 48), ":W");
                                                            if is_some(flag) then {
                                                                flag
                                                            } else {
                                                                let one_3_plus_lin_addr : sbits(61) = bits_of_int(13 + signed(lin_addr), 61);
                                                                let (flag, p_addr13) : (option(string), bits(52)) = la_to_pa(truncate(one_3_plus_lin_addr, 48), ":W");
                                                                if is_some(flag) then {
                                                                    flag
                                                                } else {
                                                                    let one_4_plus_lin_addr : sbits(62) = bits_of_int(14 + signed(lin_addr), 62);
                                                                    let (flag, p_addr14) : (option(string), bits(52)) = la_to_pa(truncate(one_4_plus_lin_addr, 48), ":W");
                                                                    if is_some(flag) then {
                                                                        flag
                                                                    } else {
                                                                        let one_5_plus_lin_addr : sbits(63) = bits_of_int(15 + signed(lin_addr), 63);
                                                                        let (flag, p_addr15) : (option(string), bits(52)) = la_to_pa(truncate(one_5_plus_lin_addr, 48), ":W");
                                                                        if is_some(flag) then {
                                                                            flag
                                                                        } else {
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
                                                                            bang_memi(sail_zero_extend(p_addr15, 64), byte15);
                                                                            None() : option(string)
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        Some("wml128")
    }
}

val rml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), string) -> (option(string), bits(128))

function rml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => {
          let (elem0, elem1) : (option(string), bits(8)) = rml08(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      2 => {
          let (elem0, elem1) : (option(string), bits(16)) = rml16(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      4 => {
          let (elem0, elem1) : (option(string), bits(32)) = rml32(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      6 => {
          let (elem0, elem1) : (option(string), bits(48)) = rml48(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      8 => {
          let (elem0, elem1) : (option(string), bits(64)) = rml64(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      10 => {
          let (elem0, elem1) : (option(string), bits(80)) = rml80(addr, r_x);
          (elem0, sail_zero_extend(elem1, 128))
      },
      16 => rml128(addr, r_x),
      _ => {
          if canonical_address_p(-1 + (nbytes + signed(addr))) then {
              let (elem0, elem1) = rb(nbytes, addr, r_x);
              (elem0, sail_mask(128, elem1))
          } else {
              (Some("rml-size"), 0x00000000000000000000000000000000)
          }
      }
    }) : (option(string), bits(128))
}

val riml_size : ({|1, 2, 4, 8|}, sbits(48), string) -> (option(string), sbits(64))

function riml_size (nbytes, addr, r_x) = {
    (match nbytes {
      1 => {
          let (elem0, elem1) : (option(string), sbits(8)) = riml08(addr, r_x);
          (elem0, sail_sign_extend(elem1, 64))
      },
      2 => {
          let (elem0, elem1) : (option(string), sbits(16)) = riml16(addr, r_x);
          (elem0, sail_sign_extend(elem1, 64))
      },
      4 => {
          let (elem0, elem1) : (option(string), sbits(32)) = riml32(addr, r_x);
          (elem0, sail_sign_extend(elem1, 64))
      },
      8 => riml64(addr, r_x),
      _ => (Some("riml-size"), bits_of_int(nbytes, 64))
    }) : (option(string), sbits(64))
}

val wml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), nat) -> option(string)

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
              Some("wml-size")
          }
      }
    }) : option(string)
}

val wiml_size : ({|1, 2, 4, 8|}, sbits(48), int) -> option(string)

function wiml_size (nbytes, addr, sailval) = {
    (match nbytes {
      1 => wiml08(addr, bits_of_int(sailval, 8)),
      2 => wiml16(addr, bits_of_int(sailval, 16)),
      4 => wiml32(addr, bits_of_int(sailval, 32)),
      8 => wiml64(addr, bits_of_int(sailval, 64)),
      _ => Some("wiml-size")
    }) : option(string)
}

val write_canonical_address_to_memory_user_exec : (sbits(48), sbits(48)) -> option(string)

function write_canonical_address_to_memory_user_exec (lin_addr, canonical_address) = {
    assert(app_view & canonical_address_p(7 + signed(lin_addr)));
    let canonical_address_low_nat : bits(32) = truncate(canonical_address, 32);
    let canonical_address_high_int : sbits(32) = sail_sign_extend(sail_zero_extend(canonical_address[47 .. 32], 16), 32);
    let flg0 : option(string) = wml32(lin_addr, canonical_address_low_nat);
    let next_addr : sbits(49) = bits_of_int(4 + signed(lin_addr), 49);
    if not_bool(canonical_address_p(signed(next_addr))) then {
        Some("wml64-canonical-address-user-view")
    } else {
        let flg1 : option(string) = wiml32(truncate(next_addr, 48), canonical_address_high_int);
        if is_some(flg0) | is_some(flg1) then {
            Some("wml64-canonical-address-user-view")
        } else {
            None() : option(string)
        }
    }
}

val write_canonical_address_to_memory : (sbits(48), sbits(48)) -> option(string)

function write_canonical_address_to_memory (lin_addr, canonical_address) = {
    let seven__plus_lin_addr : sbits(50) = bits_of_int(7 + signed(lin_addr), 50);
    if canonical_address_p(signed(seven__plus_lin_addr)) then {
        if app_view then {
            wb(8, lin_addr, ":W", bits_of_int(loghead(64, signed(canonical_address)), 64))
        } else {
            let canonical_address_unsigned_val : int = loghead(64, signed(canonical_address));
            wml64(lin_addr, bits_of_int(canonical_address_unsigned_val, 64))
        }
    } else {
        Some("write-canonical-address-to-memory-error")
    }
}
