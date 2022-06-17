
val reg_index : (bits(3), bits(8), bits(2)) -> bits(4)

function reg_index (reg, rex_byte, index) = {
    if logbitp(unsigned(index), rex_byte) then {
        logior(0x8, sail_zero_extend(n03(reg), 4))
    } else {
        sail_zero_extend(n03(reg), 4)
    }
}

val rr08 : (bits(4), bits(8)) -> bits(8)

function rr08 (reg, rex) = {
    if not_bool(rex == 0x00) | unsigned(reg) < 4 then {
        let qword : bits(64) = rgfi(unsigned(reg));
        n08(truncate(qword, 8))
    } else {
        let qword : bits(64) = rgfi(unsigned(bits_of_int(unsigned(reg) - 4, 4)));
        slice(qword, 8, 8)
    }
}

val rr16 : bits(4) -> bits(16)

function rr16 reg = {
    n16(truncate(rgfi(unsigned(reg)), 16))
}

val rr32 : bits(4) -> bits(32)

function rr32 reg = {
    n32(truncate(rgfi(unsigned(reg)), 32))
}

val rr64 : bits(4) -> bits(64)

function rr64 reg = {
    n64(rgfi(unsigned(reg)))
}

val wr08 : (bits(4), bits(8), bits(8)) -> unit

function wr08 (reg, rex, byte) = {
    if not_bool(rex == 0x00) | unsigned(reg) < 4 then {
        let qword : bits(64) = rgfi(unsigned(reg));
        write_rgfi(unsigned(reg), n64_to_i64(changeSlice(slice(qword, 0, 64), 0, 8, byte)))
    } else {
        let index : bits(4) = bits_of_int(unsigned(reg) - 4, 4);
        let qword : bits(64) = rgfi(unsigned(index));
        write_rgfi(unsigned(index), n64_to_i64(changeSlice(slice(qword, 0, 64), 8, 8, byte)))
    }
}

val wr16 : (bits(4), bits(16)) -> unit

function wr16 (reg, sailval) = {
    let qword : bits(64) = rgfi(unsigned(reg));
    write_rgfi(unsigned(reg), n64_to_i64(changeSlice(slice(qword, 0, 64), 0, 16, sailval)))
}

val wr32 : (bits(4), bits(32)) -> unit

function wr32 (reg, sailval) = {
    write_rgfi(unsigned(reg), sail_zero_extend(n32(sailval), 64))
}

val wr64 : (bits(4), bits(64)) -> unit

function wr64 (reg, sailval) = {
    write_rgfi(unsigned(reg), n64_to_i64(sailval))
}

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

val write_rgfi_size : (bits(4), bits(4), int, bits(8)) -> unit

function write_rgfi_size (nbytes, index, sailval, rex) = {
    (match nbytes {
      1 => wr08(index, rex, bits_of_int(sailval, 8)),
      2 => wr16(index, bits_of_int(sailval, 16)),
      4 => wr32(index, bits_of_int(sailval, 32)),
      8 => wr64(index, bits_of_int(sailval, 64)),
      _ => ()
    }) : unit
}

val rz32 : bits(5) -> bits(32)

function rz32 reg = {
    n32(truncate(zmms[unsigned(reg)], 32))
}

val rz64 : bits(5) -> bits(64)

function rz64 reg = {
    n64(truncate(zmms[unsigned(reg)], 64))
}

val rz128 : bits(5) -> bits(128)

function rz128 reg = {
    n128(truncate(zmms[unsigned(reg)], 128))
}

struct struct_wz32 = {regtype : int}

val wz32 : (bits(5), bits(32), struct_wz32) -> unit

function wz32 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : int = match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    };
    let sailval : bits(32) = n32(sailval);
    zmms[unsigned(reg)] = bits_of_int(changeSlice(data, 0, 32, unsigned(sailval)), 512)
}

struct struct_wz64 = {regtype : int}

val wz64 : (bits(5), bits(64), struct_wz64) -> unit

function wz64 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : int = match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    };
    let sailval : bits(64) = n64(sailval);
    zmms[unsigned(reg)] = bits_of_int(changeSlice(data, 0, 64, unsigned(sailval)), 512)
}

struct struct_wz128 = {regtype : int}

val wz128 : (bits(5), bits(128), struct_wz128) -> unit

function wz128 (reg, sailval, keywords) = {
    let data : bits(512) = zmms[unsigned(reg)];
    let regtype : bits(3) = bits_of_int(keywords.regtype, 3);
    let data : int = match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    };
    let sailval : bits(128) = n128(sailval);
    zmms[unsigned(reg)] = bits_of_int(changeSlice(data, 0, 128, unsigned(sailval)), 512)
}

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

val wx32 : (bits(4), bits(32)) -> unit

function wx32 (reg, sailval) = {
    wz32(sail_zero_extend(reg, 5), sailval, struct { regtype = 1 })
}

val wx64 : (bits(4), bits(64)) -> unit

function wx64 (reg, sailval) = {
    wz64(sail_zero_extend(reg, 5), sailval, struct { regtype = 1 })
}

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

val write_xmmi_size : (bits(5), bits(4), int) -> unit

function write_xmmi_size (nbytes, index, sailval) = {
    (match nbytes {
      4 => wx32(index, bits_of_int(sailval, 32)),
      8 => wx64(index, bits_of_int(sailval, 64)),
      16 => wx128(index, bits_of_int(sailval, 128)),
      _ => ()
    }) : unit
}

val undef_read : unit -> nat

function undef_read () = {
    undef_read_logic()
}

val undef_flg_logic : unit -> nat

function undef_flg_logic () = {
    undef_read()
}

val undef_flg : unit -> bits(1)

function undef_flg () = {
    let sailval : nat = undef_flg_logic();
    n01(bits_of_int(sailval, 1))
}

val write_user_rflags : (bits(32), bits(32)) -> unit

function write_user_rflags (user_flags_vector, undefined_mask) = {
    let user_flags_vector : bits(32) = n32(user_flags_vector);
    let undefined_mask : bits(32) = n32(undefined_mask);
    let input_rflags : bits(32) = n32(rflags);
    if rflagsbits_get_cf(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_cf(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_cf(rflagsbits_get_cf(user_flags_vector), rflags_var);
        rflags = new_rflags
    };
    if rflagsbits_get_pf(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_pf(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_pf(rflagsbits_get_pf(user_flags_vector), rflags_var);
        rflags = new_rflags
    };
    if rflagsbits_get_af(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_af(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_af(rflagsbits_get_af(user_flags_vector), rflags_var);
        rflags = new_rflags
    };
    if rflagsbits_get_zf(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_zf(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_zf(rflagsbits_get_zf(user_flags_vector), rflags_var);
        rflags = new_rflags
    };
    if rflagsbits_get_sf(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_sf(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_sf(rflagsbits_get_sf(user_flags_vector), rflags_var);
        rflags = new_rflags
    };
    if rflagsbits_get_of(undefined_mask) == 0b1 then {
        let rflags_var : bits(32) = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : bits(32) = set_rflagsbits_get_of(sailval, rflags_var);
        rflags = new_rflags
    } else {
        let rflags_var : bits(32) = rflags;
        let new_rflags : bits(32) = set_rflagsbits_get_of(rflagsbits_get_of(user_flags_vector), rflags_var);
        rflags = new_rflags
    }
}
