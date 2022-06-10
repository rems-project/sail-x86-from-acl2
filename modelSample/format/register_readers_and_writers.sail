
val reg_index : (bits(3), bits(8), bits(2)) -> bits(4)

function reg_index (reg, rex_byte, index) = {
    if logbitp(unsigned(index), rex_byte) then {
        logior(get_slice_int(4, 8, 0), the_bits(4, n03(reg)))
    } else {
        the_bits(4, n03(reg))
    }
}

val rr08 : (bits(4), bits(8)) -> bits(8)

function rr08 (reg, rex) = {
    if not_bool(rex == get_slice_int(8, 0, 0)) | unsigned(reg) < 4 then {
        let qword = rgfi(unsigned(reg)) : bits(64);
        n08(the_bits(8, qword))
    } else {
        let qword = rgfi(unsigned(the_bits(4, unsigned(reg) - 4))) : bits(64);
        slice(qword, 8, 8)
    }
}

val rr16 : bits(4) -> bits(16)

function rr16 reg = {
    n16(the_bits(16, rgfi(unsigned(reg))))
}

val rr32 : bits(4) -> bits(32)

function rr32 reg = {
    n32(the_bits(32, rgfi(unsigned(reg))))
}

val rr64 : bits(4) -> bits(64)

function rr64 reg = {
    n64(rgfi(unsigned(reg)))
}

val wr08 : (bits(4), bits(8), bits(8)) -> unit

function wr08 (reg, rex, byte) = {
    if not_bool(rex == get_slice_int(8, 0, 0)) | unsigned(reg) < 4 then {
        let qword = rgfi(unsigned(reg)) : bits(64);
        write_rgfi(unsigned(reg), n64_to_i64(changeSlice(slice(qword, 0, 64), 0, 8, byte)))
    } else {
        let index = the_bits(4, unsigned(reg) - 4) : bits(4);
        let qword = rgfi(unsigned(index)) : bits(64);
        write_rgfi(unsigned(index), n64_to_i64(changeSlice(slice(qword, 0, 64), 8, 8, byte)))
    }
}

val wr16 : (bits(4), bits(16)) -> unit

function wr16 (reg, sailval) = {
    let qword = rgfi(unsigned(reg)) : bits(64);
    write_rgfi(unsigned(reg), n64_to_i64(changeSlice(slice(qword, 0, 64), 0, 16, sailval)))
}

val wr32 : (bits(4), bits(32)) -> unit

function wr32 (reg, sailval) = {
    write_rgfi(unsigned(reg), the_bits(64, n32(sailval)))
}

val wr64 : (bits(4), bits(64)) -> unit

function wr64 (reg, sailval) = {
    write_rgfi(unsigned(reg), n64_to_i64(sailval))
}

val rgfi_size : (bits(4), bits(4), bits(8)) -> bits(64)

function rgfi_size (nbytes, index, rex) = {
    (match nbytes {
      1 => the_bits(64, rr08(index, rex)),
      2 => the_bits(64, rr16(index)),
      4 => the_bits(64, rr32(index)),
      8 => rr64(index),
      _ => get_slice_int(64, 0, 0)
    }) : bits(64)
}

val write_rgfi_size : (bits(4), bits(4), int, bits(8)) -> unit

function write_rgfi_size (nbytes, index, sailval, rex) = {
    (match nbytes {
      1 => wr08(index, rex, get_slice_int(8, sailval, 0)),
      2 => wr16(index, get_slice_int(16, sailval, 0)),
      4 => wr32(index, get_slice_int(32, sailval, 0)),
      8 => wr64(index, get_slice_int(64, sailval, 0)),
      _ => ()
    }) : unit
}

val rz32 : bits(5) -> bits(32)

function rz32 reg = {
    n32(the_bits(32, zmms[unsigned(reg)]))
}

val rz64 : bits(5) -> bits(64)

function rz64 reg = {
    n64(the_bits(64, zmms[unsigned(reg)]))
}

val rz128 : bits(5) -> bits(128)

function rz128 reg = {
    n128(the_bits(128, zmms[unsigned(reg)]))
}

struct struct_wz32 = {regtype : int}

val wz32 : (bits(5), bits(32), struct_wz32) -> unit

function wz32 (reg, sailval, keywords) = {
    let data = zmms[unsigned(reg)] : bits(512);
    let regtype = the_bits(3, keywords.regtype) : bits(3);
    let data = (match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    }) : int : int;
    let sailval = n32(sailval) : bits(32);
    zmms[unsigned(reg)] = get_slice_int(512, changeSlice(data, 0, 32, unsigned(sailval)), 0)
}

struct struct_wz64 = {regtype : int}

val wz64 : (bits(5), bits(64), struct_wz64) -> unit

function wz64 (reg, sailval, keywords) = {
    let data = zmms[unsigned(reg)] : bits(512);
    let regtype = the_bits(3, keywords.regtype) : bits(3);
    let data = (match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    }) : int : int;
    let sailval = n64(sailval) : bits(64);
    zmms[unsigned(reg)] = get_slice_int(512, changeSlice(data, 0, 64, unsigned(sailval)), 0)
}

struct struct_wz128 = {regtype : int}

val wz128 : (bits(5), bits(128), struct_wz128) -> unit

function wz128 (reg, sailval, keywords) = {
    let data = zmms[unsigned(reg)] : bits(512);
    let regtype = the_bits(3, keywords.regtype) : bits(3);
    let data = (match regtype {
      2 => loghead(128, unsigned(data)),
      3 => loghead(256, unsigned(data)),
      _ => unsigned(data)
    }) : int : int;
    let sailval = n128(sailval) : bits(128);
    zmms[unsigned(reg)] = get_slice_int(512, changeSlice(data, 0, 128, unsigned(sailval)), 0)
}

val rx32 : bits(4) -> bits(32)

function rx32 reg = {
    rz32(the_bits(5, reg))
}

val rx64 : bits(4) -> bits(64)

function rx64 reg = {
    rz64(the_bits(5, reg))
}

val rx128 : bits(4) -> bits(128)

function rx128 reg = {
    rz128(the_bits(5, reg))
}

val wx32 : (bits(4), bits(32)) -> unit

function wx32 (reg, sailval) = {
    wz32(the_bits(5, reg), sailval, struct { regtype = 1 })
}

val wx64 : (bits(4), bits(64)) -> unit

function wx64 (reg, sailval) = {
    wz64(the_bits(5, reg), sailval, struct { regtype = 1 })
}

val wx128 : (bits(4), bits(128)) -> unit

function wx128 (reg, sailval) = {
    wz128(the_bits(5, reg), sailval, struct { regtype = 1 })
}

val xmmi_size : (bits(5), bits(4)) -> bits(128)

function xmmi_size (nbytes, index) = {
    (match nbytes {
      4 => the_bits(128, rx32(index)),
      8 => the_bits(128, rx64(index)),
      16 => rx128(index),
      _ => get_slice_int(128, 0, 0)
    }) : bits(128)
}

val write_xmmi_size : (bits(5), bits(4), int) -> unit

function write_xmmi_size (nbytes, index, sailval) = {
    (match nbytes {
      4 => wx32(index, get_slice_int(32, sailval, 0)),
      8 => wx64(index, get_slice_int(64, sailval, 0)),
      16 => wx128(index, get_slice_int(128, sailval, 0)),
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
    let sailval = undef_flg_logic() : nat;
    n01(get_slice_int(1, sailval, 0))
}

val write_user_rflags : (bits(32), bits(32)) -> unit

function write_user_rflags (user_flags_vector, undefined_mask) = {
    let user_flags_vector = n32(user_flags_vector) : bits(32);
    let undefined_mask = n32(undefined_mask) : bits(32);
    let input_rflags = n32(rflags) : bits(32);
    if the_bits(8, rflagsbits_get_cf(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_cf(rflagsbits_get_cf(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    };
    if the_bits(8, rflagsbits_get_pf(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_pf(rflagsbits_get_pf(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    };
    if the_bits(8, rflagsbits_get_af(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_af(rflagsbits_get_af(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    };
    if the_bits(8, rflagsbits_get_zf(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_zf(rflagsbits_get_zf(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    };
    if the_bits(8, rflagsbits_get_sf(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_sf(rflagsbits_get_sf(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    };
    if the_bits(8, rflagsbits_get_of(undefined_mask)) == get_slice_int(8, 1, 0) then {
        let rflags_var = rflags : bits(32);
        let sailval = undef_flg() : bits(1);
        let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : bits(32);
        rflags = new_rflags
    } else {
        let rflags_var = rflags : bits(32);
        let new_rflags = set_rflagsbits_get_of(rflagsbits_get_of(user_flags_vector), rflags_var) : bits(32);
        rflags = new_rflags
    }
}
