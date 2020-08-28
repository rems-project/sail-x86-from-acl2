
val reg_index : (int, int, int) -> int effect {escape}

function reg_index (reg, rex_byte, index) = {
    if logbitp(index, rex_byte) then {
        binary_logior(8, n03(reg))
    } else {
        n03(reg)
    }
}

val rr08 : (int, int, int) -> int effect {escape, rreg}

function rr08 (reg, rex, x86) = {
    if not_bool(rex == 0) | reg < 4 then {
        let qword = rgfi(reg, 0) : int;
        n08(qword)
    } else {
        let qword = rgfi(reg - 4, 0) : int;
        unsigned(get_slice_int(8, qword, 8))
    }
}

val rr16 : (int, int) -> int effect {escape, rreg}

function rr16 (reg, x86) = {
    n16(rgfi(reg, 0))
}

val rr32 : (int, int) -> int effect {escape, rreg}

function rr32 (reg, x86) = {
    n32(rgfi(reg, 0))
}

val rr64 : (int, int) -> int effect {escape, rreg}

function rr64 (reg, x86) = {
    n64(rgfi(reg, 0))
}

val wr08 : (int, int, int, int) -> int effect {escape, rreg, wreg}

function wr08 (reg, rex, byte, x86) = {
    if not_bool(rex == 0) | reg < 4 then {
        let qword = rgfi(reg, 0) : int;
        write_rgfi(reg, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 0, 8, byte)), 0)
    } else {
        let index = (reg - 4) : int;
        let qword = rgfi(index, 0) : int;
        write_rgfi(index, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 8, 8, byte)), 0)
    }
}

val wr16 : (int, int, int) -> int effect {escape, rreg, wreg}

function wr16 (reg, sailval, x86) = {
    let qword = rgfi(reg, 0) : int;
    write_rgfi(reg, n64_to_i64(changeBits(unsigned(get_slice_int(64, qword, 0)), 0, 16, sailval)), 0)
}

val wr32 : (int, int, int) -> int effect {escape, wreg}

function wr32 (reg, sailval, x86) = {
    write_rgfi(reg, n32(sailval), 0)
}

val wr64 : (int, int, int) -> int effect {escape, wreg}

function wr64 (reg, sailval, x86) = {
    write_rgfi(reg, n64_to_i64(sailval), 0)
}

val rgfi_size : (int, int, int, int) -> int effect {escape, rreg}

function rgfi_size (nbytes, index, rex, x86) = {
    (match nbytes {
      1 => rr08(index, rex, 0),
      2 => rr16(index, 0),
      4 => rr32(index, 0),
      8 => rr64(index, 0),
      _ => 0
    }) : int
}

val write_rgfi_size : (int, int, int, int, int) -> int effect {escape, rreg, wreg}

function write_rgfi_size (nbytes, index, sailval, rex, x86) = {
    (match nbytes {
      1 => wr08(index, rex, sailval, 0),
      2 => wr16(index, sailval, 0),
      4 => wr32(index, sailval, 0),
      8 => wr64(index, sailval, 0),
      _ => 0
    }) : int
}

val rz32 : (int, int) -> int effect {escape, rreg}

function rz32 (reg, x86) = {
    n32(zmmi(reg, 0))
}

val rz64 : (int, int) -> int effect {escape, rreg}

function rz64 (reg, x86) = {
    n64(zmmi(reg, 0))
}

val rz128 : (int, int) -> int effect {escape, rreg}

function rz128 (reg, x86) = {
    n128(zmmi(reg, 0))
}

struct struct_wz32 = {regtype : int}

val wz32 : (int, int, int, struct_wz32) -> int effect {escape, rreg, wreg}

function wz32 (reg, sailval, x86, keywords) = {
    let data = zmmi(reg, 0) : int;
    let regtype = keywords.regtype : int;
    let data = (match regtype {
      2 => loghead(128, data),
      3 => loghead(256, data),
      _ => data
    }) : int : int;
    let sailval = n32(sailval) : int;
    write_zmmi(reg, changeBits(data, 0, 32, sailval), 0)
}

struct struct_wz64 = {regtype : int}

val wz64 : (int, int, int, struct_wz64) -> int effect {escape, rreg, wreg}

function wz64 (reg, sailval, x86, keywords) = {
    let data = zmmi(reg, 0) : int;
    let regtype = keywords.regtype : int;
    let data = (match regtype {
      2 => loghead(128, data),
      3 => loghead(256, data),
      _ => data
    }) : int : int;
    let sailval = n64(sailval) : int;
    write_zmmi(reg, changeBits(data, 0, 64, sailval), 0)
}

struct struct_wz128 = {regtype : int}

val wz128 : (int, int, int, struct_wz128) -> int effect {escape, rreg, wreg}

function wz128 (reg, sailval, x86, keywords) = {
    let data = zmmi(reg, 0) : int;
    let regtype = keywords.regtype : int;
    let data = (match regtype {
      2 => loghead(128, data),
      3 => loghead(256, data),
      _ => data
    }) : int : int;
    let sailval = n128(sailval) : int;
    write_zmmi(reg, changeBits(data, 0, 128, sailval), 0)
}

val rx32 : (int, int) -> int effect {escape, rreg}

function rx32 (reg, x86) = {
    rz32(reg, 0)
}

val rx64 : (int, int) -> int effect {escape, rreg}

function rx64 (reg, x86) = {
    rz64(reg, 0)
}

val rx128 : (int, int) -> int effect {escape, rreg}

function rx128 (reg, x86) = {
    rz128(reg, 0)
}

val wx32 : (int, int, int) -> int effect {escape, rreg, wreg}

function wx32 (reg, sailval, x86) = {
    wz32(reg, sailval, 0, struct { regtype = 1 })
}

val wx64 : (int, int, int) -> int effect {escape, rreg, wreg}

function wx64 (reg, sailval, x86) = {
    wz64(reg, sailval, 0, struct { regtype = 1 })
}

val wx128 : (int, int, int) -> int effect {escape, rreg, wreg}

function wx128 (reg, sailval, x86) = {
    wz128(reg, sailval, 0, struct { regtype = 1 })
}

val xmmi_size : (int, int, int) -> int effect {escape, rreg}

function xmmi_size (nbytes, index, x86) = {
    (match nbytes {
      4 => rx32(index, 0),
      8 => rx64(index, 0),
      16 => rx128(index, 0),
      _ => 0
    }) : int
}

val write_xmmi_size : (int, int, int, int) -> int effect {escape, rreg, wreg}

function write_xmmi_size (nbytes, index, sailval, x86) = {
    (match nbytes {
      4 => wx32(index, sailval, 0),
      8 => wx64(index, sailval, 0),
      16 => wx128(index, sailval, 0),
      _ => 0
    }) : int
}

val undef_read : int -> (int, int) effect {undef}

function undef_read x86 = {
    undef_read_logic(0)
}

val undef_flg_logic : int -> (int, int) effect {undef}

function undef_flg_logic x86 = {
    undef_read(0)
}

val undef_flg : int -> (int, int) effect {undef}

function undef_flg x86 = {
    let (sailval, x86) = undef_flg_logic(0) : (int, int);
    (n01(sailval), 0 : int)
}

val write_user_rflags : (int, int, int) -> int effect {escape, rreg, undef, wreg}

function write_user_rflags (user_flags_vector, undefined_mask, x86) = {
    let user_flags_vector = n32(user_flags_vector) : int;
    let undefined_mask = n32(undefined_mask) : int;
    let input_rflags = n32(r_rflags(0)) : int;
    let x86 = (if rflagsbits_get_cf(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_cf(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_cf(rflagsbits_get_cf(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    let x86 = (if rflagsbits_get_pf(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_pf(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_pf(rflagsbits_get_pf(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    let x86 = (if rflagsbits_get_af(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_af(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_af(rflagsbits_get_af(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    let x86 = (if rflagsbits_get_zf(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_zf(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_zf(rflagsbits_get_zf(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    let x86 = (if rflagsbits_get_sf(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_sf(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_sf(rflagsbits_get_sf(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    let x86 = (if rflagsbits_get_of(undefined_mask) == 1 then {
        let rflags_var = r_rflags(0) : int;
        let (sailval, x86) = undef_flg(0) : (int, int);
        let new_rflags = set_rflagsbits_get_of(sailval, rflags_var) : int;
        write_rflags(new_rflags, 0)
    } else {
        let rflags_var = r_rflags(0) : int;
        let new_rflags = set_rflagsbits_get_of(rflagsbits_get_of(user_flags_vector), rflags_var) : int;
        write_rflags(new_rflags, 0)
    }) : int;
    0
}
