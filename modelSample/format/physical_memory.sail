$include "./modes.sail"

val physical_address_p : int -> bool

function physical_address_p phy_addr = {
    unsigned_byte_p(52, phy_addr)
}

val rm_low_32 : int -> bits(32)

function rm_low_32 addr = {
    assert(not_bool(app_view));
    let byte0 : bits(8) = bits_of_int(loghead(8, unsigned(memi(bits_of_int(addr, 64)))), 8);
    let byte1 : bits(8) = bits_of_int(loghead(8, unsigned(memi(bits_of_int(1 + addr, 64)))), 8);
    let word0 : bits(16) = logior(bits_of_int(ash(unsigned(byte1), 8), 16), sail_zero_extend(byte0, 16));
    let byte2 : bits(8) = bits_of_int(loghead(8, unsigned(memi(bits_of_int(2 + addr, 64)))), 8);
    let byte3 : bits(8) = bits_of_int(loghead(8, unsigned(memi(bits_of_int(3 + addr, 64)))), 8);
    let word1 : bits(16) = logior(bits_of_int(ash(unsigned(byte3), 8), 16), sail_zero_extend(byte2, 16));
    let dword : bits(32) = logior(bits_of_int(ash(unsigned(word1), 16), 32), sail_zero_extend(word0, 32));
    dword
}

val wm_low_32 : (bits(52), bits(32)) -> unit

function wm_low_32 (addr, sailval) = {
    assert(not_bool(app_view));
    let addr : int = unsigned(addr);
    let byte0 : bits(8) = slice(sailval, 0, 8);
    let byte1 : bits(8) = slice(sailval, 8, 8);
    let byte2 : bits(8) = slice(sailval, 16, 8);
    let byte3 : bits(8) = slice(sailval, 24, 8);
    bang_memi(bits_of_int(addr, 64), byte0);
    bang_memi(bits_of_int(1 + addr, 64), byte1);
    bang_memi(bits_of_int(2 + addr, 64), byte2);
    bang_memi(bits_of_int(3 + addr, 64), byte3)
}

val rm_low_64 : int -> bits(64)

function rm_low_64 addr = {
    assert(not_bool(app_view));
    let dword0 : bits(32) = rm_low_32(addr);
    let dword1 : bits(32) = rm_low_32(4 + addr);
    let qword : bits(64) = logior(bits_of_int(ash(unsigned(dword1), 32), 64), sail_zero_extend(dword0, 64));
    qword
}

val wm_low_64 : (bits(52), bits(64)) -> unit

function wm_low_64 (addr, sailval) = {
    assert(not_bool(app_view));
    let addr : int = unsigned(addr);
    let dword0 : bits(32) = slice(sailval, 0, 32);
    let dword1 : bits(32) = slice(sailval, 32, 32);
    wm_low_32(bits_of_int(addr, 52), dword0);
    wm_low_32(bits_of_int(4 + addr, 52), dword1)
}
