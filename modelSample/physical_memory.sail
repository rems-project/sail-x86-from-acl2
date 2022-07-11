$include <prelude.sail>
$include <string.sail>
$include "modes.sail"
val physical_address_p : (int) -> bool effect {escape}
function physical_address_p (phy_addr) =
unsigned_byte_p(52, phy_addr)

val rm_low_32 : (int) -> bits(32) effect {rmem, rreg}
function rm_low_32 (addr) =
{ assert(not_bool(app_view));
let byte0 : bits(8) = (bits_of_int(loghead(8, unsigned(memi(bits_of_int(addr, 64)))), 8)) in
let byte1 : bits(8) = (bits_of_int(loghead(8, unsigned(memi(bits_of_int((1) + (addr), 64)))), 8)) in
let word0 : bits(16) = (logior(bits_of_int(ash(unsigned(byte1), 8), 16), sail_zero_extend(byte0, 16))) in
let byte2 : bits(8) = (bits_of_int(loghead(8, unsigned(memi(bits_of_int((2) + (addr), 64)))), 8)) in
let byte3 : bits(8) = (bits_of_int(loghead(8, unsigned(memi(bits_of_int((3) + (addr), 64)))), 8)) in
let word1 : bits(16) = (logior(bits_of_int(ash(unsigned(byte3), 8), 16), sail_zero_extend(byte2, 16))) in
let dword : bits(32) = (logior(bits_of_int(ash(unsigned(word1), 16), 32), sail_zero_extend(word0, 32))) in
dword
}

val wm_low_32 : (bits(52), bits(32)) -> unit effect {eamem, rreg, wmv}
function wm_low_32 (addr, sailval) =
{ assert(not_bool(app_view));
let addr : int = (unsigned(addr)) in
let byte0 : bits(8) = (slice(sailval, 0, 8)) in
let byte1 : bits(8) = (slice(sailval, 8, 8)) in
let byte2 : bits(8) = (slice(sailval, 16, 8)) in
let byte3 : bits(8) = (slice(sailval, 24, 8)) in
{ bang_memi(bits_of_int(addr, 64), byte0);
bang_memi(bits_of_int((1) + (addr), 64), byte1);
bang_memi(bits_of_int((2) + (addr), 64), byte2);
bang_memi(bits_of_int((3) + (addr), 64), byte3)
}
}

val rm_low_64 : (int) -> bits(64) effect {rmem, rreg}
function rm_low_64 (addr) =
{ assert(not_bool(app_view));
let dword0 : bits(32) = (rm_low_32(addr)) in
let dword1 : bits(32) = (rm_low_32((4) + (addr))) in
let qword : bits(64) = (logior(bits_of_int(ash(unsigned(dword1), 32), 64), sail_zero_extend(dword0, 64))) in
qword
}

val wm_low_64 : (bits(52), bits(64)) -> unit effect {eamem, rreg, wmv}
function wm_low_64 (addr, sailval) =
{ assert(not_bool(app_view));
let addr : int = (unsigned(addr)) in
let dword0 : bits(32) = (slice(sailval, 0, 32)) in
let dword1 : bits(32) = (slice(sailval, 32, 32)) in
{ wm_low_32(bits_of_int(addr, 52), dword0);
wm_low_32(bits_of_int((4) + (addr), 52), dword1)
}
}

