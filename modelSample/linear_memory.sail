$include <prelude.sail>
$include <string.sail>
$include "paging.sail"
/*Definitions of @ ( see rb ) and @ ( see wb )*/
/*<p>The functions @ ( ' rb ' ) ( read bytes ) and @ ( ' wb ' ) ( write 
 bytes ) are used in reasoning about memory reads and writes. Functions 
 like @ ( ' rml08 ' ) , @ ( ' rml16 ' ) , @ ( ' rml32 ' ) , and @ ( ' rml64 ' ) are reduced to 
 @ ( ' rb ' ) , and @ ( ' wml08 ' ) , @ ( ' wml16 ' ) , @ ( ' wml32 ' ) , and @ ( ' wml64 ' ) to 
 @ ( ' wb ' ) during reasoning.</p>*/
val rb_1 : (nat, int, string) -> (option(string), int) effect {escape, rmem, rreg}
function rb_1 (n, addr, r_x) =
if (n) == (0)
then (None() : option(string), 0)
else if not_bool(canonical_address_p(addr))
then (Some("rb-1"), 0)
else let (flg0, byte0) : (option(string), bits(8)) = (rvm08(bits_of_int(addr, 48))) in
if is_some(flg0)
then (flg0, 0)
else let (rest_flg, rest_bytes) : (option(string), int) = (rb_1(nat_of_int((n) - (1)), (1) + (addr), r_x)) in
(rest_flg, logior(unsigned(byte0), ash(rest_bytes, 8)))

val rb : (nat, int, string) -> (option(string), int) effect {escape, rmem, rreg}
function rb (n, addr, r_x) =
rb_1(n, addr, r_x)

val wb_1 : (nat, int, string, int) -> option(string) effect {eamem, escape, rreg, wmv}
function wb_1 (n, addr, w, value) =
if (n) == (0)
then None() : option(string)
else if not_bool(canonical_address_p(addr))
then Some("Empty error")
else let flg0 : option(string) = (wvm08(bits_of_int(addr, 48), bits_of_int(loghead(8, value), 8))) in
if is_some(flg0)
then flg0
else let rest_flg : option(string) = (wb_1(nat_of_int((n) - (1)), (1) + (addr), w, logtail(8, value))) in
rest_flg

val wb : (nat, int, string, nat) -> option(string) effect {eamem, escape, rreg, wmv}
function wb (n, addr, w, value) =
wb_1(n, addr, w, value)

val rml08 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml08 (lin_addr, r_x) =
rb(1, signed(lin_addr), r_x)

val riml08 : (sbits(48), string) -> (option(string), sbits(8)) effect {escape, rmem, rreg}
function riml08 (lin_addr, r_x) =
let (flag, sailval) : (option(string), int) = (rml08(lin_addr, r_x)) in
(flag, bits_of_int(sailval, 8))

val wml08 : (sbits(48), bits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml08 (lin_addr, sailval) =
wb(1, signed(lin_addr), ":W", unsigned(sailval))

val wiml08 : (sbits(48), sbits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wiml08 (lin_addr, sailval) =
wml08(lin_addr, sailval)

val rml16 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml16 (lin_addr, r_x) =
let one__plus_lin_addr : sbits(49) = (bits_of_int((1) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(one__plus_lin_addr))
then rb(2, signed(lin_addr), r_x)
else (Some("rml16"), 0)

val riml16 : (sbits(48), string) -> (option(string), sbits(16)) effect {escape, rmem, rreg}
function riml16 (lin_addr, r_x) =
let (flag, sailval) : (option(string), int) = (rml16(lin_addr, r_x)) in
(flag, bits_of_int(sailval, 16))

val wml16 : (sbits(48), bits(16)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml16 (lin_addr, sailval) =
if true
then let one__plus_lin_addr : sbits(49) = (bits_of_int((1) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(one__plus_lin_addr))
then wb(2, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml16")
else Some("wml16")

val wiml16 : (sbits(48), sbits(16)) -> option(string) effect {eamem, escape, rreg, wmv}
function wiml16 (lin_addr, sailval) =
wml16(lin_addr, sailval)

val rml32 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml32 (lin_addr, r_x) =
if true
then let three__plus_lin_addr : sbits(49) = (bits_of_int((3) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(three__plus_lin_addr))
then rb(4, signed(lin_addr), r_x)
else (Some("rml32"), 0)
else (Some("rml32"), 0)

val riml32 : (sbits(48), string) -> (option(string), sbits(32)) effect {escape, rmem, rreg}
function riml32 (lin_addr, r_x) =
let (flag, sailval) : (option(string), int) = (rml32(lin_addr, r_x)) in
(flag, bits_of_int(sailval, 32))

val wml32 : (sbits(48), bits(32)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml32 (lin_addr, sailval) =
if true
then let three__plus_lin_addr : sbits(49) = (bits_of_int((3) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(three__plus_lin_addr))
then wb(4, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml32")
else Some("wml32")

val wiml32 : (sbits(48), sbits(32)) -> option(string) effect {eamem, escape, rreg, wmv}
function wiml32 (lin_addr, sailval) =
wml32(lin_addr, sailval)

val rml48 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml48 (lin_addr, r_x) =
if true
then let five__plus_lin_addr : sbits(49) = (bits_of_int((5) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(five__plus_lin_addr))
then rb(6, signed(lin_addr), r_x)
else (Some("rml48"), 0)
else (Some("rml48"), 0)

val wml48 : (sbits(48), bits(48)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml48 (lin_addr, sailval) =
if true
then let five__plus_lin_addr : sbits(49) = (bits_of_int((5) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(five__plus_lin_addr))
then wb(6, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml48")
else Some("wml48")

val rml64 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml64 (lin_addr, r_x) =
if true
then let seven__plus_lin_addr : sbits(49) = (bits_of_int((7) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(seven__plus_lin_addr))
then rb(8, signed(lin_addr), r_x)
else (Some("rml64"), 0)
else (Some("rml64"), 0)

val riml64 : (sbits(48), string) -> (option(string), sbits(64)) effect {escape, rmem, rreg}
function riml64 (lin_addr, r_x) =
let (flag, sailval) : (option(string), int) = (rml64(lin_addr, r_x)) in
(flag, bits_of_int(sailval, 64))

val wml64 : (sbits(48), bits(64)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml64 (lin_addr, sailval) =
if true
then let seven__plus_lin_addr : sbits(50) = (bits_of_int((7) + (signed(lin_addr)), 50)) in
if canonical_address_p(signed(seven__plus_lin_addr))
then wb(8, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml64")
else Some("wml64")

val wiml64 : (sbits(48), sbits(64)) -> option(string) effect {eamem, escape, rreg, wmv}
function wiml64 (lin_addr, sailval) =
wml64(lin_addr, sailval)

val rml80 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml80 (lin_addr, r_x) =
if true
then let nine__plus_lin_addr : sbits(49) = (bits_of_int((9) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(nine__plus_lin_addr))
then rb(10, signed(lin_addr), r_x)
else (Some("rml80"), 0)
else (Some("rml80"), 0)

val wml80 : (sbits(48), bits(80)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml80 (lin_addr, sailval) =
if true
then let nine__plus_lin_addr : sbits(49) = (bits_of_int((9) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(nine__plus_lin_addr))
then wb(10, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml80")
else Some("wml80")

val rml128 : (sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml128 (lin_addr, r_x) =
if true
then let one_5_plus_lin_addr : sbits(49) = (bits_of_int((15) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(one_5_plus_lin_addr))
then rb(16, signed(lin_addr), r_x)
else (Some("rml128"), 0)
else (Some("rml128"), 0)

val wml128 : (sbits(48), bits(128)) -> option(string) effect {eamem, escape, rreg, wmv}
function wml128 (lin_addr, sailval) =
if true
then let one_5_plus_lin_addr : sbits(49) = (bits_of_int((15) + (signed(lin_addr)), 49)) in
if canonical_address_p(signed(one_5_plus_lin_addr))
then wb(16, signed(lin_addr), ":W", unsigned(sailval))
else Some("wml128")
else Some("wml128")

/*Functions to read/write 8/16/32/64/128 bits into the memory:*/
val rml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), string) -> (option(string), int) effect {escape, rmem, rreg}
function rml_size (nbytes, addr, r_x) =
(match nbytes {
1 => rml08(addr, r_x),
2 => rml16(addr, r_x),
4 => rml32(addr, r_x),
6 => rml48(addr, r_x),
8 => rml64(addr, r_x),
10 => rml80(addr, r_x),
16 => rml128(addr, r_x),
_ => if canonical_address_p((-1) + ((nbytes) + (signed(addr))))
then rb(nbytes, signed(addr), r_x)
else (Some("rml-size"), 0)
}) : (option(string), int)

val riml_size : ({|1, 2, 4, 8|}, sbits(48), string) -> (option(string), sbits(64)) effect {escape, rmem, rreg}
function riml_size (nbytes, addr, r_x) =
(match nbytes {
1 => let (elem0, elem1) : (option(string), sbits(8)) = (riml08(addr, r_x)) in
(elem0, sail_sign_extend(elem1, 64)),
2 => let (elem0, elem1) : (option(string), sbits(16)) = (riml16(addr, r_x)) in
(elem0, sail_sign_extend(elem1, 64)),
4 => let (elem0, elem1) : (option(string), sbits(32)) = (riml32(addr, r_x)) in
(elem0, sail_sign_extend(elem1, 64)),
8 => riml64(addr, r_x),
_ => (Some("riml-size"), bits_of_int(nbytes, 64))
}) : (option(string), sbits(64))

val wml_size : ({|1, 2, 4, 6, 8, 10, 16|}, sbits(48), nat) -> option(string) effect {eamem, escape, rreg, wmv}
function wml_size (nbytes, addr, sailval) =
(match nbytes {
1 => wml08(addr, bits_of_int(sailval, 8)),
2 => wml16(addr, bits_of_int(sailval, 16)),
4 => wml32(addr, bits_of_int(sailval, 32)),
6 => wml48(addr, bits_of_int(sailval, 48)),
8 => wml64(addr, bits_of_int(sailval, 64)),
10 => wml80(addr, bits_of_int(sailval, 80)),
16 => wml128(addr, bits_of_int(sailval, 128)),
_ => if canonical_address_p((-1) + ((nbytes) + (signed(addr))))
then wb(nbytes, signed(addr), ":W", sailval)
else Some("wml-size")
}) : option(string)

val wiml_size : ({|1, 2, 4, 8|}, sbits(48), int) -> option(string) effect {eamem, escape, rreg, wmv}
function wiml_size (nbytes, addr, sailval) =
(match nbytes {
1 => wiml08(addr, bits_of_int(sailval, 8)),
2 => wiml16(addr, bits_of_int(sailval, 16)),
4 => wiml32(addr, bits_of_int(sailval, 32)),
8 => wiml64(addr, bits_of_int(sailval, 64)),
_ => Some("wiml-size")
}) : option(string)

