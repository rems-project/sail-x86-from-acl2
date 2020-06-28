$include <prelude.sail>
$include <string.sail>
$include "paging.sail"
/*Definitions of @ ( see rb ) and @ ( see wb )*/
/*<p>The functions @ ( ' rb ' ) ( read bytes ) and @ ( ' wb ' ) ( write 
 bytes ) are used in reasoning about memory reads and writes. Functions 
 like @ ( ' rml08 ' ) , @ ( ' rml16 ' ) , @ ( ' rml32 ' ) , and @ ( ' rml64 ' ) are reduced to 
 @ ( ' rb ' ) , and @ ( ' wml08 ' ) , @ ( ' wml16 ' ) , @ ( ' wml32 ' ) , and @ ( ' wml64 ' ) to 
 @ ( ' wb ' ) during reasoning.</p>*/
val rb_1 : (int, int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rb_1 (n, addr, r_x, x86) =
if (n) == (0)
then (None() : option(string), 0 : int, 0 : int)
else if not_bool(canonical_address_p(addr))
then (Some("rb-1"), 0 : int, 0 : int)
else let (flg0, byte0, x86) = (rvm08(addr, 0)) : (option(string), int, int) in
if is_some(flg0)
then (flg0, 0 : int, 0 : int)
else let (rest_flg, rest_bytes, x86) = (rb_1((n) - (1), (1) + (addr), r_x, 0)) : (option(string), int, int) in
(rest_flg, binary_logior(byte0, ash(rest_bytes, 8)), 0 : int)

val rb : (int, int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rb (n, addr, r_x, x86) =
rb_1(n, addr, r_x, 0)

val wb_1 : (int, int, string, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wb_1 (n, addr, w, value, x86) =
if (n) == (0)
then (None() : option(string), 0 : int)
else if not_bool(canonical_address_p(addr))
then (Some("Empty error"), 0 : int)
else let (flg0, x86) = (wvm08(addr, loghead(8, value), 0)) : (option(string), int) in
if is_some(flg0)
then (flg0, 0 : int)
else let (rest_flg, x86) = (wb_1((n) - (1), (1) + (addr), w, logtail(8, value), 0)) : (option(string), int) in
(rest_flg, 0 : int)

val wb : (int, int, string, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wb (n, addr, w, value, x86) =
wb_1(n, addr, w, value, 0)

val rml08 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml08 (lin_addr, r_x, x86) =
rb(1, lin_addr, r_x, 0)

val riml08 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function riml08 (lin_addr, r_x, x86) =
let (flag, sailval, x86) = (rml08(lin_addr, r_x, 0)) : (option(string), int, int) in
(flag, n08_to_i08(sailval), 0 : int)

val wml08 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml08 (lin_addr, sailval, x86) =
wb(1, lin_addr, ":W", sailval, 0)

val wiml08 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wiml08 (lin_addr, sailval, x86) =
wml08(lin_addr, n08(sailval), 0)

val rml16 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml16 (lin_addr, r_x, x86) =
let one__plus_lin_addr = ((1) + (lin_addr)) : int in
if canonical_address_p(one__plus_lin_addr)
then rb(2, lin_addr, r_x, 0)
else (Some("rml16"), 0 : int, 0 : int)

val riml16 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function riml16 (lin_addr, r_x, x86) =
let (flag, sailval, x86) = (rml16(lin_addr, r_x, 0)) : (option(string), int, int) in
(flag, n16_to_i16(sailval), 0 : int)

val wml16 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml16 (lin_addr, sailval, x86) =
if true
then let one__plus_lin_addr = ((1) + (lin_addr)) : int in
if canonical_address_p(one__plus_lin_addr)
then wb(2, lin_addr, ":W", sailval, 0)
else (Some("wml16"), 0 : int)
else (Some("wml16"), 0 : int)

val wiml16 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wiml16 (lin_addr, sailval, x86) =
wml16(lin_addr, n16(sailval), 0)

val rml32 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml32 (lin_addr, r_x, x86) =
if true
then let three__plus_lin_addr = ((3) + (lin_addr)) : int in
if canonical_address_p(three__plus_lin_addr)
then rb(4, lin_addr, r_x, 0)
else (Some("rml32"), 0 : int, 0 : int)
else (Some("rml32"), 0 : int, 0 : int)

val riml32 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function riml32 (lin_addr, r_x, x86) =
let (flag, sailval, x86) = (rml32(lin_addr, r_x, 0)) : (option(string), int, int) in
(flag, n32_to_i32(sailval), 0 : int)

val wml32 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml32 (lin_addr, sailval, x86) =
if true
then let three__plus_lin_addr = ((3) + (lin_addr)) : int in
if canonical_address_p(three__plus_lin_addr)
then wb(4, lin_addr, ":W", sailval, 0)
else (Some("wml32"), 0 : int)
else (Some("wml32"), 0 : int)

val wiml32 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wiml32 (lin_addr, sailval, x86) =
wml32(lin_addr, n32(sailval), 0)

val rml48 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml48 (lin_addr, r_x, x86) =
if true
then let five__plus_lin_addr = ((5) + (lin_addr)) : int in
if canonical_address_p(five__plus_lin_addr)
then rb(6, lin_addr, r_x, 0)
else (Some("rml48"), 0 : int, 0 : int)
else (Some("rml48"), 0 : int, 0 : int)

val wml48 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml48 (lin_addr, sailval, x86) =
if true
then let five__plus_lin_addr = ((5) + (lin_addr)) : int in
if canonical_address_p(five__plus_lin_addr)
then wb(6, lin_addr, ":W", sailval, 0)
else (Some("wml48"), 0 : int)
else (Some("wml48"), 0 : int)

val rml64 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml64 (lin_addr, r_x, x86) =
if true
then let seven__plus_lin_addr = ((7) + (lin_addr)) : int in
if canonical_address_p(seven__plus_lin_addr)
then rb(8, lin_addr, r_x, 0)
else (Some("rml64"), 0 : int, 0 : int)
else (Some("rml64"), 0 : int, 0 : int)

val riml64 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function riml64 (lin_addr, r_x, x86) =
let (flag, sailval, x86) = (rml64(lin_addr, r_x, 0)) : (option(string), int, int) in
(flag, n64_to_i64(sailval), 0 : int)

val wml64 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml64 (lin_addr, sailval, x86) =
if true
then let seven__plus_lin_addr = ((7) + (lin_addr)) : int in
if canonical_address_p(seven__plus_lin_addr)
then wb(8, lin_addr, ":W", sailval, 0)
else (Some("wml64"), 0 : int)
else (Some("wml64"), 0 : int)

val wiml64 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wiml64 (lin_addr, sailval, x86) =
wml64(lin_addr, n64(sailval), 0)

val rml80 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml80 (lin_addr, r_x, x86) =
if true
then let nine__plus_lin_addr = ((9) + (lin_addr)) : int in
if canonical_address_p(nine__plus_lin_addr)
then rb(10, lin_addr, r_x, 0)
else (Some("rml80"), 0 : int, 0 : int)
else (Some("rml80"), 0 : int, 0 : int)

val wml80 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml80 (lin_addr, sailval, x86) =
if true
then let nine__plus_lin_addr = ((9) + (lin_addr)) : int in
if canonical_address_p(nine__plus_lin_addr)
then wb(10, lin_addr, ":W", sailval, 0)
else (Some("wml80"), 0 : int)
else (Some("wml80"), 0 : int)

val rml128 : (int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml128 (lin_addr, r_x, x86) =
if true
then let one_5_plus_lin_addr = ((15) + (lin_addr)) : int in
if canonical_address_p(one_5_plus_lin_addr)
then rb(16, lin_addr, r_x, 0)
else (Some("rml128"), 0 : int, 0 : int)
else (Some("rml128"), 0 : int, 0 : int)

val wml128 : (int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml128 (lin_addr, sailval, x86) =
if true
then let one_5_plus_lin_addr = ((15) + (lin_addr)) : int in
if canonical_address_p(one_5_plus_lin_addr)
then wb(16, lin_addr, ":W", sailval, 0)
else (Some("wml128"), 0 : int)
else (Some("wml128"), 0 : int)

/*Functions to read/write 8/16/32/64/128 bits into the memory:*/
val rml_size : (int, int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function rml_size (nbytes, addr, r_x, x86) =
(match nbytes {
1 => rml08(addr, r_x, 0),
2 => rml16(addr, r_x, 0),
4 => rml32(addr, r_x, 0),
6 => rml48(addr, r_x, 0),
8 => rml64(addr, r_x, 0),
10 => rml80(addr, r_x, 0),
16 => rml128(addr, r_x, 0),
_ => if canonical_address_p((-1) + ((nbytes) + (addr)))
then rb(nbytes, addr, r_x, 0)
else (Some("rml-size"), 0 : int, 0 : int)
}) : (option(string), int, int)

val riml_size : (int, int, string, int) -> (option(string), int, int) effect {rreg, rmem, escape}
function riml_size (nbytes, addr, r_x, x86) =
(match nbytes {
1 => riml08(addr, r_x, 0),
2 => riml16(addr, r_x, 0),
4 => riml32(addr, r_x, 0),
8 => riml64(addr, r_x, 0),
_ => (Some("riml-size"), nbytes, 0 : int)
}) : (option(string), int, int)

val wml_size : (int, int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wml_size (nbytes, addr, sailval, x86) =
(match nbytes {
1 => wml08(addr, sailval, 0),
2 => wml16(addr, sailval, 0),
4 => wml32(addr, sailval, 0),
6 => wml48(addr, sailval, 0),
8 => wml64(addr, sailval, 0),
10 => wml80(addr, sailval, 0),
16 => wml128(addr, sailval, 0),
_ => if canonical_address_p((-1) + ((nbytes) + (addr)))
then wb(nbytes, addr, ":W", sailval, 0)
else (Some("wml-size"), 0 : int)
}) : (option(string), int)

val wiml_size : (int, int, int, int) -> (option(string), int) effect {rreg, wmv, eamem, escape}
function wiml_size (nbytes, addr, sailval, x86) =
(match nbytes {
1 => wiml08(addr, sailval, 0),
2 => wiml16(addr, sailval, 0),
4 => wiml32(addr, sailval, 0),
8 => wiml64(addr, sailval, 0),
_ => (Some("wiml-size"), 0 : int)
}) : (option(string), int)

