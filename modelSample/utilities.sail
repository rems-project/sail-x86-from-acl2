$include <prelude.sail>
$include <string.sail>
val trunc : (int, int) -> int
function trunc (n, x) =
(match n {
1 => n08(x),
2 => n16(x),
4 => n32(x),
8 => n64(x),
16 => n128(x),
_ => unsigned(get_slice_int(n, x, 0))
}) : int

