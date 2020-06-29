/*-----------------------------------------------------------------*/
This file contains the handwritten functions for the ACL2
translation.
/*-----------------------------------------------------------------*/

$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <regfp.sail>
$include <real.sail>
$include <string.sail>

overload operator ^ = {concat_str, and_bool}

/*-----------------------------------------------------------------*/
// Misc
/*-----------------------------------------------------------------*/

/*
The `feature-flags` function in cpuid.lisp takes an argument
`features`.  The guard specifies `features` must be a subset of the
supported features, and so this is proved statically.  The function
checks that all features in its argument are enabled by passing each
one to `feature-flag` (not plural). In turn, `feature-flag` uses
`cpuid-flag` macro which uses `cpuid-flag-fn`... which is always 1
because all features are enabled by default.  Thus, translate
`feature-flag` simply as `1`.

Append `_fn` to the names to stop shadowing a formal parameter at
a use-site in `chk_exc_fn` in `dispatch_macros.lisp`.

TODO: avoidance of shadowing should be done elsewhere.
*/

val feature_flag_fn : (string, int) -> int
function feature_flag_fn (feature, x86_dummy) = 1


val feature_flags_fn : (list(string), int) -> int
function feature_flags_fn (features, x86_dummy) =
	match features {
		[||] => 1,
		x :: xs =>	if feature_flag_fn(x, 0) == 0
					then 0
					else feature_flags_fn(xs, 0)
	}

/*-----------------------------------------------------------------*/
// Exceptions
/*-----------------------------------------------------------------*/

union exception = {
	Emsg : string
}


/*-----------------------------------------------------------------*/
// Undefined behaviour
//
// In the ACL2 model, undefined values for things like flags just
// return zero.  Here we use Sail's builtin `undefined`, which
// actually does the same.
/*-----------------------------------------------------------------*/

val undef_read_logic : int -> (nat, int) effect {undef}
function undef_read_logic (x86_dummy) = (undefined, 0)

/*-----------------------------------------------------------------*/
// Utility functions
/*-----------------------------------------------------------------*/

/*
pow2 taken from sail-riscv/model/prelude.sail
*/

val pow2 = "pow2" : forall 'n. atom('n) -> atom(2 ^ 'n)

/*
TODO: is there a utility function for this
TODO: make bool_to_bit and bit_to_bool a mapping?

We use `int` as return type to link in with the rest of the framework
*/
val bool_to_bit : bool -> int
function bool_to_bit (b) = match b {
    //true  => 0b1,
    //false => 0b0
    true  => 1,
    false => 0
}

val bit_to_bool : bits(1) -> bool
function bit_to_bool (b) = match b {
    0b1 => true,
    0b0 => false
}

val the_int : (int) -> int
function the_int i = i

val the_nat : (int) -> nat effect {escape}
function the_nat i = {
	assert (i >= 0);
	i
}

val the_range : forall 'n 'm, 'n <= 'm. (int('n), int('m), int) -> range('n, 'm) effect {escape}
function the_range(n, m, i) = {
	assert(n <= i & i <= m);
	i
}

val isEven : int -> bool
function isEven x =
	if get_slice_int(1, x, 0) == 0b0 then true else false

// b-xor: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____B-XOR
val b_xor : (int, int) -> int effect {escape}
function b_xor (i, j) = {
	assert(0 <= i & i <= 1 & 0 <= j & j <= 1);
	if i == 0
		then if j == 0 then 0 else 1
		else if j == 0 then 1 else 0
}

val in_list : (string, list(string)) -> bool
function in_list (x, xs) = match xs {
	y :: ys => if x == y then true else in_list(x, ys),
	[||] => false
}

/*-----------------------------------------------------------------*/
// Unsigned and signed byte_p - recognisers
/*-----------------------------------------------------------------*/

/*
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____UNSIGNED-BYTE-P
*/
val unsigned_byte_p : (int, int) -> bool effect {escape}
function unsigned_byte_p (bits, x) = {
	assert(bits >= 0 & x >= 0);
	x < pow2(bits)
}

/*
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____SIGNED-BYTE-P
*/
val signed_byte_p : (int, int) -> bool effect {escape}
function signed_byte_p (bits, x) = {
	assert(bits >= 0);
	negate(pow2(bits - 1)) <= x & x < pow2(bits - 1)
}

/*-----------------------------------------------------------------*/
// Logic functions
/*-----------------------------------------------------------------*/

/*
`loghead size i` returns size low-order bits of i.
In C-like languages: `i & ((1 << size) - 1)`
If size = 0, then return = 0

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____LOGHEAD
*/

val loghead : (int, int) -> int
function loghead (size, i) =
	if size == 0
	then 0
	else
		unsigned(
			get_slice_int(size, i, 0)
		)


/*-----------------------------------------------------------------*/
/*
`logtail pos i` returns high-order part of i starting at bit pos

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____LOGTAIL
*/
val logtail : (int, int) -> int
function logtail (pos, x) =
	shr_int(x, pos)

/*-----------------------------------------------------------------*/
/*
"For a nonnegative integer i and an integer j, (logbitp i j) is a
Boolean, which is t if and only if the value of the ith bit is 1 in
the two's complement representation of j.

(Logbitp i j) has a guard that i is a nonnegative integer and j is
an integer."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGBITP
*/
val logbitp : (int, int) -> bool effect {escape}
function logbitp (i, j)={
	assert(i >= 0);
    bit_to_bool(
        get_slice_int(1, j, i)
    )
}

/*
Also `logbit` which "is just like the Common Lisp function
`(logbitppos i)`, except that we return 1 or 0 (instead of t or
nil)."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGBIT
*/
val logbit : (int, int) -> int effect {escape}
function logbit (pos, j) = {
	assert(pos >= 0);
	unsigned(get_slice_int(1, j, pos))
}

/*-----------------------------------------------------------------*/
/*
The function lognot is used in the definition of logcount.  It is
simply a bitwise not.

See (logcount): http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGCOUNT
See (lognot): http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGNOT

TODO: why do we take the lognot?  This does a bitwise not and seems
to change the number of on bits?  Maybe 'on' bits means 0 when
negative.
*/

val lognot : (int) -> int
function lognot x = negate(x) - 1

val logcount : (int) -> int effect {escape}
function logcount(x) =
	if x == 0 then 0 else
	if x < 0  then logcount(lognot(x)) else
	if isEven (x)
		then logcount (shr_int (x, 1))
		else logcount (shr_int (x, 1)) + 1
/*-----------------------------------------------------------------*/
/*
floor

This implementation passes the tests mentioned in the spec above.
An old implementation, used conversion to and from real numbers.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____FLOOR
*/
val floor2 : (int, int) -> int
function floor2 (i, j) = {
	tdiv = tdiv_int(i,j);
	tmod = tmod_int(i,j);
	if ((j > 0) ^ (tmod >= 0)) | ((j < 0) ^ (tmod <= 0))
	then tdiv
	else tdiv - 1
}

/*-----------------------------------------------------------------*/
/*
logand

Wikipedia: "Shifting right by n bits on a two's complement signed
binary number has the effect of dividing it by 2n, but it always
rounds down (towards negative infinity)."

This is what we want when replacing the `floor` function

NOTE: at first tried using tdiv_int instead of floor2, but this does
not have the same behaviour as floor in the ACL2 spec.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGAND
*/

val binary_logand : (int, int) -> int
function binary_logand (i, j) =
	if i == 0 then 0 else
	if j == 0 then 0 else
	if i == -1 then j else
	if j == -1 then i else
	let x = 2 * (binary_logand (floor2 (i, 2), (floor2 (j, 2)))) in
	x + (	if isEven (i) then 0 else
			if isEven (j) then 0 else
			1)

/*-----------------------------------------------------------------*/
/*
logior

Here only the binary version is implemented.  It's basically a
copy/paste job from the docs by this stage.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____LOGIOR
*/
val binary_logior : (int, int) -> int
function binary_logior (i, j) =
	lognot(binary_logand(lognot(i), lognot(j)))

/*-----------------------------------------------------------------*/
/*
logxor

Here only the binary version is implemented.

See:
	logxor: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGXOR
	logeqv: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGEQV
	logorc1:http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGORC1
*/
val binary_logorc1 : (int, int) -> int
function binary_logorc1 (i, j) = binary_logior(lognot(i), j)

val binary_logeqv : (int, int) -> int
function binary_logeqv (i, j) =
	binary_logand(binary_logorc1(i, j), binary_logorc1(j, i))


val binary_logxor : (int, int) -> int
function binary_logxor (i, j)=
	lognot(binary_logeqv(i, j))

/*-----------------------------------------------------------------*/
/*
logext

See:
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGEXT
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGAPP
*/
val binary_logapp : (int, int, int) -> int
function binary_logapp (size, i, j) =
	(loghead (size, i)) + (j * pow2(size))

val binary_logext : (int, int) -> int effect {escape}
function binary_logext (size, i) = {
	assert(0 < size);
	binary_logapp (size - 1,
			i,
			if logbitp (size - 1, i)
				then negate(1)
				else 0)
}


/*-----------------------------------------------------------------*/
/*
n-size.  Defined in `utilities.lisp` and simply generates, for
example `(n08 x)` from `n-size 8 x`.  We find that `n08 x` is, under
`:logic` simply `loghead 8 x`.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____N08

TODO: maybe do this automatically by evaluating `np-def-n` in
`utilities.lisp`
*/
val n_size : (int, int) -> int
function n_size (n, x) = loghead (n, x)

/*-----------------------------------------------------------------*/
/*
ash

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____ASH

TODO: check this implementation matches the spec
*/
val ash : (int, int) -> int
function ash (i, c) =
	if c == 0 then i else
	if c > 0 then	_shl_int(i, c)
	else			_shr_int(i, negate(c))

/*-----------------------------------------------------------------*/
/*
abs

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____ABS
*/
val abs : (int) -> int
function abs x = if x < 0 then negate(x) else x

/*-----------------------------------------------------------------*/
/*
mod

This implementation passes the examples in the spec below.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____MOD

TODO: is there an inbuilt for this?
*/
val mod : (int, int) -> int
function mod (x, y) =
	x - (floor2(x,y) * y)

/*-----------------------------------------------------------------*/
/*
rotate-left.

rotates-spec.lisp uses `fast-rotate-left` in
`centaur/bitops/fast-rotate`.  Apparently this is logically
equivalent to `rotate-left`, so we implement our own here.

Based loosely on:
	* http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____ROTATE-LEFT
	* http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____ROTATE-RIGHT
*/
val rotate_left : (int, int, int) -> int effect {escape}
function rotate_left (x, width, places) = {
	assert(0 <= places);
	placesMod = mod(places, width);
	low_num = width - placesMod;
	xl = get_slice_int(low_num, x, 0);
	xh = get_slice_int(placesMod, x, low_num);
	unsigned(xl @ xh)
}

val rotate_right : (int, int, int) -> int effect {escape}
function rotate_right (x, width, places) = {
	assert(0 <= places);
	placesMod = mod(places, width);
	xl = get_slice_int(placesMod, x, 0);
	xh = get_slice_int(width - placesMod, x, placesMod);
	unsigned(xl @ xh)
}

/*-----------------------------------------------------------------*/
/*
MERGE-2-U64S etc.

"Concatenate 2 64-bit numbers together to form an 128-bit result."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____MERGE-2-U64S
OR: `:pe MERGE-2-U64S` etc.

TODO: implement `the`s shown in the original functions.
*/

val merge_2_u64s : (int, int) -> int
function merge_2_u64s (a1, a0) = binary_logior(ash(a1, 64), a0)

val merge_2_u32s : (int, int) -> int
function merge_2_u32s (a1, a0) = binary_logior(ash(a1, 32), a0)

val merge_4_u32s : (int, int, int, int) -> int
function merge_4_u32s (a3, a2, a1, a0) =
	merge_2_u64s(merge_2_u32s(a3, a2), merge_2_u32s(a1, a0))

/*-----------------------------------------------------------------*/
// Some state
/*-----------------------------------------------------------------*/

infix 7 <<
overload operator << = {sail_shiftleft}

// Used in part-install.  See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-INSTALL
// TODO: this is almost identical to genericBitstructUpdater below - maybe merge them

val changeBits : (int, int, int, int) -> int
function changeBits (x, low, width, v) =
	set_slice_int(width, x, low, get_slice_int(width, v, 0))


// Although one would expect rflags to be 64 bits, the ACL2 models
// it as 32 bits
register rflags : bits(32)

val r_rflags : int -> int effect {rreg}
function r_rflags (x86_dummy) = unsigned(rflags)

val write_rflags : (int, int) -> int effect {wreg}
function write_rflags (toSet, x86_dummy) = {
	rflags = get_slice_int(32, toSet, 0);
	0
}

val genericBitstructAccessor : (int, int, int) -> int
function genericBitstructAccessor (width, inputBits, low) =
	unsigned(get_slice_int(width, inputBits, low))

val genericBitstructUpdater : (int, int, int, int) -> int
function genericBitstructUpdater (width, spliceBits, low, inputBits) =
	set_slice_int(width, inputBits, low, get_slice_int(width, spliceBits, 0))

// Registers
register rip : bits(64)
val ripi : int -> int effect {rreg}
function ripi (x86_dummy) = unsigned(rip)

val write_rip : (int, int) -> int effect {wreg}
function write_rip (toSet, x86_dummy) = {
	rip = get_slice_int(64, toSet, 0);
	0
}

// See here for some hints on rgfi:
// http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____GPRS-READS-AND-WRITES?path=3532/29063/10660/6710/25183

register gprs : vector(16, dec, bits(64)) // 16 gprs in 64 bit mode, as per table 5.1 in thesis

val rgfi : (int, int) -> int effect {escape, rreg}
function rgfi (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 16);
	signed(gprs[regNum])
}

val write_rgfi : (int, int, int) -> int effect {escape, wreg}
function write_rgfi (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 16);
	gprs[regNum] = get_slice_int(64, v, 0);
	0
}

/*
Model Specific Registers

Footnote b in section 5.1 of Goel thesis says only 6 MSR registers
(I think they mean 7) are thus far defined in the model.  For now,
we enumerate some of them explicitely here.  Fortuantely, the indexes
range from 0..6, and we can lump them into a vector of registers

TODO: make this enumeration automatic
*/
register msrs : vector(7, dec, bits(64)) // 64 bits as per table 5.1 in thesis

val msri : (int, int) -> int effect {escape, rreg}
function msri (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 7);
	unsigned(msrs[regNum]) // Not sure whether this should be signed or unsigned
}

val write_msri : (int, int, int) -> int effect {escape, wreg}
function write_msri(regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 7);
	msrs[regNum] = get_slice_int(64, v, 0);
	0
}

/*
seg-visible registers etc..  From `concrete-state.lisp` we find
there are 6 x 16bit registers

TODO: make this automatic (including the number 6, which is a global
constant but subject to change)

TODO: in `xr` in ACL2 we have things like seg_hidden_attri* (i.e.
with a trailing asterisk). Why is this?
*/
register seg_visibles : vector(6, dec, bits(16))
val seg_visiblei : (int, int) -> int effect {escape, rreg}
function seg_visiblei (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	unsigned(seg_visibles[regNum])
}

val write_seg_visiblei : (int, int, int) -> int effect {escape, wreg}
function write_seg_visiblei (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	seg_visibles[regNum] = get_slice_int(16, v, 0);
	0
}

register seg_hidden_attrs : vector(6, dec, bits(16))
val seg_hidden_attri : (int, int) -> int effect {escape, rreg}
function seg_hidden_attri (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	unsigned(seg_hidden_attrs[regNum])
}

val write_seg_hidden_attri : (int, int, int) -> int effect {escape, wreg}
function write_seg_hidden_attri (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	seg_hidden_attrs[regNum] = get_slice_int(16, v, 0);
	0
}


register seg_hidden_bases : vector(6, dec, bits(64))
val seg_hidden_basei : (int, int) -> int effect {escape, rreg}
function seg_hidden_basei (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	unsigned(seg_hidden_bases[regNum])
}

val write_seg_hidden_basei : (int, int, int) -> int effect {escape, wreg}
function write_seg_hidden_basei (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	seg_hidden_bases[regNum] = get_slice_int(64, v, 0);
	0
}

register seg_hidden_limits : vector(6, dec, bits(32))
val seg_hidden_limiti : (int, int) -> int effect {escape, rreg}
function seg_hidden_limiti (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	unsigned(seg_hidden_limits[regNum])
}

val write_seg_hidden_limiti : (int, int, int) -> int effect {escape, wreg}
function write_seg_hidden_limiti (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 6);
	seg_hidden_limits[regNum] = get_slice_int(32, v, 0);
	0
}


/*
zmm registers.  From `concrete-state.lisp` we find there are
32 x 512bit registers

TODO: make this automatic (including the number 32, which is a global
constant but subject to change)
*/
register zmms : vector (32, dec, bits(512))
val zmmi : (int, int) -> int effect {escape, rreg}
function zmmi (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 32);
	unsigned(zmms[regNum])
}

val write_zmmi : (int, int, int) -> int effect {escape, wreg}
function write_zmmi (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 32);
	zmms[regNum] = get_slice_int(512, v, 0);
	0
}

/*
Control registers.  From `concrete-state.lisp` we find there are
17 x 64bit registers

TODO: make this automatic (including the number 17, which is a global
constant but subject to change)
*/
register ctrs : vector (17, dec, bits(64))
val ctri : (int, int) -> int effect {escape, rreg}
function ctri (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 17);
	unsigned(ctrs[regNum])
}

/*
We can ise `:pe ...` from `stri` and find we need the following.
*/
register strs : vector(2, dec, bits(80))
val stri : (int, int) -> int effect {escape, rreg}
function stri (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	unsigned(strs[regNum])
}

val write_stri : (int, int, int) -> int effect {escape, wreg}
function write_stri (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	strs[regNum] = get_slice_int(80, v, 0);
	0
}

/*
ssr
*/
register ssr_visibles : vector(2, dec, bits(16))
val ssr_visiblei : (int, int) -> int effect {escape, rreg}
function ssr_visiblei (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	unsigned(ssr_visibles[regNum])
}

val write_ssr_visiblei : (int, int, int) -> int effect {escape, wreg}
function write_ssr_visiblei (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	ssr_visibles[regNum] = get_slice_int(16, v, 0);
	0
}

register ssr_hidden_bases : vector(2, dec, bits(64))
val ssr_hidden_basei : (int, int) -> int effect {escape, rreg}
function ssr_hidden_basei (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	unsigned(ssr_hidden_bases[regNum])
}

val write_ssr_hidden_basei : (int, int, int) -> int effect {escape, wreg}
function write_ssr_hidden_basei (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	ssr_hidden_bases[regNum] = get_slice_int(64, v, 0);
	0
}

register ssr_hidden_limits : vector(2, dec, bits(32))
val ssr_hidden_limiti : (int, int) -> int effect {escape, rreg}
function ssr_hidden_limiti (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	unsigned(ssr_hidden_bases[regNum])
}

val write_ssr_hidden_limiti : (int, int, int) -> int effect {escape, wreg}
function write_ssr_hidden_limiti (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	ssr_hidden_limits[regNum] = get_slice_int(32, v, 0);
	0
}

register ssr_hidden_attrs : vector(2, dec, bits(16))
val ssr_hidden_attri : (int, int) -> int effect {escape, rreg}
function ssr_hidden_attri (regNum, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	unsigned(ssr_hidden_bases[regNum])
}

val write_ssr_hidden_attri : (int, int, int) -> int effect {escape, wreg}
function write_ssr_hidden_attri (regNum, v, x86_dummy) = {
	assert(0 <= regNum & regNum < 2);
	ssr_hidden_attrs[regNum] = get_slice_int(16, v, 0);
	0
}

/*
Reading from and writing to memory
*/
val memi : (int, int) -> int effect {rmem, rreg}
function memi (addr, x86_dummy) = {
	result = unsigned(
		__read_mem( Read_plain,					// Read type
					64,							// Address size
					get_slice_int(64, addr, 0),	// Address to read
					1							// Number of bytes to read
				)
			);
	print_endline(	dec_str(ripi(0)) ^
			": R " ^
			dec_str(addr) ^ " " ^
			dec_str(1) ^ " " ^
			dec_str(result));
	result
}

val bang_memi : (int, int, int) -> int effect {eamem, wmv, rreg}
function bang_memi (addr, valToWrite, x86_dummy) = {
	dummy = 
		__write_mem_ea(	Write_plain,				// Write type
						64,							// Address size
						get_slice_int(64, addr, 0),	// Address to write to (0 here)
						1);							// Number of bytes to write
	dummy2 =
		__write_mem(Write_plain,					// Write type
					64,								// Address size
					get_slice_int(64, addr, 0),		// Address to write to (0 here)
					1,								// Number of bytes to write
					get_slice_int(8, valToWrite, 0)	// Value to write
					);
	print_endline(	dec_str(ripi(0)) ^
			": W " ^
			dec_str(addr) ^ " " ^
			dec_str(1) ^ " " ^
			dec_str(valToWrite));
	0
}

/*-----------------------------------------------------------------*/
// Config
/*-----------------------------------------------------------------*/

// app-view is part of the x86 model state.  See thesis section 5.1
// and conrete-memory.lisp

val app_view : int -> bool
function app_view (x86_dummy) = true

// Model state.
register ms_reg : bool

val ms : int -> bool effect {rreg}
function ms x86_dummy = ms_reg

// Fault.
register fault_reg : bool

val fault : int -> bool effect {rreg}
function fault x86_dummy = fault_reg

/*-----------------------------------------------------------------*/
// Untranslated functions
/*-----------------------------------------------------------------*/


val unimplemented_vex_decode_and_execute : (int, int, int, int, int, int, int) -> int effect {escape}
val unimplemented_evex_decode_and_execute : (int, int, int, int, int, int, int) -> int effect {escape}

// ---

function unimplemented_vex_decode_and_execute (_, _, _, _, _, _, _) = throw(Emsg("Error: vex decoding not implemented"))
function unimplemented_evex_decode_and_execute (_, _, _, _, _, _, _) = throw(Emsg("Error: evex decoding not implemented"))
