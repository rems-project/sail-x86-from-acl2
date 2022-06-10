/*-----------------------------------------------------------------*/
// This file contains the handwritten functions for the ACL2
// translation.
/*-----------------------------------------------------------------*/

$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <regfp.sail>
$include <real.sail>
$include <string.sail>

overload operator ^ = {concat_str, and_bool}

// Type synonym for signed bitvectors, which are distinguished from unsigned
// bitvectors by ACL2 (but not by Sail)

type sbits ('n : Int), 'n > 0 = bits('n)

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

val feature_flag_fn : string -> int
function feature_flag_fn (feature) = 1


val feature_flags_fn : list(string) -> int
function feature_flags_fn (features) =
	match features {
		[||] => 1,
		x :: xs =>	if feature_flag_fn(x) == 0
					then 0
					else feature_flags_fn(xs)
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

val undef_read_logic : unit -> nat effect {undef}
function undef_read_logic () = undefined

/*-----------------------------------------------------------------*/
// Utility functions
/*-----------------------------------------------------------------*/

val eq_bits_nat : forall 'n, 'n > 0. (bits('n), nat) -> bool
function eq_bits_nat (x, y) = (unsigned(x) == y)

overload operator == = {eq_bits_nat}

/*
pow2 taken from sail-riscv/model/prelude.sail
*/

val pow2 = "pow2" : forall 'n. int('n) -> int(2 ^ 'n)

/*
TODO: is there a utility function for this
TODO: make bool_to_bit and bit_to_bool a mapping?
*/
val bool_to_bit : bool -> bits(1)
function bool_to_bit (b) = match b {
    true  => 0b1,
    false => 0b0
}

val bit_to_bool : bits(1) -> bool
function bit_to_bool (b) = match b {
    0b1 => true,
    0b0 => false
}

// TODO: The original trunc function in x86isa/utils/utilities.lisp
// seems to intrepret n as the number of bytes in some cases, and
// the number of bits in others... although it seems to be used with
// numbers of bytes as far as I can see, so use that...
val trunc : forall 'n 'm, 'n in {1, 2, 4, 8, 16} & 'm > 0. (int('n), bits('m)) -> bits(8 * 'n)
function trunc (n, x) = sail_mask(8 * n, x)

val the_int : (int) -> int
function the_int i = i

val the_nat : (int) -> nat effect {escape}
function the_nat i = {
	assert (i >= 0);
	i
}

val the_range : forall 'n 'm 'i, 'n <= 'm. (int('n), int('m), int('i)) -> {'j, 'n <= 'j & 'j <= 'm & 'i == 'j. int('j)} effect {escape}
function the_range(n, m, i) = {
	assert(n <= i & i <= m);
	i
}

val bits_of_bits : forall 'n 'm, 'n >= 0 & 'm >= 0. (int('n), bits('m)) -> bits('n)
function bits_of_bits (n, x) = sail_mask(n, x)

val bits_of_int : forall 'n, 'n >= 0. (int('n), int) -> bits('n)
function bits_of_int (n, x) = get_slice_int(n, x, 0)

overload the_bits = {bits_of_bits, bits_of_int}

val sbits_of_bits : forall 'n 'm, 'n > 0 & 'm >= 0. (int('n), bits('m)) -> bits('n)
function sbits_of_bits (n, x) = if (n <= length(x)) then truncate(x, n) else sail_sign_extend(x, n)

overload the_sbits = {sbits_of_bits, bits_of_int}

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
val unsigned_byte_p_int : (int, int) -> bool effect {escape}
function unsigned_byte_p_int (bits, x) = {
	assert(bits >= 0 & x >= 0);
	x < pow2(bits)
}

val unsigned_byte_p_bits : forall 'n, 'n >= 0. (int, bits('n)) -> bool
function unsigned_byte_p_bits (bits, x) = unsigned_byte_p_int(bits, unsigned(x))

overload unsigned_byte_p = {unsigned_byte_p_int, unsigned_byte_p_bits}

/*
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____SIGNED-BYTE-P
*/
val signed_byte_p_int : (int, int) -> bool effect {escape}
function signed_byte_p_int (bits, x) = {
	assert(bits >= 0);
	negate(pow2(bits - 1)) <= x & x < pow2(bits - 1)
}

val signed_byte_p_bits : forall 'n, 'n > 0. (int, bits('n)) -> bool
function signed_byte_p_bits (bits, x) = signed_byte_p_int(bits, signed(x))

overload signed_byte_p = {signed_byte_p_int, signed_byte_p_bits}

/*-----------------------------------------------------------------*/
// Logic functions
/*-----------------------------------------------------------------*/

/*
`loghead size i` returns size low-order bits of i.
In C-like languages: `i & ((1 << size) - 1)`
If size = 0, then return = 0

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____LOGHEAD
*/

val loghead_int : (int, int) -> int
function loghead_int (size, i) =
	if size == 0
	then 0
	else
		unsigned(
			get_slice_int(size, i, 0)
		)

val loghead_bits : forall 'n 'm, 'n >= 0 & 'm >= 0. (int('n), bits('m)) -> bits('n)
function loghead_bits (size, x) = sail_mask(size, x)

overload loghead = {loghead_bits, loghead_int}

/*-----------------------------------------------------------------*/
/*
`logtail pos i` returns high-order part of i starting at bit pos

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____LOGTAIL
*/
val logtail_int : (int, int) -> int
function logtail_int (pos, x) =
	shr_int(x, pos)

val logtail_bits : forall 'pos 'n, 'pos >= 0 & 'pos < 'n. (int('pos), bits('n)) -> bits('n - 'pos)
function logtail_bits (pos, x) = x[('n - 1) .. pos]

overload logtail = {logtail_bits, logtail_int}

/*-----------------------------------------------------------------*/
/*
"For a nonnegative integer i and an integer j, (logbitp i j) is a
Boolean, which is t if and only if the value of the ith bit is 1 in
the two's complement representation of j.

(Logbitp i j) has a guard that i is a nonnegative integer and j is
an integer."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGBITP
*/
val logbitp_int : (int, int) -> bool effect {escape}
function logbitp_int (i, j) = {
	assert(i >= 0);
    bit_to_bool(
        get_slice_int(1, j, i)
    )
}

val logbitp_bits : forall 'n 'm, 'n >= 0 & 'm >= 0. (int('n), bits('m)) -> bool
function logbitp_bits (n, j) = if (n < 'm) then (j[n] == bitone) else false

overload logbitp = {logbitp_bits, logbitp_int}

/*
Also `logbit` which "is just like the Common Lisp function
`(logbitppos i)`, except that we return 0b1 or 0b0 (instead of t or
nil)."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGBIT
*/
val logbit_int : (int, int) -> bits(1) effect {escape}
function logbit_int (pos, j) = {
	assert(pos >= 0);
	get_slice_int(1, j, pos)
}

val logbit_bits : forall 'n 'm, 0 <= 'm < 'n. (int('m), bits('n)) -> bits(1)
function logbit_bits (pos, x) = [x[pos]]

overload logbit = {logbit_bits, logbit_int}

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

val lognot_int : (int) -> int
function lognot_int x = negate(x) - 1

overload lognot = {not_vec, lognot_int}

val logcount_int : (int) -> int effect {escape}
function logcount_int(x) =
	if x == 0 then 0 else
	if x < 0  then logcount_int(lognot(x)) else
	if isEven (x)
		then logcount_int (shr_int (x, 1))
		else logcount_int (shr_int (x, 1)) + 1

val logcount_bits : forall 'n, 'n >= 0. (bits('n)) -> int
function logcount_bits (x) = logcount_int(unsigned(x))

overload logcount = {logcount_bits, logcount_int}

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

val logand_int : (int, int) -> int
function logand_int (i, j) =
	if i == 0 then 0 else
	if j == 0 then 0 else
	if i == -1 then j else
	if j == -1 then i else
	let x = 2 * (logand_int (floor2 (i, 2), (floor2 (j, 2)))) in
	x + (	if isEven (i) then 0 else
			if isEven (j) then 0 else
			1)

overload logand = {and_vec, logand_int}

/*-----------------------------------------------------------------*/
/*
logior

Here only the binary version is implemented.  It's basically a
copy/paste job from the docs by this stage.

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____LOGIOR
*/
val logior_int : (int, int) -> int
function logior_int (i, j) =
	lognot(logand(lognot(i), lognot(j)))

overload logior = {or_vec, logior_int}

/*-----------------------------------------------------------------*/
/*
logxor

Here only the binary version is implemented.

See:
	logxor: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGXOR
	logeqv: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGEQV
	logorc1:http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____LOGORC1
*/
val logxor : forall 'n, 'n >= 0. (bits('n), bits('n)) -> bits('n)
function logxor (x, y) = xor_vec(x, y)

/*-----------------------------------------------------------------*/
/*
logext

See:
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGEXT
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGAPP
*/
/* TODO */
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
overload n_size = {bits_of_bits, bits_of_int}

/*-----------------------------------------------------------------*/
/*
ash

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____ASH

TODO: check this implementation matches the spec
*/
val ash_bits : forall 'n, 'n >= 0. (bits('n), int) -> bits('n)
function ash_bits (i, c) =
	if c == 0 then i else
	if c > 0 then	sail_shiftleft(i, c)
	else			sail_arith_shiftright(i, negate(c))

val ash_left_int : forall 'x 'n, 'n >= 0. (int('x), int('n)) -> int('x * (2 ^ 'n))
function ash_left_int (x, n) = x * pow2(n)

val ash_int : (int, int) -> int
function ash_int (i, c) =
	if c == 0 then i else
	if c > 0 then	_shl_int(i, c)
	else			_shr_int(i, negate(c))

overload ash = {ash_bits, ash_left_int, ash_int}

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

TODO: Autogenerate?
*/
val merge_2_u64s : (bits(64), bits(64)) -> bits(128)
function merge_2_u64s (a1, a0) = a1 @ a0

val merge_2_u32s : (bits(32), bits(32)) -> bits(64)
function merge_2_u32s (a1, a0) = a1 @ a0

val merge_4_u32s : (bits(32), bits(32), bits(32), bits(32)) -> bits(128)
function merge_4_u32s (a3, a2, a1, a0) = a3 @ a2 @ a1 @ a0

/*-----------------------------------------------------------------*/
// Some state
/*-----------------------------------------------------------------*/

infix 7 <<
overload operator << = {sail_shiftleft}

// Used in part-install.  See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-INSTALL
// TODO: this is almost identical to genericBitstructUpdater below - maybe merge them

val changeSlice_int : (int, int, int, int) -> int
function changeSlice_int (x, low, width, v) =
	set_slice_int(width, x, low, get_slice_int(width, v, 0))

val changeSlice_bits : forall 'n 'low 'width, 'low >= 0 & 'width > 0 & 'low + 'width <= 'n.
  (bits('n), int('low), int('width), bits('width)) -> bits('n)
function changeSlice_bits (x, low, width, v) = [x with (low + width - 1) .. low = v]

overload changeSlice = {changeSlice_bits, changeSlice_int}

// Although one would expect rflags to be 64 bits, the ACL2 models
// it as 32 bits
register rflags : bits(32)

val genericBitstructAccessor : forall 'width 'n 'low, 'width > 0 & 'low >= 0 & 'low + 'width <= 'n.
  (int('width), bits('n), int('low)) -> bits('width)
function genericBitstructAccessor (width, inputBits, low) =
	inputBits[(low + width - 1) .. low]

val genericBitstructUpdater : forall 'width 'n 'low, 'width > 0 & 'low >= 0 & 'low + 'width <= 'n.
  (int('width), bits('width), int('low), bits('n)) -> bits('n)
function genericBitstructUpdater (width, spliceBits, low, inputBits) =
        [inputBits with (low + width - 1) .. low = spliceBits]

// Registers
register rip : bits(64)

// In ACL2 rgfi() is often called with a `defconst` argument.  E.g.
//      ` (rgfi *rax*) `
// Unfortunately the translator currently translates these as number literals.
// So (ignoring the dummy x86 variable) the above will become:
//      ` rgfi(0)`
// To find the number *rax* maps to, one can simply type `*rax*` into and ACL2
// terminal with the x86 ISA project loaded.
//
// Each of the gprs is defined separately below, as opposed to a vector of
// bitvectors as other registers are.  This is so they play nicely with
// concurrency tools.  It also means rgfi() and write_rgfi() must work out
// which reg to access themselves, instead of indexing an array.
//
// See here for some hints on rgfi:
// http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____GPRS-READS-AND-WRITES?path=3532/29063/10660/6710/25183

register rax : bits(64)
register rbx : bits(64)
register rcx : bits(64)
register rdx : bits(64)
register rsi : bits(64)
register rdi : bits(64)
register rsp : bits(64)
register rbp : bits(64)
register r8 : bits(64)
register r9 : bits(64)
register r10 : bits(64)
register r11 : bits(64)
register r12 : bits(64)
register r13 : bits(64)
register r14 : bits(64)
register r15 : bits(64)

val rgfi : (int) -> bits(64) effect {escape, rreg}
function rgfi (regNum) = {
	assert(0 <= regNum & regNum < 16);
    match regNum {
        0 => rax,
        1 => rcx,
        2 => rdx,
        3 => rbx,
        4 => rsp,
        5 => rbp,
        6 => rsi,
        7 => rdi,
        8 => r8,
        9 => r9,
        10 => r10,
        11 => r11,
        12 => r12,
        13 => r13,
        14 => r14,
        15 => r15,
        _ =>  throw(Emsg("Invalid regNum in rfgi"))
    }
}

val write_rgfi : (int, bits(64)) -> unit effect {escape, wreg}
function write_rgfi (regNum, val_to_write) = {
	assert(0 <= regNum & regNum < 16);
	match regNum {
            0 => rax = val_to_write,
            1 => rcx = val_to_write,
            2 => rdx = val_to_write,
            3 => rbx = val_to_write,
            4 => rsp = val_to_write,
            5 => rbp = val_to_write,
            6 => rsi = val_to_write,
            7 => rdi = val_to_write,
            8 => r8 = val_to_write,
            9 => r9 = val_to_write,
            10 => r10 = val_to_write,
            11 => r11 = val_to_write,
            12 => r12 = val_to_write,
            13 => r13 = val_to_write,
            14 => r14 = val_to_write,
            15 => r15 = val_to_write,
            _ =>  throw(Emsg("Invalid regNum in write_rfgi"))
        }
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

/*
seg-visible registers etc..  From `concrete-state.lisp` we find
there are 6 x 16bit registers

TODO: make this automatic (including the number 6, which is a global
constant but subject to change)

TODO: in `xr` in ACL2 we have things like seg_hidden_attri* (i.e.
with a trailing asterisk). Why is this?
*/
register seg_visibles : vector(6, dec, bits(16))

register seg_hidden_attrs : vector(6, dec, bits(16))

register seg_hidden_bases : vector(6, dec, bits(64))

register seg_hidden_limits : vector(6, dec, bits(32))

/*
zmm registers.  From `concrete-state.lisp` we find there are
32 x 512bit registers

TODO: make this automatic (including the number 32, which is a global
constant but subject to change)
*/
register zmms : vector (32, dec, bits(512))

/*
Control registers.  From `concrete-state.lisp` we find there are
17 x 64bit registers

TODO: make this automatic (including the number 17, which is a global
constant but subject to change)
*/
register ctrs : vector (17, dec, bits(64))

/*
We can use `:pe ...` from `stri` and find we need the following.
*/
register strs : vector(2, dec, bits(80))

/*
ssr
*/
register ssr_visibles : vector(2, dec, bits(16))

register ssr_hidden_bases : vector(2, dec, bits(64))

register ssr_hidden_limits : vector(2, dec, bits(32))

register ssr_hidden_attrs : vector(2, dec, bits(16))

/*
Reading from and writing to memory
*/
val memi : forall 'addr_width, 'addr_width >= 0. (bits('addr_width)) -> bits(8) effect {rmem, rreg}
function memi (addr) = {
	result = __read_mem( Read_plain,					// Read type
					64,							// Address size
					sail_mask(64, addr),	// Address to read
					1							// Number of bytes to read
				);
	print_endline(	bits_str(rip) ^
			": R " ^
			bits_str(addr) ^ " " ^
			dec_str(1) ^ " " ^
			bits_str(result));
	result
}

val bang_memi : forall 'addr_width, 'addr_width >= 0. (bits('addr_width), bits(8)) -> unit effect {eamem, wmv, rreg}
function bang_memi (addr, valToWrite) = {
	dummy = 
		__write_mem_ea(	Write_plain,				// Write type
						64,							// Address size
						sail_mask(64, addr),	// Address to write to (0 here)
						1);							// Number of bytes to write
	dummy2 =
		__write_mem(Write_plain,					// Write type
					64,								// Address size
					sail_mask(64, addr),		// Address to write to (0 here)
					1,								// Number of bytes to write
					valToWrite	// Value to write
					);
	print_endline(	bits_str(rip) ^
			": W " ^
			bits_str(addr) ^ " " ^
			dec_str(1) ^ " " ^
			bits_str(valToWrite))
}

/*-----------------------------------------------------------------*/
// Config
/*-----------------------------------------------------------------*/

// app-view is part of the x86 model state.  See thesis section 5.1
// and conrete-memory.lisp

val app_view : unit -> bool
function app_view () = true

// Model state.
register ms_reg : bool

val ms : unit -> bool effect {rreg}
function ms () = ms_reg

// Fault.
register fault_reg : bool

val fault : unit -> bool effect {rreg}
function fault () = fault_reg

/*-----------------------------------------------------------------*/
// Untranslated functions
/*-----------------------------------------------------------------*/


val unimplemented_vex_decode_and_execute : (int, int, int, int, int, int) -> unit effect {escape}
val unimplemented_evex_decode_and_execute : (int, int, int, int, int, int) -> unit effect {escape}

// ---

function unimplemented_vex_decode_and_execute (_, _, _, _, _, _) = throw(Emsg("Error: vex decoding not implemented"))
function unimplemented_evex_decode_and_execute (_, _, _, _, _, _) = throw(Emsg("Error: evex decoding not implemented"))
