/*-----------------------------------------------------------------*/
// This file contains the handwritten functions for the ACL2
// translation.
/*-----------------------------------------------------------------*/

$ifndef __X86_PRELUDE
$define __X86_PRELUDE

$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <regfp.sail>
$include <string.sail>

overload operator + = {concat_str, and_bool}

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
	Emsg : string,
	Syscall : unit /* TODO: Pass data? */
}

val x86_model_error : forall ('a : Type). string -> 'a
function x86_model_error(msg) = throw(Emsg("Model error: " + msg))

val x86_fault : forall ('a : Type). string -> 'a
function x86_fault(msg) = throw(Emsg("Fault: " + msg))


/*-----------------------------------------------------------------*/
// Undefined behaviour
//
// In the ACL2 model, undefined values for things like flags just
// return zero.  Here we use Sail's builtin `undefined`, which
// actually does the same.
/*-----------------------------------------------------------------*/

val undef_read_logic : unit -> nat
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

val pow2 = pure "pow2" : forall 'n. int('n) -> int(2 ^ 'n)

/*
TODO: is there a utility function for this
TODO: make bool_to_bit and bit_to_bool a mapping?
*/
val bool_to_bits : bool -> bits(1)
function bool_to_bits (b) = match b {
    true  => 0b1,
    false => 0b0
}

val bits_to_bool : bits(1) -> bool
function bits_to_bool (b) = match b {
    0b1 => true,
    0b0 => false
}

val sail_mask_signed : forall 'len 'v, 'len >= 0 & 'v >= 0. (int('len), bits('v)) -> bits('len)

function sail_mask_signed(len, v) = if len <= length(v) then truncate(v, len) else sail_sign_extend(v, len)

// TODO: The original trunc function in x86isa/utils/utilities.lisp
// seems to intrepret n as the number of bytes in some cases, and
// the number of bits in others... although it seems to be used with
// numbers of bytes as far as I can see, so use that...
val trunc : forall 'n 'm, 'n in {1, 2, 4, 8, 16} & 'm > 0. (int('n), bits('m)) -> bits(8 * 'n)
function trunc (n, x) = sail_mask(8 * n, x)

val trunc_signed : forall 'n 'm, 'n in {1, 2, 4, 8, 16} & 'm > 0. (int('n), bits('m)) -> bits(8 * 'n)
function trunc_signed (n, x) = sail_mask_signed(8 * n, x)

// Bit slicing functions for translating part_select.  This may be called with
// indices that go beyond the length of the bitvector, so handle the case where
// we have to zero-extend or sign-extend, depending on whether the original
// bitvector is treated as signed or not.
val bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)
function bitslice (x, low, width) = {
	assert('n >= 0 & low >= 0 & width > 0);
	if (low + width <= 'n) then {
		x[low + width - 1 .. low]
	} else if (low < 'n - 1) then {
		sail_zeros(low + width - 'n) @ x['n - 1 .. low]
	} else {
		sail_zeros(width)
	}
}

// Signed bitslicing function that sign-extends rather than zero-extends in
// case the slice goes out of bounds
val signed_bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)
function signed_bitslice (x, low, width) = {
	assert('n >= 0 & low >= 0 & width > 0);
	if (low + width <= 'n) then {
		x[low + width - 1 .. low]
	} else if (low < 'n - 1) then {
		// Sign-extend by replicating the MSB
		replicate_bits([x['n - 1]], low + width - 'n) @ x['n - 1 .. low]
	} else {
		sail_zeros(width)
	}
}

val nat_of_int : (int) -> nat
function nat_of_int i = {
	assert (i >= 0);
	i
}

val nfix : int -> nat
function nfix i = if i >= 0 then i else 0

val check_range : forall 'n 'm 'i, 'n <= 'm. (int('n), int('m), int('i)) -> {'j, 'n <= 'j & 'j <= 'm & 'i == 'j. int('j)}
function check_range(n, m, i) = {
	assert(n <= i & i <= m);
	i
}

val bits_of_int : forall 'n, 'n >= 0. (int, int('n)) -> bits('n)
function bits_of_int (x, n) = get_slice_int(n, x, 0)

val fits_in_bitvector : (int, int) -> bool
function fits_in_bitvector (n, x) = (0 <= x & x < pow2(n))

val fits_in_signed_bitvector : (int, int) -> bool
function fits_in_signed_bitvector (n, x) = (negate(pow2(n - 1)) <= x & x < pow2(n - 1))

val isEven : int -> bool
function isEven x =
       if get_slice_int(1, x, 0) == 0b0 then true else false

// b-xor: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____B-XOR
val b_xor : (int, int) -> int
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
val unsigned_byte_p_int : (int, int) -> bool
function unsigned_byte_p_int (bits, x) = {
	assert(bits >= 0);
	0 <= x & x < pow2(bits)
}

val unsigned_byte_p_bits : forall 'n, 'n >= 0. (int, bits('n)) -> bool
function unsigned_byte_p_bits (bits, x) = unsigned_byte_p_int(bits, unsigned(x))

overload unsigned_byte_p = {unsigned_byte_p_int, unsigned_byte_p_bits}

/*
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____SIGNED-BYTE-P
*/
val signed_byte_p_int : (int, int) -> bool
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
val logbitp_int : (int, int) -> bool
function logbitp_int (i, j) = {
	assert(i >= 0);
    bits_to_bool(
        get_slice_int(1, j, i)
    )
}

val logbitp_bits : forall 'n 'pos, 'n >= 0. (int('pos), bits('n)) -> bool
function logbitp_bits (pos, x) = if (0 <= pos & pos < 'n) then (x[pos] == bitone) else false

overload logbitp = {logbitp_bits, logbitp_int}

/*
Also `logbit` which "is just like the Common Lisp function
`(logbitppos i)`, except that we return 0b1 or 0b0 (instead of t or
nil)."

See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____LOGBIT
*/
val logbit_int : (int, int) -> bits(1)
function logbit_int (pos, j) = {
	assert(pos >= 0);
	get_slice_int(1, j, pos)
}

val logbit_bits : forall 'n 'pos, 'n >= 0. (int('pos), bits('n)) -> bits(1)
function logbit_bits (pos, x) = if (0 <= pos & pos < 'n) then [x[pos]] else [bitzero]

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

val logcount_int : (int) -> int
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
	if ((j > 0) & (tmod >= 0)) | ((j < 0) & (tmod <= 0))
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

val binary_logext : (int, int) -> int
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
val rotate_left_bits : forall 'n. (bits('n), int, int) -> bits('n)
function rotate_left_bits (x, width, places) = {
  let high = sail_shiftleft(x, places);
  let low = sail_shiftright(x, width-places);
  high | low
}

val rotate_left_int : (int, int, int) -> int
function rotate_left_int (x, width, places) = {
	assert(0 <= places);
	placesMod = mod(places, width);
	low_num = width - placesMod;
	xl = get_slice_int(low_num, x, 0);
	xh = get_slice_int(placesMod, x, low_num);
	unsigned(xl @ xh)
}
overload rotate_left = {rotate_left_bits, rotate_left_int}

val rotate_right_bits : forall 'n. (bits('n), int, int) -> bits('n)
function rotate_right_bits (x, width, places) = {
  let low = sail_shiftright(x, places);
  let high = sail_shiftleft(x, width-places);
  high | low
}

val rotate_right_int : (int, int, int) -> int
function rotate_right_int (x, width, places) = {
	assert(0 <= places);
	placesMod = mod(places, width);
	xl = get_slice_int(placesMod, x, 0);
	xh = get_slice_int(width - placesMod, x, placesMod);
	unsigned(xl @ xh)
}
overload rotate_right = {rotate_right_bits, rotate_right_int}

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
// Some bitvector operations
/*-----------------------------------------------------------------*/

infix 7 <<
overload operator << = {sail_shiftleft}

infix 7 >>
overload operator >> = {sail_shiftright}

// Used in part-install.  See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-INSTALL
// TODO: this is almost identical to genericBitstructUpdater below - maybe merge them

val changeSlice_int : (int, int, int, int) -> int
function changeSlice_int (x, low, width, v) =
	set_slice_int(width, x, low, get_slice_int(width, v, 0))

val changeSlice_bits : forall 'n 'low 'width, 'low >= 0 & 'width > 0 & 'low + 'width <= 'n.
  (bits('n), int('low), int('width), bits('width)) -> bits('n)
function changeSlice_bits (x, low, width, v) = [x with (low + width - 1) .. low = v]

overload changeSlice = {changeSlice_bits, changeSlice_int}

val genericBitstructAccessor : forall 'width 'n 'low, 'width > 0 & 'low >= 0 & 'low + 'width <= 'n.
  (int('width), bits('n), int('low)) -> bits('width)
function genericBitstructAccessor (width, inputBits, low) =
	inputBits[(low + width - 1) .. low]

val genericBitstructUpdater : forall 'width 'n 'low, 'width > 0 & 'low >= 0 & 'low + 'width <= 'n.
  (int('width), bits('width), int('low), bits('n)) -> bits('n)
function genericBitstructUpdater (width, spliceBits, low, inputBits) =
        [inputBits with (low + width - 1) .. low = spliceBits]

/*
Reading from and writing to memory
*/

val log_memory_read : forall 'addr_width 'n, 'addr_width > 0 & 'n > 0. (bits('addr_width), int('n), bits(8 * 'n)) -> unit

// TODO: Rename these
val memi : forall 'addr_width, 'addr_width > 0. (bits('addr_width)) -> bits(8)
function memi (addr) = {
	result = __read_mem( Read_plain,					// Read type
					64,							// Address size
					sail_mask(64, addr),	// Address to read
					1							// Number of bytes to read
				);
	log_memory_read(addr, 1, result);
	result
}

val log_memory_write : forall 'addr_width 'n, 'addr_width > 0 & 'n > 0. (bits('addr_width), int('n), bits(8 * 'n)) -> unit

val bang_memi : forall 'addr_width, 'addr_width > 0. (bits('addr_width), bits(8)) -> unit
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
	log_memory_write(addr, 1, valToWrite);
}

/*-----------------------------------------------------------------*/
// Config
/*-----------------------------------------------------------------*/

val __SetConfig : (string, int) -> unit
scattered function __SetConfig

// app-view is part of the x86 model state.  See thesis section 5.1
// and conrete-memory.lisp

register app_view : bool
register marking_view : bool

/* Register logging helpers */
register log_register_writes : bool

val log_gpr_write : range(0, 15) -> unit
val log_rflags_write : unit -> unit
val log_rip_write : unit -> unit

// Model state.
register ms_reg : bool

val ms : unit -> bool
function ms () = ms_reg

// Fault.
register fault_reg : bool

val fault : unit -> bool
function fault () = fault_reg

/* Handwritten implementations of `rb` and `wb` in a more idiomatic style,
 * using for-loops instead of working with lists of addresses and bytes, like
 * the ACL2 implementations.
 */

/* Address translation function and another helper function used by `rb`/`wb` */
val ia32e_la_to_pa : (sbits(48), string) -> bits(52)
val canonical_address_p : int -> bool
val n64_bit_modep : unit -> bool
val observe_mem_write : (sbits(48), int) -> unit

val rb : forall 'addr_width 'n, 'addr_width > 0 & 'n > 0. (int('n), bits('addr_width), string) -> bits(8 * 'n)

function rb (n, addr, access_kind) = {
  result : bits(8 * 'n) = sail_zeros(8 * n);
  foreach (offset from 0 to (n - 1)) {
    if not_bool(canonical_address_p(signed(addr) + offset)) then x86_model_error("rb: non-canonical address");
    let lin_addr : bits(48) = add_bits_int(sail_mask_signed(48, addr), offset);
    let phys_addr : bits(52) =
      if app_view then sail_mask_signed(52, lin_addr) else ia32e_la_to_pa(lin_addr, access_kind)
    in
    result[((8 * offset) + 7) .. (8 * offset)] = memi(phys_addr)
  };
  return result
}

val wb : forall 'addr_width 'n, 'addr_width > 0 & 'n > 0. (int('n), bits('addr_width), string, bits(8 * 'n)) -> unit

function wb (n, addr, access_kind, value) = {
  foreach (offset from 0 to (n - 1)) {
    if not_bool(canonical_address_p(signed(addr) + offset)) then {
        if offset != 0 then observe_mem_write(sail_mask_signed(48, addr), offset - 1);
        x86_model_error("wb: non-canonical address")
    };
    let lin_addr : bits(48) = add_bits_int(sail_mask_signed(48, addr), offset);
    let phys_addr : bits(52) =
      if app_view then sail_mask_signed(52, lin_addr) else ia32e_la_to_pa(lin_addr, access_kind)
    in
    bang_memi(phys_addr, value[((8 * offset) + 7) .. (8 * offset)])
  };

  let dummy = observe_mem_write(sail_mask_signed(48, addr), n);
  return ()
}

$endif
