$include <regfp.sail>
$include <real.sail>

overload operator ^ = {concat_str, and_bool}

type sbits('n: Int), 'n > 0 = bits('n)

val feature_flag_fn : string -> int

function feature_flag_fn feature = 1

val feature_flags_fn : list(string) -> int

function feature_flags_fn features = {
    match features {
      [||] => 1,
      x :: xs => if feature_flag_fn(x) == 0 then 0 else {
          feature_flags_fn(xs)
      }
    }
}

union exception = {Emsg : string, Syscall : unit}

val undef_read_logic : unit -> nat

function undef_read_logic () = undefined

val eq_bits_nat : forall 'n, 'n > 0. (bits('n), nat) -> bool

function eq_bits_nat (x, y) = {
    unsigned(x) == y
}

overload operator == = {eq_bits_nat}

val pow2 = pure {_: "pow2"}: forall 'n. int('n) -> int(2 ^ 'n)

val bool_to_bit : bool -> bits(1)

function bool_to_bit b = {
    match b {
      true => 0b1,
      false => 0b0
    }
}

val bit_to_bool : bits(1) -> bool

function bit_to_bool b = {
    match b {
      0b1 => true,
      0b0 => false
    }
}

val trunc : forall 'n 'm, ('n in {1, 2, 4, 8, 16} & 'm > 0). (int('n), bits('m)) -> bits(8 * 'n)

function trunc (n, x) = {
    sail_mask(8 * n, x)
}

val bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)

function bitslice (x, low, width) = {
    assert('n >= 0 & low >= 0 & width > 0);
    if low + width <= 'n then {
        x[low + width - 1 .. low]
    } else if low < 'n - 1 then {
        sail_zeros(low + width - 'n) @ x['n - 1 .. low]
    } else {
        sail_zeros(width)
    }
}

val signed_bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)

function signed_bitslice (x, low, width) = {
    assert('n >= 0 & low >= 0 & width > 0);
    if low + width <= 'n then {
        x[low + width - 1 .. low]
    } else if low < 'n - 1 then {
        replicate_bits([x['n - 1]], low + width - 'n) @ x['n - 1 .. low]
    } else {
        sail_zeros(width)
    }
}

val nat_of_int : int -> nat

function nat_of_int i = {
    assert(i >= 0);
    i
}

val nfix : int -> nat

function nfix i = {
    if i >= 0 then {
        i
    } else 0
}

val check_range : forall 'n 'm 'i, 'n <= 'm.
  (int('n), int('m), int('i)) -> {'j, ('n <= 'j & 'j <= 'm & 'i == 'j). int('j)}

function check_range (n, m, i) = {
    assert(n <= i & i <= m);
    i
}

val sail_mask_signed : forall 'len 'v, ('len >= 0 & 'v >= 0). (int('len), bits('v)) -> bits('len)

function sail_mask_signed (len, v) = {
    if len <= length(v) then {
        truncate(v, len)
    } else {
        sail_sign_extend(v, len)
    }
}

val bits_of_int : forall 'n, 'n >= 0. (int, int('n)) -> bits('n)

function bits_of_int (x, n) = {
    get_slice_int(n, x, 0)
}

val fits_in_bitvector : (int, int) -> bool

function fits_in_bitvector (n, x) = {
    0 <= x & x < pow2(n)
}

val fits_in_signed_bitvector : (int, int) -> bool

function fits_in_signed_bitvector (n, x) = {
    negate(2 ^ (n - 1)) <= x & x < pow2(n - 1)
}

val isEven : int -> bool

function isEven x = {
    if get_slice_int(1, x, 0) == 0b0 then true else false
}

val b_xor : (int, int) -> int

function b_xor (i, j) = {
    assert(0 <= i & i <= 1 & 0 <= j & j <= 1);
    if i == 0 then {
        if j == 0 then 0 else 1
    } else if j == 0 then 1 else 0
}

val in_list : (string, list(string)) -> bool

function in_list (x, xs) = {
    match xs {
      y :: ys => if x == y then true else {
          in_list(x, ys)
      },
      [||] => false
    }
}

val unsigned_byte_p_int : (int, int) -> bool

function unsigned_byte_p_int (bits, x) = {
    assert(bits >= 0);
    0 <= x & x < pow2(bits)
}

val unsigned_byte_p_bits : forall 'n, 'n >= 0. (int, bits('n)) -> bool

function unsigned_byte_p_bits (bits, x) = {
    unsigned_byte_p_int(bits, unsigned(x))
}

overload unsigned_byte_p = {unsigned_byte_p_int, unsigned_byte_p_bits}

val signed_byte_p_int : (int, int) -> bool

function signed_byte_p_int (bits, x) = {
    assert(bits >= 0);
    negate(2 ^ (bits - 1)) <= x & x < pow2(bits - 1)
}

val signed_byte_p_bits : forall 'n, 'n > 0. (int, bits('n)) -> bool

function signed_byte_p_bits (bits, x) = {
    signed_byte_p_int(bits, signed(x))
}

overload signed_byte_p = {signed_byte_p_int, signed_byte_p_bits}

val loghead_int : (int, int) -> int

function loghead_int (size, i) = {
    if size == 0 then 0 else {
        unsigned(get_slice_int(size, i, 0))
    }
}

val loghead_bits : forall 'n 'm, ('n >= 0 & 'm >= 0). (int('n), bits('m)) -> bits('n)

function loghead_bits (size, x) = {
    sail_mask(size, x)
}

overload loghead = {loghead_bits, loghead_int}

val logtail_int : (int, int) -> int

function logtail_int (pos, x) = {
    shr_int(x, pos)
}

val logtail_bits : forall 'pos 'n, ('pos >= 0 & 'pos < 'n). (int('pos), bits('n)) -> bits('n - 'pos)

function logtail_bits (pos, x) = {
    x['n - 1 .. pos]
}

overload logtail = {logtail_bits, logtail_int}

val logbitp_int : (int, int) -> bool

function logbitp_int (i, j) = {
    assert(i >= 0);
    bit_to_bool(get_slice_int(1, j, i))
}

val logbitp_bits : forall 'n 'pos, 'n >= 0. (int('pos), bits('n)) -> bool

function logbitp_bits (pos, x) = {
    if 0 <= pos & pos < 'n then {
        x[pos] == bitone
    } else false
}

overload logbitp = {logbitp_bits, logbitp_int}

val logbit_int : (int, int) -> bits(1)

function logbit_int (pos, j) = {
    assert(pos >= 0);
    get_slice_int(1, j, pos)
}

val logbit_bits : forall 'n 'pos, 'n >= 0. (int('pos), bits('n)) -> bits(1)

function logbit_bits (pos, x) = {
    if 0 <= pos & pos < 'n then {
        [x[pos]]
    } else {
        [bitzero]
    }
}

overload logbit = {logbit_bits, logbit_int}

val lognot_int : int -> int

function lognot_int x = {
    negate(x) - 1
}

overload lognot = {not_vec, lognot_int}

val logcount_int : int -> int

function logcount_int x = {
    if x == 0 then 0 else if x < 0 then {
        logcount_int(lognot(x))
    } else if isEven(x) then {
        logcount_int(shr_int(x, 1))
    } else {
        logcount_int(shr_int(x, 1)) + 1
    }
}

val logcount_bits : forall 'n, 'n >= 0. bits('n) -> int

function logcount_bits x = {
    logcount_int(unsigned(x))
}

overload logcount = {logcount_bits, logcount_int}

val floor2 : (int, int) -> int

function floor2 (i, j) = {
    tdiv = tdiv_int(i, j);
    tmod = tmod_int(i, j);
    if (j > 0) ^ (tmod >= 0) | (j < 0) ^ (tmod <= 0) then {
        tdiv
    } else {
        tdiv - 1
    }
}

val logand_int : (int, int) -> int

function logand_int (i, j) = {
    if i == 0 then 0 else if j == 0 then 0 else if i == -1 then {
        j
    } else if j == -1 then {
        i
    } else {
        let x = 2 * logand_int(floor2(i, 2), floor2(j, 2));
        x + (if isEven(i) then 0 else if isEven(j) then 0 else 1)
    }
}

overload logand = {and_vec, logand_int}

val logior_int : (int, int) -> int

function logior_int (i, j) = {
    lognot(logand(lognot(i), lognot(j)))
}

overload logior = {or_vec, logior_int}

val logxor : forall 'n, 'n >= 0. (bits('n), bits('n)) -> bits('n)

function logxor (x, y) = {
    xor_vec(x, y)
}

val binary_logapp : (int, int, int) -> int

function binary_logapp (size, i, j) = {
    loghead(size, i) + j * pow2(size)
}

val binary_logext : (int, int) -> int

function binary_logext (size, i) = {
    assert(0 < size);
    binary_logapp(size - 1, i, if logbitp(size - 1, i) then {
        negate(1)
    } else 0)
}

val ash_bits : forall 'n, 'n >= 0. (bits('n), int) -> bits('n)

function ash_bits (i, c) = {
    if c == 0 then {
        i
    } else if c > 0 then {
        sail_shiftleft(i, c)
    } else {
        sail_arith_shiftright(i, negate(c))
    }
}

val ash_left_int : forall 'x 'n, 'n >= 0. (int('x), int('n)) -> int('x * 2 ^ 'n)

function ash_left_int (x, n) = {
    x * pow2(n)
}

val ash_int : (int, int) -> int

function ash_int (i, c) = {
    if c == 0 then {
        i
    } else if c > 0 then {
        _shl_int(i, c)
    } else {
        _shr_int(i, negate(c))
    }
}

overload ash = {ash_bits, ash_left_int, ash_int}

val abs : int -> int

function abs x = {
    if x < 0 then {
        negate(x)
    } else {
        x
    }
}

val mod : (int, int) -> int

function mod (x, y) = {
    x - floor2(x, y) * y
}

val rotate_left : (int, int, int) -> int

function rotate_left (x, width, places) = {
    assert(0 <= places);
    placesMod = mod(places, width);
    low_num = width - placesMod;
    xl = get_slice_int(low_num, x, 0);
    xh = get_slice_int(placesMod, x, low_num);
    unsigned(xl @ xh)
}

val rotate_right : (int, int, int) -> int

function rotate_right (x, width, places) = {
    assert(0 <= places);
    placesMod = mod(places, width);
    xl = get_slice_int(placesMod, x, 0);
    xh = get_slice_int(width - placesMod, x, placesMod);
    unsigned(xl @ xh)
}

val merge_2_u64s : (bits(64), bits(64)) -> bits(128)

function merge_2_u64s (a1, a0) = {
    a1 @ a0
}

val merge_2_u32s : (bits(32), bits(32)) -> bits(64)

function merge_2_u32s (a1, a0) = {
    a1 @ a0
}

val merge_4_u32s : (bits(32), bits(32), bits(32), bits(32)) -> bits(128)

function merge_4_u32s (a3, a2, a1, a0) = {
    a3 @ (a2 @ (a1 @ a0))
}

infix 7 <<

overload operator << = {sail_shiftleft}

val changeSlice_int : (int, int, int, int) -> int

function changeSlice_int (x, low, width, v) = {
    set_slice_int(width, x, low, get_slice_int(width, v, 0))
}

val changeSlice_bits : forall 'n 'low 'width, ('low >= 0 & 'width > 0 & 'low + 'width <= 'n).
  (bits('n), int('low), int('width), bits('width)) -> bits('n)

function changeSlice_bits (x, low, width, v) = {
    [x with (low + width - 1) .. low = v]
}

overload changeSlice = {changeSlice_bits, changeSlice_int}

val genericBitstructAccessor : forall 'width 'n 'low, ('width > 0 & 'low >= 0 & 'low + 'width <= 'n).
  (int('width), bits('n), int('low)) -> bits('width)

function genericBitstructAccessor (width, inputBits, low) = {
    inputBits[low + width - 1 .. low]
}

val genericBitstructUpdater : forall 'width 'n 'low, ('width > 0 & 'low >= 0 & 'low + 'width <= 'n).
  (int('width), bits('width), int('low), bits('n)) -> bits('n)

function genericBitstructUpdater (width, spliceBits, low, inputBits) = {
    [inputBits with (low + width - 1) .. low = spliceBits]
}

val log_memory_read : forall 'addr_width, 'addr_width >= 0. (bits('addr_width), bits(8)) -> unit

val memi : forall 'addr_width, 'addr_width >= 0. bits('addr_width) -> bits(8)

function memi addr = {
    result = __read_mem(Read_plain, 64, sail_mask(64, addr), 1);
    log_memory_read(addr, result);
    result
}

val log_memory_write : forall 'addr_width, 'addr_width >= 0. (bits('addr_width), bits(8)) -> unit

val bang_memi : forall 'addr_width, 'addr_width >= 0. (bits('addr_width), bits(8)) -> unit

function bang_memi (addr, valToWrite) = {
    dummy = __write_mem_ea(Write_plain, 64, sail_mask(64, addr), 1);
    dummy2 = __write_mem(Write_plain, 64, sail_mask(64, addr), 1, valToWrite);
    log_memory_write(addr, valToWrite)
}

register app_view : bool

register marking_view : bool

register ms_reg : bool

val ms : unit -> bool

function ms () = {
    ms_reg
}

register fault_reg : bool

val fault : unit -> bool

function fault () = {
    fault_reg
}

val ia32e_la_to_pa : (sbits(48), string) -> (option(string), bits(52))

val canonical_address_p : int -> bool

val rb : forall 'addr_width 'n, ('addr_width > 0 & 'n > 0).
  (int('n), bits('addr_width), string) -> (option(string), bits(8 * 'n))

function rb (n, addr, access_kind) = {
    result : bits(8 * 'n) = sail_zeros(8 * n);
    foreach (offset from 0 to (n - 1) by 1 in inc) {
        if not_bool(canonical_address_p(signed(addr) + offset)) then return((Some("rb: non-canonical address"), result));
        let lin_addr : bits(48) = add_bits_int(sail_mask_signed(48, addr), offset);
        let phys_addr : bits(52) = if app_view then {
            sail_mask_signed(52, lin_addr)
        } else {
            let (flg, phys_addr) = ia32e_la_to_pa(lin_addr, access_kind);
            if is_some(flg) then return((flg, result));
            phys_addr
        };
        result[8 * offset + 7 .. 8 * offset] = memi(phys_addr)
    };
    return((None(), result))
}

val wb : forall 'addr_width 'n, ('addr_width > 0 & 'n > 0).
  (int('n), bits('addr_width), string, bits(8 * 'n)) -> option(string)

function wb (n, addr, access_kind, value) = {
    foreach (offset from 0 to (n - 1) by 1 in inc) {
        if not_bool(canonical_address_p(signed(addr) + offset)) then return(Some("wb: non-canonical address"));
        let lin_addr : bits(48) = add_bits_int(sail_mask_signed(48, addr), offset);
        let phys_addr : bits(52) = if app_view then {
            sail_mask_signed(52, lin_addr)
        } else {
            let (flg, phys_addr) = ia32e_la_to_pa(lin_addr, access_kind);
            if is_some(flg) then return(flg);
            phys_addr
        };
        bang_memi(phys_addr, value[8 * offset + 7 .. 8 * offset])
    };
    return(None())
}

val unimplemented_x86_syscall_app_view : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function unimplemented_x86_syscall_app_view (_, _, _, _, _, _, _, _) = {
    throw(Syscall())
}
