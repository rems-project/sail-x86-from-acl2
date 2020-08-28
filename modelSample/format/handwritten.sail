$include <regfp.sail>
$include <real.sail>

overload operator ^ = {concat_str, and_bool}

val feature_flag_fn : (string, int) -> int

function feature_flag_fn (feature, x86_dummy) = 1

val feature_flags_fn : (list(string), int) -> int

function feature_flags_fn (features, x86_dummy) = {
    match features {
      [||] => 1,
      x :: xs => if feature_flag_fn(x, 0) == 0 then 0 else {
          feature_flags_fn(xs, 0)
      }
    }
}

union exception = {Emsg : string}

val undef_read_logic : int -> (nat, int) effect {undef}

function undef_read_logic x86_dummy = {
    (undefined, 0)
}

val pow2 = {_: "pow2"}: forall ('n : Int). int('n) -> int(2 ^ 'n)

val bool_to_bit : bool -> int

function bool_to_bit b = {
    match b {
      true => 1,
      false => 0
    }
}

val bit_to_bool : bits(1) -> bool

function bit_to_bool b = {
    match b {
      0b1 => true,
      0b0 => false
    }
}

val the_int : int -> int

function the_int i = {
    i
}

val the_nat : int -> nat effect {escape}

function the_nat i = {
    assert(i >= 0);
    i
}

val the_range : forall ('n 'm : Int), 'n <= 'm. (int('n), int('m), int) -> range('n, 'm) effect {escape}

function the_range (n, m, i) = {
    assert(n <= i & i <= m);
    i
}

val isEven : int -> bool

function isEven x = {
    get_slice_int(1, x, 0) == 0b0
}

val b_xor : (int, int) -> int effect {escape}

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

val unsigned_byte_p : (int, int) -> bool effect {escape}

function unsigned_byte_p (bits, x) = {
    assert(bits >= 0 & x >= 0);
    x < pow2(bits)
}

val signed_byte_p : (int, int) -> bool effect {escape}

function signed_byte_p (bits, x) = {
    assert(bits >= 0);
    negate(2 ^ (bits - 1)) <= x & x < pow2(bits - 1)
}

val loghead : (int, int) -> int

function loghead (size, i) = {
    if size == 0 then 0 else {
        unsigned(get_slice_int(size, i, 0))
    }
}

val logtail : (int, int) -> int

function logtail (pos, x) = {
    shr_int(x, pos)
}

val logbitp : (int, int) -> bool effect {escape}

function logbitp (i, j) = {
    assert(i >= 0);
    bit_to_bool(get_slice_int(1, j, i))
}

val logbit : (int, int) -> int effect {escape}

function logbit (pos, j) = {
    assert(pos >= 0);
    unsigned(get_slice_int(1, j, pos))
}

val lognot : int -> int

function lognot x = {
    negate(x) - 1
}

val logcount : int -> int effect {escape}

function logcount x = {
    if x == 0 then 0 else if x < 0 then {
        logcount(lognot(x))
    } else if isEven(x) then {
        logcount(shr_int(x, 1))
    } else {
        logcount(shr_int(x, 1)) + 1
    }
}

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

val binary_logand : (int, int) -> int

function binary_logand (i, j) = {
    if i == 0 then 0 else if j == 0 then 0 else if i == -1 then {
        j
    } else if j == -1 then {
        i
    } else {
        let x = 2 * binary_logand(floor2(i, 2), floor2(j, 2));
        x + (if isEven(i) then 0 else if isEven(j) then 0 else 1)
    }
}

val binary_logior : (int, int) -> int

function binary_logior (i, j) = {
    lognot(binary_logand(lognot(i), lognot(j)))
}

val binary_logorc1 : (int, int) -> int

function binary_logorc1 (i, j) = {
    binary_logior(lognot(i), j)
}

val binary_logeqv : (int, int) -> int

function binary_logeqv (i, j) = {
    binary_logand(binary_logorc1(i, j), binary_logorc1(j, i))
}

val binary_logxor : (int, int) -> int

function binary_logxor (i, j) = {
    lognot(binary_logeqv(i, j))
}

val binary_logapp : (int, int, int) -> int

function binary_logapp (size, i, j) = {
    loghead(size, i) + j * pow2(size)
}

val binary_logext : (int, int) -> int effect {escape}

function binary_logext (size, i) = {
    assert(0 < size);
    binary_logapp(size - 1, i, if logbitp(size - 1, i) then {
        negate(1)
    } else 0)
}

val n_size : (int, int) -> int

function n_size (n, x) = {
    loghead(n, x)
}

val ash : (int, int) -> int

function ash (i, c) = {
    if c == 0 then {
        i
    } else if c > 0 then {
        _shl_int(i, c)
    } else {
        _shr_int(i, negate(c))
    }
}

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

val merge_2_u64s : (int, int) -> int

function merge_2_u64s (a1, a0) = {
    binary_logior(ash(a1, 64), a0)
}

val merge_2_u32s : (int, int) -> int

function merge_2_u32s (a1, a0) = {
    binary_logior(ash(a1, 32), a0)
}

val merge_4_u32s : (int, int, int, int) -> int

function merge_4_u32s (a3, a2, a1, a0) = {
    merge_2_u64s(merge_2_u32s(a3, a2), merge_2_u32s(a1, a0))
}

infix 7 <<

overload operator << = {sail_shiftleft}

val changeBits : (int, int, int, int) -> int

function changeBits (x, low, width, v) = {
    set_slice_int(width, x, low, get_slice_int(width, v, 0))
}

register rflags : bits(32)

val r_rflags : int -> int effect {rreg}

function r_rflags x86_dummy = {
    unsigned(rflags)
}

val write_rflags : (int, int) -> int effect {wreg}

function write_rflags (toSet, x86_dummy) = {
    rflags = get_slice_int(32, toSet, 0);
    0
}

val genericBitstructAccessor : (int, int, int) -> int

function genericBitstructAccessor (width, inputBits, low) = {
    unsigned(get_slice_int(width, inputBits, low))
}

val genericBitstructUpdater : (int, int, int, int) -> int

function genericBitstructUpdater (width, spliceBits, low, inputBits) = {
    set_slice_int(width, inputBits, low, get_slice_int(width, spliceBits, 0))
}

register rip : bits(64)

val ripi : int -> int effect {rreg}

function ripi x86_dummy = {
    unsigned(rip)
}

val write_rip : (int, int) -> int effect {wreg}

function write_rip (toSet, x86_dummy) = {
    rip = get_slice_int(64, toSet, 0);
    0
}

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

val rgfi : (int, int) -> int effect {escape, rreg}

function rgfi (regNum, x86_dummy) = {
    assert(0 <= regNum & regNum < 16);
    match regNum {
      0 => signed(rax),
      1 => signed(rcx),
      2 => signed(rdx),
      3 => signed(rbx),
      4 => signed(rsp),
      5 => signed(rbp),
      6 => signed(rsi),
      7 => signed(rdi),
      8 => signed(r8),
      9 => signed(r9),
      10 => signed(r10),
      11 => signed(r11),
      12 => signed(r12),
      13 => signed(r13),
      14 => signed(r14),
      15 => signed(r15),
      _ => throw(Emsg("Invalid regNum in rfgi"))
    }
}

val write_rgfi : (int, int, int) -> int effect {escape, wreg}

function write_rgfi (regNum, v, x86_dummy) = {
    assert(0 <= regNum & regNum < 16);
    val_to_write = get_slice_int(64, v, 0);
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
      _ => throw(Emsg("Invalid regNum in write_rfgi"))
    };
    0
}

register msrs : vector(7, dec, bits(64))

val msri : (int, int) -> int effect {escape, rreg}

function msri (regNum, x86_dummy) = {
    assert(0 <= regNum & regNum < 7);
    unsigned(msrs[regNum])
}

val write_msri : (int, int, int) -> int effect {escape, wreg}

function write_msri (regNum, v, x86_dummy) = {
    assert(0 <= regNum & regNum < 7);
    msrs[regNum] = get_slice_int(64, v, 0);
    0
}

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

register zmms : vector(32, dec, bits(512))

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

register ctrs : vector(17, dec, bits(64))

val ctri : (int, int) -> int effect {escape, rreg}

function ctri (regNum, x86_dummy) = {
    assert(0 <= regNum & regNum < 17);
    unsigned(ctrs[regNum])
}

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

val memi : (int, int) -> int effect {rmem, rreg}

function memi (addr, x86_dummy) = {
    result = unsigned(__read_mem(Read_plain, 64, get_slice_int(64, addr, 0), 1));
    print_endline(dec_str(ripi(0)) ^ ": R " ^ dec_str(addr) ^ " " ^ dec_str(1) ^ " " ^ dec_str(result));
    result
}

val bang_memi : (int, int, int) -> int effect {eamem, wmv, rreg}

function bang_memi (addr, valToWrite, x86_dummy) = {
    dummy = __write_mem_ea(Write_plain, 64, get_slice_int(64, addr, 0), 1);
    dummy2 = __write_mem(Write_plain, 64, get_slice_int(64, addr, 0), 1, get_slice_int(8, valToWrite, 0));
    print_endline(dec_str(ripi(0)) ^ ": W " ^ dec_str(addr) ^ " " ^ dec_str(1) ^ " " ^ dec_str(valToWrite));
    0
}

val app_view : int -> bool

function app_view x86_dummy = true

register ms_reg : bool

val ms : int -> bool effect {rreg}

function ms x86_dummy = {
    ms_reg
}

register fault_reg : bool

val fault : int -> bool effect {rreg}

function fault x86_dummy = {
    fault_reg
}

val unimplemented_vex_decode_and_execute : (int, int, int, int, int, int, int) -> int effect {escape}

val unimplemented_evex_decode_and_execute : (int, int, int, int, int, int, int) -> int effect {escape}

function unimplemented_vex_decode_and_execute (_, _, _, _, _, _, _) = {
    throw(Emsg("Error: vex decoding not implemented"))
}

function unimplemented_evex_decode_and_execute (_, _, _, _, _, _, _) = {
    throw(Emsg("Error: evex decoding not implemented"))
}
