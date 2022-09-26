$include "./add_spec.sail"
$include "./sub_spec.sail"
$include "./or_spec.sail"
$include "./and_spec.sail"
$include "./xor_spec.sail"

val gpr_arith_logic_spec_1 : ({|0, 1, 2, 3, 4, 5, 6, 7, 8|}, bits(8), bits(8), rflagsbits) -> (bits(8), rflagsbits, rflagsbits)

function gpr_arith_logic_spec_1 (operation, dst, src, input_rflags) = {
    (match operation {
      0 => gpr_add_spec_1(dst, src, input_rflags),
      1 => gpr_or_spec_1(dst, src, input_rflags),
      2 => gpr_adc_spec_1(dst, src, input_rflags),
      3 => gpr_and_spec_1(dst, src, input_rflags),
      4 => gpr_sub_spec_1(dst, src, input_rflags),
      5 => gpr_xor_spec_1(dst, src, input_rflags),
      6 => gpr_sbb_spec_1(dst, src, input_rflags),
      7 => gpr_and_spec_1(dst, src, input_rflags),
      8 => gpr_sub_spec_1(dst, src, input_rflags),
      _ => (0x00, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(8), rflagsbits, rflagsbits)
}

val gpr_arith_logic_spec_2 : ({|0, 1, 2, 3, 4, 5, 6, 7, 8|}, bits(16), bits(16), rflagsbits) -> (bits(16), rflagsbits, rflagsbits)

function gpr_arith_logic_spec_2 (operation, dst, src, input_rflags) = {
    (match operation {
      0 => gpr_add_spec_2(dst, src, input_rflags),
      1 => gpr_or_spec_2(dst, src, input_rflags),
      2 => gpr_adc_spec_2(dst, src, input_rflags),
      3 => gpr_and_spec_2(dst, src, input_rflags),
      4 => gpr_sub_spec_2(dst, src, input_rflags),
      5 => gpr_xor_spec_2(dst, src, input_rflags),
      6 => gpr_sbb_spec_2(dst, src, input_rflags),
      7 => gpr_and_spec_2(dst, src, input_rflags),
      8 => gpr_sub_spec_2(dst, src, input_rflags),
      _ => (0x0000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(16), rflagsbits, rflagsbits)
}

val gpr_arith_logic_spec_4 : ({|0, 1, 2, 3, 4, 5, 6, 7, 8|}, bits(32), bits(32), rflagsbits) -> (bits(32), rflagsbits, rflagsbits)

function gpr_arith_logic_spec_4 (operation, dst, src, input_rflags) = {
    (match operation {
      0 => gpr_add_spec_4(dst, src, input_rflags),
      1 => gpr_or_spec_4(dst, src, input_rflags),
      2 => gpr_adc_spec_4(dst, src, input_rflags),
      3 => gpr_and_spec_4(dst, src, input_rflags),
      4 => gpr_sub_spec_4(dst, src, input_rflags),
      5 => gpr_xor_spec_4(dst, src, input_rflags),
      6 => gpr_sbb_spec_4(dst, src, input_rflags),
      7 => gpr_and_spec_4(dst, src, input_rflags),
      8 => gpr_sub_spec_4(dst, src, input_rflags),
      _ => (0x00000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(32), rflagsbits, rflagsbits)
}

val gpr_arith_logic_spec_8 : ({|0, 1, 2, 3, 4, 5, 6, 7, 8|}, bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_arith_logic_spec_8 (operation, dst, src, input_rflags) = {
    (match operation {
      0 => gpr_add_spec_8(dst, src, input_rflags),
      1 => gpr_or_spec_8(dst, src, input_rflags),
      2 => gpr_adc_spec_8(dst, src, input_rflags),
      3 => gpr_and_spec_8(dst, src, input_rflags),
      4 => gpr_sub_spec_8(dst, src, input_rflags),
      5 => gpr_xor_spec_8(dst, src, input_rflags),
      6 => gpr_sbb_spec_8(dst, src, input_rflags),
      7 => gpr_and_spec_8(dst, src, input_rflags),
      8 => gpr_sub_spec_8(dst, src, input_rflags),
      _ => (0x0000000000000000, Mk_rflagsbits(0x00000000), Mk_rflagsbits(0x00000000))
    }) : (bits(64), rflagsbits, rflagsbits)
}

val gpr_arith_logic_spec : ({|1, 2, 4, 8|}, {|0, 1, 2, 3, 4, 5, 6, 7, 8|}, bits(64), bits(64), rflagsbits) -> (bits(64), rflagsbits, rflagsbits)

function gpr_arith_logic_spec (operand_size, operation, dst, src, input_rflags) = {
    (match operand_size {
      1 => {
          let (elem0, elem1, elem2) : (bits(8), rflagsbits, rflagsbits) = gpr_arith_logic_spec_1(operation, truncate(dst, 8), truncate(src, 8), input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      2 => {
          let (elem0, elem1, elem2) : (bits(16), rflagsbits, rflagsbits) = gpr_arith_logic_spec_2(operation, truncate(dst, 16), truncate(src, 16), input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      4 => {
          let (elem0, elem1, elem2) : (bits(32), rflagsbits, rflagsbits) = gpr_arith_logic_spec_4(operation, truncate(dst, 32), truncate(src, 32), input_rflags);
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      _ => gpr_arith_logic_spec_8(operation, dst, src, input_rflags)
    }) : (bits(64), rflagsbits, rflagsbits)
}
