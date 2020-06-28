$include <prelude.sail>
$include <string.sail>
$include "add_spec.sail"
$include "sub_spec.sail"
$include "or_spec.sail"
$include "and_spec.sail"
$include "xor_spec.sail"
val gpr_arith_logic_spec_1 : (int, int, int, int) -> (int, int, int) effect {escape}
function gpr_arith_logic_spec_1 (operation, dst, src, input_rflags) =
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
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val gpr_arith_logic_spec_2 : (int, int, int, int) -> (int, int, int) effect {escape}
function gpr_arith_logic_spec_2 (operation, dst, src, input_rflags) =
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
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val gpr_arith_logic_spec_4 : (int, int, int, int) -> (int, int, int) effect {escape}
function gpr_arith_logic_spec_4 (operation, dst, src, input_rflags) =
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
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

val gpr_arith_logic_spec_8 : (int, int, int, int) -> (int, int, int) effect {escape}
function gpr_arith_logic_spec_8 (operation, dst, src, input_rflags) =
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
_ => (0 : int, 0 : int, 0 : int)
}) : (int, int, int)

/*Semantics of general-purpose arithmetic and logical instructions*/
/*<p>These instructions are:</p> 
 <ul> 
 <li>@ ( ' ADD ' ) </li> 
 <li>@ ( ' ADC ' ) </li> 
 <li>@ ( ' SUB ' ) </li> 
 <li>@ ( ' SBB ' ) </li> 
 <li>@ ( ' CMP ' ) </li> 
 <li>@ ( ' OR ' ) </li> 
 <li>@ ( ' AND ' ) </li> 
 <li>@ ( ' XOR ' ) </li> 
 <li>@ ( ' TEST ' ) </li> 
 </ul> 
 
 @ ( def gpr-arith/logic-spec )*/
