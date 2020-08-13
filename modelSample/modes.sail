$include <prelude.sail>
$include <string.sail>
$include "register_readers_and_writers.sail"
/*Check whether we are in 64-bit mode.*/
/*<p> 
 Given the above modeling assumption stated in @ ( see x86-modes ) , 
 this predicate discriminates between 
 64-bit mode and the other two modes ( collectively, 32-bit mode ) . 
 Based on Intel manual, Mar ' 17, Vol. 3A, Sec. 2.2 ( near Fig. 2-3 ) , 
 the discrimination is based on the IA32_EFER.LME and CS.L bits: 
 if they are both 1, we are in 64-bit mode, 
 otherwise we are in 32-bit mode 
 ( protected mode if IA32_EFER.LME is 0, 
 compatibility mode if IA32_EFER.LME is 1 and CS.L is 0; 
 note that when IA32_EFER.LME is 0, CS.L should be 0, 
 according to Intel manual, Mar ' 17, Vol. 3A, Sec. 3.4.5 ) . 
 </p> 
 <p> 
 This predicate does not include state invariants such as 
 the constraints imposed by the 64-bit mode consistency checks 
 described in Intel manual, Mar ' 17, Vol. 3A, Sec. 9.8.5. 
 </p> 
 <p> 
 This predicate is useful as a hypothesis of theorems 
 about either 64-bit or 32-bit mode. 
 </p> 
 <p> 
 Since @ ( ' ( xr :msr ... x86 ) ' ) returns a 64-bit value 
 but the IA32_EFER register consists of 12 bits. 
 So we use @ ( tsee n12 ) to make @ ( ' ia32_eferBits ' ) functions applicable. 
 </p>*/
val n64_bit_modep : (int) -> bool effect {escape, rreg}
function n64_bit_modep (x86) =
let ia32_efer = (n12(msri(0, 0))) : int in
let ia32_efer_lma = (ia32_eferbits_get_lma(ia32_efer)) : int in
let cs_attr = (seg_hidden_attri(1, 0)) : int in
let cs_l = (code_segment_descriptor_attributesbits_get_l(cs_attr)) : int in
((ia32_efer_lma) == (1)) & ((cs_l) == (1))

/*Returns the current mode of operation of the x86 machine*/
/*<p>We only support 64-bit, Compatibility, and 32-bit Protected Modes 
 for now.</p>*/
val x86_operation_mode : (int) -> int effect {escape, rreg}
function x86_operation_mode (x86) =
if n64_bit_modep(0)
then 0
else 1

