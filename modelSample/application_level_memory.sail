$include <prelude.sail>
$include <string.sail>
$include "state_field_thms.sail"
/*Recognizer of a canonical address*/
/*<p>In 64-bit mode, a <i>linear</i> address is considered to be in 
 canonical form if address bits 63 through to the most-significant implemented 
 bit by the microarchitecture ( represented by the constant 
 @ ( ' *max-linear-address-size* ' ) in these books ) are set to either all ones or 
 all zeros.</p>*/
val canonical_address_p : (sbits(48)) -> bool effect {escape}
function canonical_address_p (lin_addr) =
signed_byte_p(48, lin_addr)

val rvm08 : (sbits(48)) -> (option(string), bits(8)) effect {escape, rmem, rreg}
function rvm08 (addr) =
if canonical_address_p(addr)
then (None() : option(string), memi(the_bits(64, n48(addr))))
else (Some("rvm08"), get_slice_int(8, 0, 0))

val wvm08 : (sbits(48), bits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wvm08 (addr, sailval) =
if canonical_address_p(addr)
then let x86 = (bang_memi(the_bits(64, n48(addr)), n08(sailval))) : unit in
None() : option(string)
else Some("wvm08")

