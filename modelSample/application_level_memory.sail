$include <prelude.sail>
$include <string.sail>
$include "state.sail"
/*Recognizer of a canonical address*/
/*<p>In 64-bit mode, a <i>linear</i> address is considered to be in 
 canonical form if address bits 63 through to the most-significant implemented 
 bit by the microarchitecture ( represented by the constant 
 @ ( ' *max-linear-address-size* ' ) in these books ) are set to either all ones or 
 all zeros.</p>*/
val canonical_address_p : (int) -> bool effect {escape}
function canonical_address_p (lin_addr) =
signed_byte_p(48, lin_addr)

val rvm08 : (sbits(48)) -> (option(string), bits(8)) effect {escape, rmem, rreg}
function rvm08 (addr) =
if canonical_address_p(signed(addr))
then (None() : option(string), memi(sail_zero_extend(addr, 64)))
else (Some("rvm08"), 0x00)

val wvm08 : (sbits(48), bits(8)) -> option(string) effect {eamem, escape, rreg, wmv}
function wvm08 (addr, sailval) =
if canonical_address_p(signed(addr))
then let x86 : unit = (bang_memi(sail_zero_extend(addr, 64), sailval)) in
None() : option(string)
else Some("wvm08")

