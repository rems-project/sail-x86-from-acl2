$include <prelude.sail>
$include <string.sail>
$include "state_field_thms.sail"
/*Recognizer of a canonical address*/
/*<p>In 64-bit mode, a <i>linear</i> address is considered to be in 
 canonical form if address bits 63 through to the most-significant implemented 
 bit by the microarchitecture ( represented by the constant 
 @ ( ' *max-linear-address-size* ' ) in these books ) are set to either all ones or 
 all zeros.</p>*/
val canonical_address_p : (int) -> bool effect {escape}
function canonical_address_p (lin_addr) =
signed_byte_p(48, lin_addr)

val rvm08 : (int, int) -> (option(string), int, int) effect {escape, rmem, rreg}
function rvm08 (addr, x86) =
if canonical_address_p(addr)
then (None() : option(string), memi(n48(addr), 0), 0 : int)
else (Some("rvm08"), 0 : int, 0 : int)

val wvm08 : (int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}
function wvm08 (addr, sailval, x86) =
if canonical_address_p(addr)
then let x86 = (bang_memi(n48(addr), n08(sailval), 0)) : int in
(None() : option(string), 0 : int)
else (Some("wvm08"), 0 : int)

