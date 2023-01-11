/*! Recognizer of a canonical address
<p>In 64-bit mode, a <i>linear</i> address is considered to be in 
 canonical form if address bits 63 through to the most-significant implemented 
 bit by the microarchitecture ( represented by the constant 
 @ ( ' *max-linear-address-size* ' ) in these books ) are set to either all ones or 
 all zeros.</p> */
function canonical_address_p lin_addr = {
  signed_byte_p(max_linear_address_size, lin_addr)
}
