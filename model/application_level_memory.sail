
/*! Recognizer of a canonical address
<p>In 64-bit mode, a <i>linear</i> address is considered to be in 
 canonical form if address bits 63 through to the most-significant implemented 
 bit by the microarchitecture ( represented by the constant 
 @ ( ' *max-linear-address-size* ' ) in these books ) are set to either all ones or 
 all zeros.</p> */
function canonical_address_p lin_addr = {
    signed_byte_p(48, lin_addr)
}

val rvm08 : sbits(48) -> bits(8)

function rvm08 addr = {
    if canonical_address_p(signed(addr)) then {
        memi(sail_zero_extend(addr, 64))
    } else {
        x86_model_error("rvm08")
    }
}

val wvm08 : (sbits(48), bits(8)) -> unit

function wvm08 (addr, sailval) = {
    if canonical_address_p(signed(addr)) then {
        bang_memi(sail_zero_extend(addr, 64), sailval)
    } else {
        x86_model_error("wvm08")
    }
}
