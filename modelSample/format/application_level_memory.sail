$include "./state_field_thms.sail"

val canonical_address_p : sbits(48) -> bool

function canonical_address_p lin_addr = {
    signed_byte_p(48, lin_addr)
}

val rvm08 : sbits(48) -> (option(string), bits(8))

function rvm08 addr = {
    if canonical_address_p(addr) then {
        (None() : option(string), memi(the_bits(64, n48(addr))))
    } else {
        (Some("rvm08"), get_slice_int(8, 0, 0))
    }
}

val wvm08 : (sbits(48), bits(8)) -> option(string)

function wvm08 (addr, sailval) = {
    if canonical_address_p(addr) then {
        let x86 = bang_memi(the_bits(64, n48(addr)), n08(sailval)) : unit;
        None() : option(string)
    } else {
        Some("wvm08")
    }
}
