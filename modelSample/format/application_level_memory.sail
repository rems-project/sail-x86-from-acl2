$include "./state_field_thms.sail"

val canonical_address_p : int -> bool

function canonical_address_p lin_addr = {
    signed_byte_p(48, lin_addr)
}

val rvm08 : sbits(48) -> (option(string), bits(8))

function rvm08 addr = {
    if canonical_address_p(signed(addr)) then {
        (None() : option(string), memi(sail_zero_extend(addr, 64)))
    } else {
        (Some("rvm08"), 0x00)
    }
}

val wvm08 : (sbits(48), bits(8)) -> option(string)

function wvm08 (addr, sailval) = {
    if canonical_address_p(signed(addr)) then {
        let x86 : unit = bang_memi(sail_zero_extend(addr, 64), sailval);
        None() : option(string)
    } else {
        Some("wvm08")
    }
}
