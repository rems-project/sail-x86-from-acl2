$include "./state_field_thms.sail"

val canonical_address_p : int -> bool effect {escape}

function canonical_address_p lin_addr = {
    signed_byte_p(48, lin_addr)
}

val rvm08 : (int, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rvm08 (addr, x86) = {
    if canonical_address_p(addr) then {
        (None() : option(string), memi(n48(addr), 0), 0 : int)
    } else {
        (Some("rvm08"), 0 : int, 0 : int)
    }
}

val wvm08 : (int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wvm08 (addr, sailval, x86) = {
    if canonical_address_p(addr) then {
        let x86 = bang_memi(n48(addr), n08(sailval), 0) : int;
        (None() : option(string), 0 : int)
    } else {
        (Some("wvm08"), 0 : int)
    }
}
