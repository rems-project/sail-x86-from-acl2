
val ext_one_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function ext_one_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_two_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), bits(8), modr_m, sib) -> unit

function ext_two_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f38_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f38_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f3a_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f3a_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f38_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f38_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f3a_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f3a_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}
