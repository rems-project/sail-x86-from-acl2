$ifndef __X86_OPCODE_EXT
$define __X86_OPCODE_EXT

$include "structures.sail"

type proc_mode = range(0, 4)

bitfield prefixes : bits(52) =
  {nxt : 51..44, adr : 43..36, opr : 35..28, seg : 27..20, rep : 19..12, lck : 11..4, num : 3..0}

val is_ext_prefix_byte : (proc_mode, prefixes, bits(8), range(0, 15), bits(8)) -> bool

function is_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte) = false

val process_ext_prefix_byte : (proc_mode, prefixes, bits(8), range(0, 15), bits(8)) -> (option(string), prefixes, bits(8), bool)

function process_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte) = (None(), prefixes, rex_byte, false)

val ext_one_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_two_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), bits(8), modr_m, sib) -> unit

function ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f38_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_vex_0f3a_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit

function ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f38_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

val ext_evex_0f3a_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function ext_evex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    throw(Emsg("Error: Unimplemented or illegal instruction"))
}

$endif
