
val chk_exc_fn : (string, string, list(string), proc_mode, prefixes, bits(8), bits(8), modr_m, sib) -> unit

function chk_exc_fn (decode_context, type_id, feature_flags, proc_mode, prefixes, rex_byte, opcode, modr_m, sib) = {
    if decode_context == ":VEX" & ((0xf3 == prefixes[rep] | 0xf2 == prefixes[rep]) | 0x66 == prefixes[opr] | not_bool(rex_byte == 0x00) | not_bool(in_64bit_mode(proc_mode) | in_compatibility_mode(proc_mode) | in_protected_mode(proc_mode))) then {
        x86_fault("(:ud)")
    } else if type_id == ":TYPE-VEX-GPR" then {
        if feature_flags_fn(feature_flags) == 0 then x86_fault("(:ud)");
        ()
    } else if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 then {
        x86_fault("(:nm)")
    } else if not_bool(in_list(type_id, [|":TYPE-22-7", ":TYPE-22-8", ":TYPE-22-9"|])) & Mk_cr4bits(loghead(22, ctrs[4]))[osfxsr] == 0b0 then {
        x86_fault("(:ud)")
    } else if Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 | 0xf0 == prefixes[lck] | feature_flags_fn(feature_flags) == 0 then {
        x86_fault("(:ud)")
    } else ()
}
