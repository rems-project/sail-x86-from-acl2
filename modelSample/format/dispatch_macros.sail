
val chk_exc_fn : (string, string, list(string), range(0, 4), prefixes, bits(8), bits(8), modr_m, sib) -> option(string)

function chk_exc_fn (decode_context, type_id, feature_flags, proc_mode, prefixes, rex_byte, opcode, modr_m, sib) = {
    if decode_context == ":VEX" & ((0xf3 == prefixes[rep] | 0xf2 == prefixes[rep]) | 0x66 == prefixes[opr] | not_bool(rex_byte == 0x00) | not_bool(proc_mode == 0 | proc_mode == 1 | proc_mode == 2)) then {
        Some(":UD")
    } else if type_id == ":TYPE-VEX-GPR" then {
        if feature_flags_fn(feature_flags) == 0 then {
            Some(":UD")
        } else {
            None() : option(string)
        }
    } else if Mk_cr0bits(bits_of_int(loghead(32, unsigned(ctrs[0])), 32))[ts] == 0b1 then {
        Some(":NM")
    } else if not_bool(in_list(type_id, [|":TYPE-22-7", ":TYPE-22-8", ":TYPE-22-9"|])) & Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22))[osfxsr] == 0b0 then {
        Some(":UD")
    } else if Mk_cr0bits(bits_of_int(loghead(32, unsigned(ctrs[0])), 32))[em] == 0b1 | 0xf0 == prefixes[lck] | feature_flags_fn(feature_flags) == 0 then {
        Some(":UD")
    } else {
        None() : option(string)
    }
}
