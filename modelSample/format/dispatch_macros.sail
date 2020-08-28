
val chk_exc_fn : (string, string, list(string), int, int, int, int, int, int, int) -> option(string) effect {escape, rreg}

function chk_exc_fn (decode_context, type_id, feature_flags, proc_mode, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    if decode_context == ":VEX" & ((243 == prefixes_get_rep(prefixes) | 242 == prefixes_get_rep(prefixes)) | 102 == prefixes_get_opr(prefixes) | not_bool(rex_byte == 0) | not_bool(proc_mode == 0 | proc_mode == 1 | proc_mode == 2)) then {
        Some(":ud")
    } else if type_id == ":TYPE-VEX-GPR" then {
        if feature_flags_fn(feature_flags, 0) == 0 then {
            Some(":ud")
        } else {
            throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
        }
    } else if cr0bits_get_ts(loghead(32, ctri(0, 0))) == 1 then {
        Some(":nm")
    } else if not_bool(in_list(type_id, [|":TYPE-22-7", ":TYPE-22-8", ":TYPE-22-9"|])) & cr4bits_get_osfxsr(loghead(22, ctri(4, 0))) == 0 then {
        Some(":ud")
    } else if cr0bits_get_em(loghead(32, ctri(0, 0))) == 1 | 240 == prefixes_get_lck(prefixes) | feature_flags_fn(feature_flags, 0) == 0 then {
        Some(":ud")
    } else {
        throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
    }
}
