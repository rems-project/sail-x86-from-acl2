
val chk_exc_fn : (string, string, list(string), range(0, 4), bits(52), bits(8), bits(8), bits(8), bits(8)) -> option(string)

function chk_exc_fn (decode_context, type_id, feature_flags, proc_mode, prefixes, rex_byte, opcode, modr_m, sib) = {
    if decode_context == ":VEX" & ((0xf3 == prefixes_get_rep(prefixes) | 0xf2 == prefixes_get_rep(prefixes)) | 0x66 == prefixes_get_opr(prefixes) | not_bool(rex_byte == 0x00) | not_bool(proc_mode == 0 | proc_mode == 1 | proc_mode == 2)) then {
        Some(":ud")
    } else if type_id == ":TYPE-VEX-GPR" then {
        if feature_flags_fn(feature_flags) == 0 then {
            Some(":ud")
        } else {
            throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
        }
    } else if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
        Some(":nm")
    } else if not_bool(in_list(type_id, [|":TYPE-22-7", ":TYPE-22-8", ":TYPE-22-9"|])) & cr4bits_get_osfxsr(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)) == 0b0 then {
        Some(":ud")
    } else if cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | 0xf0 == prefixes_get_lck(prefixes) | feature_flags_fn(feature_flags) == 0 then {
        Some(":ud")
    } else {
        throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
    }
}
