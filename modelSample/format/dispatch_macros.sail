
val chk_exc_fn : (string, string, list(string), range(0, 4), bits(52), bits(8), bits(8), bits(8), bits(8)) -> option(string)

function chk_exc_fn (decode_context, type_id, feature_flags, proc_mode, prefixes, rex_byte, opcode, modr_m, sib) = {
    if decode_context == ":VEX" & ((get_slice_int(8, 243, 0) == prefixes_get_rep(prefixes) | get_slice_int(8, 242, 0) == prefixes_get_rep(prefixes)) | get_slice_int(8, 102, 0) == prefixes_get_opr(prefixes) | not_bool(rex_byte == get_slice_int(8, 0, 0)) | not_bool(get_slice_int(8, proc_mode, 0) == get_slice_int(8, 0, 0) | get_slice_int(8, proc_mode, 0) == get_slice_int(8, 1, 0) | get_slice_int(8, proc_mode, 0) == get_slice_int(8, 2, 0))) then {
        Some(":ud")
    } else if type_id == ":TYPE-VEX-GPR" then {
        if get_slice_int(8, feature_flags_fn(feature_flags), 0) == get_slice_int(8, 0, 0) then {
            Some(":ud")
        } else {
            throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
        }
    } else if the_bits(8, cr0bits_get_ts(the_bits(32, loghead(32, unsigned(ctrs[0]))))) == get_slice_int(8, 1, 0) then {
        Some(":nm")
    } else if not_bool(in_list(type_id, [|":TYPE-22-7", ":TYPE-22-8", ":TYPE-22-9"|])) & the_bits(8, cr4bits_get_osfxsr(the_bits(22, loghead(22, unsigned(ctrs[4]))))) == get_slice_int(8, 0, 0) then {
        Some(":ud")
    } else if the_bits(8, cr0bits_get_em(the_bits(32, loghead(32, unsigned(ctrs[0]))))) == get_slice_int(8, 1, 0) | get_slice_int(8, 240, 0) == prefixes_get_lck(prefixes) | get_slice_int(8, feature_flags_fn(feature_flags), 0) == get_slice_int(8, 0, 0) then {
        Some(":ud")
    } else {
        throw(Emsg("Translation error: final clause in cond was not `t` and that condition failed"))
    }
}
