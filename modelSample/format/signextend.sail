
val x86_cbw_cwd_cdqe : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cbw_cwd_cdqe (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-CBW/CWD/CDQE" : string;
    let register_size = the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let src_size = the_range(1, 4, ash(register_size, -1)) : range(1, 4);
    let src = the_bits(32, rgfi_size(get_slice_int(4, src_size, 0), get_slice_int(4, 0, 0), rex_byte)) : bits(32);
    let dst = (if logbitp(the_range(0, 32, the_range(0, 32, ash(src_size, 3)) - 1), src) then {
        sail_mask(64, trunc(register_size, (match src_size {
          1 => the_sbits(32, n08_to_i08(the_bits(8, src))),
          2 => the_sbits(32, n16_to_i16(the_bits(16, src))),
          _ => n32_to_i32(src)
        }) : sbits(32)))
    } else {
        the_bits(64, src)
    }) : bits(64);
    write_rgfi_size(get_slice_int(4, register_size, 0), get_slice_int(4, 0, 0), unsigned(dst), rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_cwd_cdq_cqo : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function x86_cwd_cdq_cqo (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx = "X86-CWD/CDQ/CQO" : string;
    let src_size = the_range(1, 8, select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false)) : {|8, 1, 2, 4|};
    let src = rgfi_size(get_slice_int(4, src_size, 0), get_slice_int(4, 0, 0), rex_byte) : bits(64);
    let rdx_var = (if logbitp(ash(src_size, 3) - 1, src) then {
        sail_mask(64, trunc(src_size, get_slice_int(64, -1, 0)))
    } else {
        get_slice_int(64, 0, 0)
    }) : bits(64);
    write_rgfi_size(get_slice_int(4, src_size, 0), get_slice_int(4, 2, 0), unsigned(rdx_var), rex_byte);
    write_iptr(proc_mode, temp_rip)
}
