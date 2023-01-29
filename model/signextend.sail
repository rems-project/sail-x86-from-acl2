
val x86_cbw_cwd_cdqe : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cbw_cwd_cdqe (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CBW/CWD/CDQE";
    let register_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let src_size : range(1, 4) = check_range(1, 4, ash(register_size, -1));
    let src : bits(32) = truncate(rgfi_size(bits_of_int(src_size, 4), 0x0, rex_byte), 32);
    let dst : bits(64) = {
        if logbitp(check_range(0, 32, check_range(0, 32, ash(src_size, 3)) - 1), src) then {
            sail_mask(64, trunc(register_size, (match src_size {
              1 => sail_sign_extend(truncate(src, 8), 32),
              2 => sail_sign_extend(truncate(src, 16), 32),
              _ => src
            }) : sbits(32)))
        } else {
            sail_zero_extend(src, 64)
        }
    };
    write_rgfi_size(bits_of_int(register_size, 4), 0x0, dst, rex_byte);
    write_iptr(proc_mode, temp_rip)
}

val x86_cwd_cdq_cqo : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function x86_cwd_cdq_cqo (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    let ctx : string = "X86-CWD/CDQ/CQO";
    let src_size : {|1, 2, 4, 8|} = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false);
    let src : bits(64) = rgfi_size(bits_of_int(src_size, 4), 0x0, rex_byte);
    let rdx_var : bits(64) = {
        if logbitp(ash(src_size, 3) - 1, src) then {
            sail_mask(64, trunc(src_size, 0xffffffffffffffff))
        } else 0x0000000000000000
    };
    write_rgfi_size(bits_of_int(src_size, 4), 0x2, rdx_var, rex_byte);
    write_iptr(proc_mode, temp_rip)
}
