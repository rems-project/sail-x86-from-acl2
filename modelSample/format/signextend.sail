
val x86_cbw_cwd_cdqe : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rreg, wreg}

function x86_cbw_cwd_cdqe (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-CBW/CWD/CDQE" : string;
    let register_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0) : int;
    let src_size = ash(register_size, -1) : int;
    let src = rgfi_size(src_size, 0, rex_byte, 0) : int;
    let dst = (if logbitp(ash(src_size, 3) - 1, src) then {
        trunc(register_size, (match src_size {
          1 => n08_to_i08(src),
          2 => n16_to_i16(src),
          _ => n32_to_i32(src)
        }) : int)
    } else {
        src
    }) : int;
    let x86 = write_rgfi_size(register_size, 0, dst, rex_byte, 0) : int;
    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
    0
}

val x86_cwd_cdq_cqo : (int, int, int, int, int, int, int, int, int) -> int effect {escape, rreg, wreg}

function x86_cwd_cdq_cqo (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) = {
    let ctx = "X86-CWD/CDQ/CQO" : string;
    let src_size = select_operand_size(proc_mode, false, rex_byte, false, prefixes, false, false, false, 0) : int;
    let src = rgfi_size(src_size, 0, rex_byte, 0) : int;
    let rdx_var = (if logbitp(ash(src_size, 3) - 1, src) then {
        trunc(src_size, -1)
    } else 0) : int;
    let x86 = write_rgfi_size(src_size, 2, rdx_var, rex_byte, 0) : int;
    let x86 = write_iptr(proc_mode, temp_rip, 0) : int;
    0
}
