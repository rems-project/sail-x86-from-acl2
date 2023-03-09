val rime_size : (proc_mode, address_size, {|1, 2, 4, 6, 8|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> sbits(64)

function rime_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    sail_sign_extend(load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?), 64);
}
