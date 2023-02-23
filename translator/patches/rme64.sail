val rme64 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> bits(64)

function rme64 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 8, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}
