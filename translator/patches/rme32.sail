val rme32 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(32))

function rme32 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    load_bytes_from_ea(proc_mode, addr_size, 4, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?)
}
