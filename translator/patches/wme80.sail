val wme80 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(80), bool) -> unit

function wme80 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 10, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}
