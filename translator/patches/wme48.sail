val wme48 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(48), bool) -> option(string)

function wme48 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 6, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}
