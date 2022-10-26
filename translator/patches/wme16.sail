val wme16 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(16), bool) -> option(string)

function wme16 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 2, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}
