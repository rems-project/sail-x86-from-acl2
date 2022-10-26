val wme08 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(8)) -> option(string)

function wme08 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data) = {
    store_bytes_to_ea(proc_mode, addr_size, 1, eff_addr, base_reg, seg_reg, data, false, false)
}
