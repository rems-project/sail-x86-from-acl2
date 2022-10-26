val wme_size : forall 'nbytes, 'nbytes in {1, 2, 4, 6, 8, 10, 16}.
  (proc_mode, address_size, int('nbytes), sbits(64), option(base_reg_idx), seg_reg_idx, bits(128), bool, bool) -> option(string)

function wme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    store_bytes_to_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, truncate(data, 8 * nbytes), check_alignment?, mem_ptr?)
}
