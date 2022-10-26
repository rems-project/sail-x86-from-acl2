val rme_size : (proc_mode, address_size, {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(128))

function rme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    let (flg, result) = load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?);
    (flg, sail_zero_extend(result, 128))
}
