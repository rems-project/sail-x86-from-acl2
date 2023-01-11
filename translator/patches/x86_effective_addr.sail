val x86_effective_addr : (proc_mode, prefixes, sbits(64), bits(8), bits(3), bits(2), sib, bits(3)) -> (sbits(64), {|0, 1, 2, 4|})

function x86_effective_addr (proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let p4 = (0x67 == prefixes[adr]);
    if 2 == select_address_size(proc_mode, Some(prefixes)) then {
        let (elem1, elem2) : (bits(16), {|0, 1, 2|}) = x86_effective_addr_16(proc_mode, temp_rip, r_m, mod_var);
        (sail_zero_extend(elem1, 64), elem2)
    } else {
        x86_effective_addr_32_64(proc_mode, p4, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
    }
}
