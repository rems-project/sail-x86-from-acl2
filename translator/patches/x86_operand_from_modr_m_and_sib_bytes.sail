val x86_operand_from_modr_m_and_sib_bytes : (proc_mode, bits(1), {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, range(0, 5), prefixes, sbits(48), bits(8), bits(3), bits(2), sib, bits(3)) -> (bits(128), {|0, 1, 2, 4|}, sbits(64))

function x86_operand_from_modr_m_and_sib_bytes (proc_mode, reg_type, operand_size, inst_ac?, memory_ptr?, seg_reg, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes) = {
    let (addr, increment_rip_by) : (sbits(64), {|0, 1, 2, 4|}) = {
        if mod_var == 0b11 then {
            (0x0000000000000000, 0)
        } else {
            x86_effective_addr(proc_mode, prefixes, temp_rip, rex_byte, r_m, mod_var, sib, num_imm_bytes)
        }
    };
    let operand : bits(128) = {
        if mod_var == 0b11 then {
            if reg_type == 0b0 then {
                (sail_zero_extend(rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), rex_byte), 128))
            } else {
                (xmmi_size(bits_of_int(operand_size, 5), reg_index(r_m, rex_byte, 0b00)))
            }
        } else {
            let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
            let addr_size = select_address_size(proc_mode, Some(prefixes));
            let base_reg = select_base_register(proc_mode, rex_byte, r_m, mod_var, sib);
            rme_size(proc_mode, addr_size, operand_size, addr, base_reg, seg_reg, ":R", check_alignment?, memory_ptr?)
        }
    };
    (operand, increment_rip_by, addr)
}
