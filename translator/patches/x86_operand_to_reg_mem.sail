val x86_operand_to_reg_mem : (proc_mode, {|1, 2, 4, 6, 8, 10, 16|}, bool, bool, bits(128), seg_reg_idx, sbits(64), prefixes, bits(8), bits(3), bits(2)) -> option(string)

function x86_operand_to_reg_mem (proc_mode, operand_size, inst_ac?, memory_ptr?, operand, seg_reg, addr, prefixes, rex_byte, r_m, mod_var) = {
    if mod_var == 0b11 then {
        write_rgfi_size(bits_of_int(operand_size, 4), reg_index(r_m, rex_byte, 0b00), unsigned(operand), rex_byte);
        None() : option(string)
    } else {
        let check_alignment? : bool = inst_ac? & alignment_checking_enabled_p();
        let addr_size = select_address_size(proc_mode, Some(prefixes));
        let base_reg : option(base_reg_idx) = None(); // TODO
        wme_size(proc_mode, addr_size, operand_size, addr, base_reg, seg_reg, operand, check_alignment?, memory_ptr?)
    }
}
