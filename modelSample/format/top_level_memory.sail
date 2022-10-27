
val address_aligned_p : (sbits(48), {|1, 2, 4, 6, 8, 10, 16, 32, 64|}, bool) -> bool

function address_aligned_p (addr, operand_size, memory_ptr?) = {
    (match operand_size {
      1 => true,
      6 => logand(addr, 0x000000000003) == 0x000000000000,
      10 => logand(addr, 0x000000000007) == 0x000000000000,
      _ => {
          if memory_ptr? & operand_size == 4 then {
              logand(addr, 0x000000000001) == 0x000000000000
          } else {
              logand(addr, bits_of_int(check_range(0, 65, operand_size - 1), 48)) == 0x000000000000
          }
      }
    }) : bool
}

val rme08 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string) -> (option(string), bits(8))

function rme08 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x) = {
    load_bytes_from_ea(proc_mode, addr_size, 1, eff_addr, base_reg, seg_reg, r_x, false, false)
}

overload rime08 = {rme08}

val rme16 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> (option(string), bits(16))

function rme16 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 2, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

overload rime16 = {rme16}

val rme32 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(32))

function rme32 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    load_bytes_from_ea(proc_mode, addr_size, 4, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?)
}

overload rime32 = {rme32}

val rme48 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> (option(string), bits(48))

function rme48 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 6, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

val rme64 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> (option(string), bits(64))

function rme64 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 8, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

overload rime64 = {rme64}

val rme80 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> (option(string), bits(80))

function rme80 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 10, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

val rme128 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool) -> (option(string), bits(128))

function rme128 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, r_x, check_alignment?) = {
    load_bytes_from_ea(proc_mode, addr_size, 16, eff_addr, base_reg, seg_reg, r_x, check_alignment?, false)
}

val rme_size : (proc_mode, address_size, {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(128))

function rme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    let (flg, result) = load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?);
    (flg, sail_zero_extend(result, 128))
}

val rime_size : (proc_mode, address_size, {|1, 2, 4, 6, 8|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(64))

function rime_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    let (flg, result) = load_bytes_from_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?);
    (flg, sail_sign_extend(result, 64))
}

val wme08 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(8)) -> option(string)

function wme08 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data) = {
    store_bytes_to_ea(proc_mode, addr_size, 1, eff_addr, base_reg, seg_reg, data, false, false)
}

overload wime08 = {wme08}

val wme16 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(16), bool) -> option(string)

function wme16 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 2, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

overload wime16 = {wme16}

val wme32 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(32), bool, bool) -> option(string)

function wme32 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    store_bytes_to_ea(proc_mode, addr_size, 4, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?)
}

overload wime32 = {wme32}

val wme48 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(48), bool) -> option(string)

function wme48 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 6, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

val wme64 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(64), bool) -> option(string)

function wme64 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 8, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

overload wime64 = {wme64}

val wme80 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(80), bool) -> option(string)

function wme80 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 10, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

val wme128 : (proc_mode, address_size, sbits(64), option(base_reg_idx), seg_reg_idx, bits(128), bool) -> option(string)

function wme128 (proc_mode, addr_size, eff_addr, base_reg, seg_reg, data, check_alignment?) = {
    store_bytes_to_ea(proc_mode, addr_size, 16, eff_addr, base_reg, seg_reg, data, check_alignment?, false)
}

val wme_size : forall 'nbytes, 'nbytes in {1, 2, 4, 6, 8, 10, 16}.
  (proc_mode, address_size, int('nbytes), sbits(64), option(base_reg_idx), seg_reg_idx, bits(128), bool, bool) -> option(string)

function wme_size (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    store_bytes_to_ea(proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, truncate(data, 8 * nbytes), check_alignment?, mem_ptr?)
}

overload wime_size = {wme_size}
