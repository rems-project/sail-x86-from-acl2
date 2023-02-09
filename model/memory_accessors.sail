$ifndef __X86_MEMORY_ACCESSORS
$define __X86_MEMORY_ACCESSORS

$include "structures.sail"
$include "segmentation_structures.sail"
$include "linear_memory.sail"
$include "segmentation.sail"
// For prefixes bitfield definition:
$include "opcode_ext.sail"

type address_size = {|2, 4, 8|}
type moffset_size = {|2, 4, 8|}
type base_reg_idx = range(0, 15)
type seg_reg_idx = range(0, 5)

val address_aligned_p : (sbits(48), {|1, 2, 4, 6, 8, 10, 16, 32, 64|}, bool) -> bool

val check_linear_memory_access : (proc_mode, address_size, {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> option(string)

function check_linear_memory_access (proc_mode, addr_size, nbytes, lin_addr, base_reg, seg_reg, access_kind, check_alignment?, mem_ptr?) = {
    if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), nbytes, mem_ptr?)) then {
        Some(":UNALIGNED-LINEAR-ADDRESS")
    } else {
        None()
    }
}

// TODO: Move the following functions?

val select_base_register : (proc_mode, bits(8), bits(3), bits(2), sib) -> option(base_reg_idx)

function select_base_register(proc_mode, rex_byte, r_m, mod_var, sib) = {
    // TODO
    None()
}

val select_address_size : (proc_mode, option(prefixes)) -> address_size

function select_address_size (proc_mode, prefixes) = {
    let p4? : bool = match prefixes {
         Some(prefixes) => 0x67 == prefixes[adr],
         None() => false
    };
    if in_64bit_mode(proc_mode) then {
        if p4? then 4 else 8
    } else {
        let cs_attr : bits(16) = seg_hidden_attrs[1];
        let cs_d : bits(1) = Mk_code_segment_descriptor_attributesbits(cs_attr)[d];
        if cs_d == 0b1 then {
            if p4? then 2 else 4
        } else if p4? then 4 else 2
    }
}

val select_moffset_size : (proc_mode, option(prefixes)) -> moffset_size

function select_moffset_size (proc_mode, prefixes) = {
    select_address_size (proc_mode, prefixes)
}

val select_segment_register : (proc_mode, prefixes, bits(2), bits(3), sib) -> seg_reg_idx

function select_segment_register (proc_mode, prefixes, mod_var, r_m, sib) = {
    match prefixes[seg] {
      46 => 1,
      54 => 2,
      62 => 3,
      38 => 0,
      100 => 4,
      101 => 5,
      _ => {
          let addr_size : {|2, 4, 8|} = select_address_size(proc_mode, Some(prefixes));
          if addr_size == 2 then {
              if not_bool(mod_var == 0b11) & (r_m == 0b010 | r_m == 0b011) then 2 else 3
          } else if (mod_var == 0b01 | mod_var == 0b10) & r_m == 0b101 | not_bool(mod_var == 0b11) & r_m == 0b100 & sib[base] == 0b100 then 2 else 3
      }
    }
}

val load_bytes_from_ea : forall 'nbytes, 'nbytes in {1, 2, 4, 6, 8, 10, 16}.
  (proc_mode, address_size, int('nbytes), sbits(64), option(base_reg_idx), seg_reg_idx, string, bool, bool) -> (option(string), bits(8 * 'nbytes))

function load_bytes_from_ea (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) = {
    let cs_attrs = Mk_code_segment_descriptor_attributesbits(seg_hidden_attrs[1]);
    if not_bool(in_64bit_mode(proc_mode)) & seg_reg == 1 & r_x == ":R" & cs_attrs[r] == 0b0 then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), sail_zeros(8 * nbytes))
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            (flg, sail_zeros(8 * nbytes))
        } else {
            match check_linear_memory_access(proc_mode, addr_size, nbytes, lin_addr, base_reg, seg_reg, r_x, check_alignment?, mem_ptr?) {
                Some(error) => (Some(error), sail_zeros(8 * nbytes)),
                None() => {
                    // Use rb directly instead of rml_size;  it returns a
                    // bitvector of the right width and performs the same
                    // checks for canonical addresses that rml_size does.
                    rb(nbytes, lin_addr, r_x)
                }
            }
        }
    }
}

val store_bytes_to_ea : forall 'nbytes, 'nbytes in {1, 2, 4, 6, 8, 10, 16}.
  (proc_mode, address_size, int('nbytes), sbits(64), option(base_reg_idx), seg_reg_idx, bits(8 * 'nbytes), bool, bool) -> option(string)

function store_bytes_to_ea (proc_mode, addr_size, nbytes, eff_addr, base_reg, seg_reg, data, check_alignment?, mem_ptr?) = {
    let seg_attrs = Mk_data_segment_descriptor_attributesbits(seg_hidden_attrs[seg_reg]);
    if not_bool(in_64bit_mode(proc_mode)) & (seg_reg == 1 | seg_attrs[w] == 0b0) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            flg
        } else {
            match check_linear_memory_access(proc_mode, addr_size, nbytes, lin_addr, base_reg, seg_reg, ":W", check_alignment?, mem_ptr?) {
                Some(error) => Some(error),
                None() => {
                    wb(nbytes, lin_addr, ":W", data)
                }
            }
        }
    }
}

function observe_mem_write (addr, len) = ()

$endif
