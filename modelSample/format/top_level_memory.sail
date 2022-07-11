$include "./segmentation.sail"

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

val rme08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), bits(8))

function rme08 (proc_mode, eff_addr, seg_reg, r_x) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 1);
        if is_some(flg) then {
            (flg, 0x00)
        } else {
            rml08(truncate(lin_addr, 48), r_x)
        }
    }
}

val rime08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), sbits(8))

function rime08 (proc_mode, eff_addr, seg_reg, r_x) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 1);
        if is_some(flg) then {
            (flg, 0x00)
        } else {
            riml08(truncate(lin_addr, 48), r_x)
        }
    }
}

val rme16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), bits(16))

function rme16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 2);
        if is_some(flg) then {
            (flg, 0x0000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000)
        } else {
            rml16(truncate(lin_addr, 48), r_x)
        }
    }
}

val rime16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(16))

function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 2);
        if is_some(flg) then {
            (flg, 0x0000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000)
        } else {
            riml16(truncate(lin_addr, 48), r_x)
        }
    }
}

struct struct_rme32 = {mem_ptr? : bool}

val rme32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rme32) -> (option(string), bits(32))

function rme32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 4);
        if is_some(flg) then {
            (flg, 0x00000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x00000000)
        } else {
            rml32(truncate(lin_addr, 48), r_x)
        }
    }
}

struct struct_rime32 = {mem_ptr? : bool}

val rime32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rime32) -> (option(string), sbits(32))

function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 4);
        if is_some(flg) then {
            (flg, 0x00000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x00000000)
        } else {
            riml32(truncate(lin_addr, 48), r_x)
        }
    }
}

val rme48 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), bits(48))

function rme48 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 6);
        if is_some(flg) then {
            (flg, 0x000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 6, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x000000000000)
        } else {
            rml48(truncate(lin_addr, 48), r_x)
        }
    }
}

val rme64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), bits(64))

function rme64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 8);
        if is_some(flg) then {
            (flg, 0x0000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000000000000000)
        } else {
            rml64(truncate(lin_addr, 48), r_x)
        }
    }
}

val rime64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(64))

function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 8);
        if is_some(flg) then {
            (flg, 0x0000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000000000000000)
        } else {
            riml64(truncate(lin_addr, 48), r_x)
        }
    }
}

val rme80 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), bits(80))

function rme80 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00000000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 10);
        if is_some(flg) then {
            (flg, 0x00000000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 10, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x00000000000000000000)
        } else {
            rml80(truncate(lin_addr, 48), r_x)
        }
    }
}

val rme128 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), bits(128))

function rme128 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00000000000000000000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 16);
        if is_some(flg) then {
            (flg, 0x00000000000000000000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 16, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x00000000000000000000000000000000)
        } else {
            rml128(truncate(lin_addr, 48), r_x)
        }
    }
}

struct struct_rme_size = {mem_ptr? : bool}

val rme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), string, bool, struct_rme_size) -> (option(string), bits(128))

function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x00000000000000000000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            (flg, 0x00000000000000000000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x00000000000000000000000000000000)
        } else {
            rml_size(nbytes, truncate(lin_addr, 48), r_x)
        }
    }
}

struct struct_rime_size = {mem_ptr? : bool}

val rime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), string, bool, struct_rime_size) -> (option(string), sbits(64))

function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]))
    in
    let r : bits(1) = code_segment_descriptor_attributesbits_get_r(bits_of_int(attr, 16)) in r == 0b0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0x0000000000000000)
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            (flg, 0x0000000000000000)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0x0000000000000000)
        } else {
            riml_size(nbytes, truncate(lin_addr, 48), r_x)
        }
    }
}

val wme08 : (range(0, 4), sbits(64), range(0, 5), bits(8)) -> option(string)

function wme08 (proc_mode, eff_addr, seg_reg, sailval) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 1);
        if is_some(flg) then {
            flg
        } else {
            wml08(truncate(lin_addr, 48), sailval)
        }
    }
}

val wime08 : (range(0, 4), sbits(64), range(0, 5), sbits(8)) -> option(string)

function wime08 (proc_mode, eff_addr, seg_reg, sailval) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 1);
        if is_some(flg) then {
            flg
        } else {
            wiml08(truncate(lin_addr, 48), sailval)
        }
    }
}

val wme16 : (range(0, 4), sbits(64), range(0, 5), bits(16), bool) -> option(string)

function wme16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 2);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 2, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml16(truncate(lin_addr, 48), sailval)
        }
    }
}

val wime16 : (range(0, 4), sbits(64), range(0, 5), sbits(16), bool) -> option(string)

function wime16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 2);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 2, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml16(truncate(lin_addr, 48), sailval)
        }
    }
}

struct struct_wme32 = {mem_ptr? : bool}

val wme32 : (range(0, 4), sbits(64), range(0, 5), bits(32), bool, struct_wme32) -> option(string)

function wme32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 4);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 4, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml32(truncate(lin_addr, 48), sailval)
        }
    }
}

struct struct_wime32 = {mem_ptr? : bool}

val wime32 : (range(0, 4), sbits(64), range(0, 5), sbits(32), bool, struct_wime32) -> option(string)

function wime32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 4);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 4, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml32(truncate(lin_addr, 48), sailval)
        }
    }
}

val wme48 : (range(0, 4), sbits(64), range(0, 5), bits(48), bool) -> option(string)

function wme48 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 6);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 6, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml48(truncate(lin_addr, 48), sailval)
        }
    }
}

val wme64 : (range(0, 4), sbits(64), range(0, 5), bits(64), bool) -> option(string)

function wme64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 8);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 8, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml64(truncate(lin_addr, 48), sailval)
        }
    }
}

val wime64 : (range(0, 4), sbits(64), range(0, 5), sbits(64), bool) -> option(string)

function wime64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 8);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 8, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml64(truncate(lin_addr, 48), sailval)
        }
    }
}

val wme80 : (range(0, 4), sbits(64), range(0, 5), bits(80), bool) -> option(string)

function wme80 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 10);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 10, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml80(truncate(lin_addr, 48), sailval)
        }
    }
}

val wme128 : (range(0, 4), sbits(64), range(0, 5), bits(128), bool) -> option(string)

function wme128 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, 16);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), 16, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml128(truncate(lin_addr, 48), sailval)
        }
    }
}

struct struct_wme_size = {mem_ptr? : bool}

val wme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), nat, bool, struct_wme_size) -> option(string)

function wme_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), nbytes, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml_size(nbytes, truncate(lin_addr, 48), sailval)
        }
    }
}

struct struct_wime_size = {mem_ptr? : bool}

val wime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), int, bool, struct_wime_size) -> option(string)

function wime_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) in
    let w : bits(1) = data_segment_descriptor_attributesbits_get_w(bits_of_int(attr, 16)) in w == 0b0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) : (option(string), sbits(64)) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes);
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(truncate(lin_addr, 48), nbytes, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml_size(nbytes, truncate(lin_addr, 48), sailval)
        }
    }
}
