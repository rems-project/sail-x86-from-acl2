$include "./segmentation.sail"

val address_aligned_p : (sbits(48), {|1, 2, 4, 6, 8, 10, 16|}, bool) -> bool

function address_aligned_p (addr, operand_size, memory_ptr?) = {
    let addr = addr : sbits(48);
    let operand_size = the_range(0, 16, operand_size) : {|1, 2, 4, 6, 8, 10, 16|};
    (match operand_size {
      1 => true,
      6 => the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 3, 0))) == get_slice_int(8, 0, 0),
      10 => the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 7, 0))) == get_slice_int(8, 0, 0),
      _ => if memory_ptr? & get_slice_int(8, operand_size, 0) == get_slice_int(8, 4, 0) then {
          the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, 1, 0))) == get_slice_int(8, 0, 0)
      } else {
          the_bits(8, logand(the_sbits(49, addr), get_slice_int(49, the_range(0, 15, operand_size - 1), 0))) == get_slice_int(8, 0, 0)
      }
    }) : bool
}

val rme08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), int)

function rme08 (proc_mode, eff_addr, seg_reg, r_x) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else {
            rml08(the_sbits(48, lin_addr), r_x)
        }
    }
}

val rime08 : (range(0, 4), sbits(64), range(0, 5), string) -> (option(string), sbits(10))

function rime08 (proc_mode, eff_addr, seg_reg, r_x) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(10, 0, 0))
    } else {
        let (elem0, elem1) = (let (flg, lin_addr) =
          ea_to_la(proc_mode, eff_addr, seg_reg, 1) : (option(string), sbits(67))
        in
          if is_some(flg) then {
              (flg, get_slice_int(9, 0, 0))
          } else {
              let (elem0, elem1) = riml08(the_sbits(48, lin_addr), r_x) : (option(string), sbits(8));
              (elem0, the_sbits(9, elem1))
          }) : (option(string), sbits(9));
        (elem0, the_sbits(10, elem1))
    }
}

val rme16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int)

function rme16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml16(the_sbits(48, lin_addr), r_x)
        }
    }
}

val rime16 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(19))

function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(19, 0, 0))
    } else {
        let (elem0, elem1) = (let (flg, lin_addr) =
          ea_to_la(proc_mode, eff_addr, seg_reg, 2) : (option(string), sbits(67))
        in
          if is_some(flg) then {
              (flg, get_slice_int(18, 0, 0))
          } else {
              let (elem0, elem1) = (if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 2, false)) then {
                  (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(17, 0, 0))
              } else {
                  let (elem0, elem1) = riml16(the_sbits(48, lin_addr), r_x) : (option(string), sbits(16));
                  (elem0, the_sbits(17, elem1))
              }) : (option(string), sbits(17));
              (elem0, the_sbits(18, elem1))
          }) : (option(string), sbits(18));
        (elem0, the_sbits(19, elem1))
    }
}

struct struct_rme32 = {mem_ptr? : bool}

val rme32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rme32) -> (option(string), int)

function rme32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml32(the_sbits(48, lin_addr), r_x)
        }
    }
}

struct struct_rime32 = {mem_ptr? : bool}

val rime32 : (range(0, 4), sbits(64), range(0, 5), string, bool, struct_rime32) -> (option(string), sbits(35))

function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(35, 0, 0))
    } else {
        let (elem0, elem1) = (let (flg, lin_addr) =
          ea_to_la(proc_mode, eff_addr, seg_reg, 4) : (option(string), sbits(67))
        in
          if is_some(flg) then {
              (flg, get_slice_int(34, 0, 0))
          } else {
              let (elem0, elem1) = (if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 4, keywords.mem_ptr?)) then {
                  (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(33, 0, 0))
              } else {
                  let (elem0, elem1) = riml32(the_sbits(48, lin_addr), r_x) : (option(string), sbits(32));
                  (elem0, the_sbits(33, elem1))
              }) : (option(string), sbits(33));
              (elem0, the_sbits(34, elem1))
          }) : (option(string), sbits(34));
        (elem0, the_sbits(35, elem1))
    }
}

val rme48 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int)

function rme48 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 6) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 6, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml48(the_sbits(48, lin_addr), r_x)
        }
    }
}

val rme64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int)

function rme64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml64(the_sbits(48, lin_addr), r_x)
        }
    }
}

val rime64 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), sbits(67))

function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(67, 0, 0))
    } else {
        let (elem0, elem1) = (let (flg, lin_addr) =
          ea_to_la(proc_mode, eff_addr, seg_reg, 8) : (option(string), sbits(67))
        in
          if is_some(flg) then {
              (flg, get_slice_int(66, 0, 0))
          } else {
              let (elem0, elem1) = (if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 8, false)) then {
                  (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(65, 0, 0))
              } else {
                  let (elem0, elem1) = riml64(the_sbits(48, lin_addr), r_x) : (option(string), sbits(64));
                  (elem0, the_sbits(65, elem1))
              }) : (option(string), sbits(65));
              (elem0, the_sbits(66, elem1))
          }) : (option(string), sbits(66));
        (elem0, the_sbits(67, elem1))
    }
}

val rme80 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int)

function rme80 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 10) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 10, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml80(the_sbits(48, lin_addr), r_x)
        }
    }
}

val rme128 : (range(0, 4), sbits(64), range(0, 5), string, bool) -> (option(string), int)

function rme128 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 16) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 16, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml128(the_sbits(48, lin_addr), r_x)
        }
    }
}

struct struct_rme_size = {mem_ptr? : bool}

val rme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), string, bool, struct_rme_size) -> (option(string), int)

function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : {|0|})
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes) : (option(string), sbits(67));
        if is_some(flg) then {
            (flg, 0 : {|0|})
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : {|0|})
        } else {
            rml_size(nbytes, the_sbits(48, lin_addr), r_x)
        }
    }
}

struct struct_rime_size = {mem_ptr? : bool}

val rime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), string, bool, struct_rime_size) -> (option(string), sbits(68))

function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int
    in
    let r = code_segment_descriptor_attributesbits_get_r(get_slice_int(16, attr, 0)) : bits(1) in unsigned(r) == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), get_slice_int(68, 0, 0))
    } else {
        let (elem0, elem1) = (let (flg, lin_addr) =
          ea_to_la(proc_mode, eff_addr, seg_reg, nbytes) : (option(string), sbits(67))
        in
          if is_some(flg) then {
              (flg, get_slice_int(67, 0, 0))
          } else {
              let (elem0, elem1) = (if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), nbytes, keywords.mem_ptr?)) then {
                  (Some(":UNALIGNED-LINEAR-ADDRESS"), get_slice_int(66, 0, 0))
              } else {
                  let (elem0, elem1) = riml_size(nbytes, the_sbits(48, lin_addr), r_x) : (option(string), sbits(65));
                  (elem0, the_sbits(66, elem1))
              }) : (option(string), sbits(66));
              (elem0, the_sbits(67, elem1))
          }) : (option(string), sbits(67));
        (elem0, the_sbits(68, elem1))
    }
}

val wme08 : (range(0, 4), sbits(64), range(0, 5), bits(8)) -> option(string)

function wme08 (proc_mode, eff_addr, seg_reg, sailval) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else {
            wml08(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wime08 : (range(0, 4), sbits(64), range(0, 5), sbits(8)) -> option(string)

function wime08 (proc_mode, eff_addr, seg_reg, sailval) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else {
            wiml08(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wme16 : (range(0, 4), sbits(64), range(0, 5), bits(16), bool) -> option(string)

function wme16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 2, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml16(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wime16 : (range(0, 4), sbits(64), range(0, 5), sbits(16), bool) -> option(string)

function wime16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 2, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml16(the_sbits(48, lin_addr), sailval)
        }
    }
}

struct struct_wme32 = {mem_ptr? : bool}

val wme32 : (range(0, 4), sbits(64), range(0, 5), bits(32), bool, struct_wme32) -> option(string)

function wme32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 4, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml32(the_sbits(48, lin_addr), sailval)
        }
    }
}

struct struct_wime32 = {mem_ptr? : bool}

val wime32 : (range(0, 4), sbits(64), range(0, 5), sbits(32), bool, struct_wime32) -> option(string)

function wime32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 4, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml32(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wme48 : (range(0, 4), sbits(64), range(0, 5), bits(48), bool) -> option(string)

function wme48 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 6) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 6, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml48(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wme64 : (range(0, 4), sbits(64), range(0, 5), bits(64), bool) -> option(string)

function wme64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 8, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml64(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wime64 : (range(0, 4), sbits(64), range(0, 5), sbits(64), bool) -> option(string)

function wime64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 8, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml64(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wme80 : (range(0, 4), sbits(64), range(0, 5), bits(80), bool) -> option(string)

function wme80 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 10) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 10, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml80(the_sbits(48, lin_addr), sailval)
        }
    }
}

val wme128 : (range(0, 4), sbits(64), range(0, 5), bits(128), bool) -> option(string)

function wme128 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 16) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), 16, false)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml128(the_sbits(48, lin_addr), sailval)
        }
    }
}

struct struct_wme_size = {mem_ptr? : bool}

val wme_size : (range(0, 4), {|1, 2, 4, 6, 8, 10, 16|}, sbits(64), range(0, 5), nat, bool, struct_wme_size) -> option(string)

function wme_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), nbytes, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wml_size(nbytes, the_sbits(48, lin_addr), sailval)
        }
    }
}

struct struct_wime_size = {mem_ptr? : bool}

val wime_size : (range(0, 4), {|1, 2, 4, 8|}, sbits(64), range(0, 5), int, bool, struct_wime_size) -> option(string)

function wime_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
    let w = data_segment_descriptor_attributesbits_get_w(get_slice_int(16, attr, 0)) : bits(1) in unsigned(w) == 0)) then {
        Some(":NON-WRITABLE-SEGMENT")
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes) : (option(string), sbits(67));
        if is_some(flg) then {
            flg
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(the_sbits(48, lin_addr), nbytes, keywords.mem_ptr?)) then {
            Some(":UNALIGNED-LINEAR-ADDRESS")
        } else {
            wiml_size(nbytes, the_sbits(48, lin_addr), sailval)
        }
    }
}
