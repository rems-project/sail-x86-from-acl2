$include "./segmentation.sail"

val address_aligned_p : (int, int, bool) -> bool

function address_aligned_p (addr, operand_size, memory_ptr?) = {
    let addr = addr : int;
    let operand_size = operand_size : int;
    (match operand_size {
      1 => true,
      6 => binary_logand(addr, 3) == 0,
      10 => binary_logand(addr, 7) == 0,
      _ => if memory_ptr? & operand_size == 4 then {
          binary_logand(addr, 1) == 0
      } else {
          binary_logand(addr, operand_size - 1) == 0
      }
    }) : bool
}

val rme08 : (int, int, int, string, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme08 (proc_mode, eff_addr, seg_reg, r_x, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else {
            rml08(lin_addr, r_x, 0)
        }
    }
}

val rime08 : (int, int, int, string, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rime08 (proc_mode, eff_addr, seg_reg, r_x, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else {
            riml08(lin_addr, r_x, 0)
        }
    }
}

val rme16 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml16(lin_addr, r_x, 0)
        }
    }
}

val rime16 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rime16 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            riml16(lin_addr, r_x, 0)
        }
    }
}

struct struct_rme32 = {mem_ptr? : bool}

val rme32 : (int, int, int, string, bool, int, struct_rme32) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml32(lin_addr, r_x, 0)
        }
    }
}

struct struct_rime32 = {mem_ptr? : bool}

val rime32 : (int, int, int, string, bool, int, struct_rime32) -> (option(string), int, int) effect {escape, rmem, rreg}

function rime32 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            riml32(lin_addr, r_x, 0)
        }
    }
}

val rme48 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme48 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 6, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 6, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml48(lin_addr, r_x, 0)
        }
    }
}

val rme64 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml64(lin_addr, r_x, 0)
        }
    }
}

val rime64 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rime64 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            riml64(lin_addr, r_x, 0)
        }
    }
}

val rme80 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme80 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 10, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 10, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml80(lin_addr, r_x, 0)
        }
    }
}

val rme128 : (int, int, int, string, bool, int) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme128 (proc_mode, eff_addr, seg_reg, r_x, check_alignment?, x86) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 16, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 16, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml128(lin_addr, r_x, 0)
        }
    }
}

struct struct_rme_size = {mem_ptr? : bool}

val rme_size : (int, int, int, int, string, bool, int, struct_rme_size) -> (option(string), int, int) effect {escape, rmem, rreg}

function rme_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            rml_size(nbytes, lin_addr, r_x, 0)
        }
    }
}

struct struct_rime_size = {mem_ptr? : bool}

val rime_size : (int, int, int, int, string, bool, int, struct_rime_size) -> (option(string), int, int) effect {escape, rmem, rreg}

function rime_size (proc_mode, nbytes, eff_addr, seg_reg, r_x, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & seg_reg == 1 & r_x == ":R" & (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let r = code_segment_descriptor_attributesbits_get_r(attr) : int in r == 0) then {
        (Some(":EXECUTE-ONLY-CODE-SEGMENT"), 0 : int, 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int, 0 : int)
        } else {
            riml_size(nbytes, lin_addr, r_x, 0)
        }
    }
}

val wme08 : (int, int, int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme08 (proc_mode, eff_addr, seg_reg, sailval, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else {
            wml08(lin_addr, sailval, 0)
        }
    }
}

val wime08 : (int, int, int, int, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wime08 (proc_mode, eff_addr, seg_reg, sailval, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 1, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else {
            wiml08(lin_addr, sailval, 0)
        }
    }
}

val wme16 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml16(lin_addr, sailval, 0)
        }
    }
}

val wime16 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wime16 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 2, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 2, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wiml16(lin_addr, sailval, 0)
        }
    }
}

struct struct_wme32 = {mem_ptr? : bool}

val wme32 : (int, int, int, int, bool, int, struct_wme32) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml32(lin_addr, sailval, 0)
        }
    }
}

struct struct_wime32 = {mem_ptr? : bool}

val wime32 : (int, int, int, int, bool, int, struct_wime32) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wime32 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 4, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 4, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wiml32(lin_addr, sailval, 0)
        }
    }
}

val wme48 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme48 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 6, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 6, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml48(lin_addr, sailval, 0)
        }
    }
}

val wme64 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml64(lin_addr, sailval, 0)
        }
    }
}

val wime64 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wime64 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 8, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 8, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wiml64(lin_addr, sailval, 0)
        }
    }
}

val wme80 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme80 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 10, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 10, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml80(lin_addr, sailval, 0)
        }
    }
}

val wme128 : (int, int, int, int, bool, int) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme128 (proc_mode, eff_addr, seg_reg, sailval, check_alignment?, x86) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, 16, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, 16, false)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml128(lin_addr, sailval, 0)
        }
    }
}

struct struct_wme_size = {mem_ptr? : bool}

val wme_size : (int, int, int, int, int, bool, int, struct_wme_size) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wme_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wml_size(nbytes, lin_addr, sailval, 0)
        }
    }
}

struct struct_wime_size = {mem_ptr? : bool}

val wime_size : (int, int, int, int, int, bool, int, struct_wime_size) -> (option(string), int) effect {eamem, escape, rreg, wmv}

function wime_size (proc_mode, nbytes, eff_addr, seg_reg, sailval, check_alignment?, x86, keywords) = {
    if proc_mode != 0 & (seg_reg == 1 | (let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
      let w = data_segment_descriptor_attributesbits_get_w(attr) : int in w == 0)) then {
        (Some(":NON-WRITABLE-SEGMENT"), 0 : int)
    } else {
        let (flg, lin_addr) = ea_to_la(proc_mode, eff_addr, seg_reg, nbytes, 0) : (option(string), int);
        if is_some(flg) then {
            (flg, 0 : int)
        } else if not_bool(not_bool(check_alignment?) | address_aligned_p(lin_addr, nbytes, keywords.mem_ptr?)) then {
            (Some(":UNALIGNED-LINEAR-ADDRESS"), 0 : int)
        } else {
            wiml_size(nbytes, lin_addr, sailval, 0)
        }
    }
}
