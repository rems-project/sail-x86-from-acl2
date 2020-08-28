$include "./segmentation_structures.sail"
$include "./linear_memory.sail"

val segment_base_and_bounds : (int, int, int) -> (int, int, int) effect {escape, rreg}

function segment_base_and_bounds (proc_mode, seg_reg, x86) = {
    (match proc_mode {
      0 => if seg_reg == 4 then {
          (msri(1, 0), 0 : int, 0 : int)
      } else if seg_reg == 5 then {
          (msri(2, 0), 0 : int, 0 : int)
      } else {
          (0 : int, 0 : int, 0 : int)
      },
      1 => let base = loghead(64, seg_hidden_basei(seg_reg, 0)) : int in
        let limit = loghead(32, seg_hidden_limiti(seg_reg, 0)) : int in
          let attr = loghead(16, seg_hidden_attri(seg_reg, 0)) : int in
            let d_b = (if seg_reg == 1 then {
                code_segment_descriptor_attributesbits_get_d(attr)
            } else {
                data_segment_descriptor_attributesbits_get_d_b(attr)
            }) : int in
              let e = (if seg_reg == 1 then 0 else {
                  data_segment_descriptor_attributesbits_get_e(attr)
              }) : int in
                let lower = (if e == 1 then {
                    1 + limit
                } else 0) : int in
                  let upper = (if e == 1 then {
                      if d_b == 1 then 4294967295 else 65535
                  } else {
                      limit
                  }) : int in
                    (n32(base), lower, upper),
      _ => (0 : int, 0 : int, 0 : int)
    }) : (int, int, int)
}

val ea_to_la : (int, int, int, int, int) -> (option(string), int) effect {escape, rreg}

function ea_to_la (proc_mode, eff_addr, seg_reg, nbytes, x86) = {
    (match proc_mode {
      0 => let lin_addr = (if seg_reg == 4 | seg_reg == 5 then {
          let (base, _, _) = segment_base_and_bounds(0, seg_reg, 0) : (int, int, int);
          let lin_addr = I64(base + n64(eff_addr)) : int;
          lin_addr
      } else {
          eff_addr
      }) : int in
        if not_bool(canonical_address_p(lin_addr)) then {
            (Some(":NON-CANONICAL-ADDRESS"), 0 : int)
        } else {
            (None() : option(string), lin_addr)
        },
      1 => if not_bool(seg_reg == 1) & not_bool(seg_reg == 2) & seg_visiblei(seg_reg, 0) < 4 then {
          (Some(":NULL-SEGMENT-SELECTOR"), 0 : int)
      } else {
          let (base, lower_bound, upper_bound) = segment_base_and_bounds(proc_mode, seg_reg, 0) : (int, int, int);
          let first_addr = eff_addr : int;
          let last_addr = (eff_addr + (nbytes + -1)) : int;
          if not_bool(lower_bound <= first_addr & last_addr <= upper_bound) then {
              (Some(":SEGMENT-LIMIT-FAIL"), 0 : int)
          } else {
              let lin_addr = n32(base + eff_addr) : int;
              (None() : option(string), lin_addr)
          }
      },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0 : int)
    }) : (option(string), int)
}

val ia32e_valid_code_segment_descriptor_p : int -> (bool, (string, int))

function ia32e_valid_code_segment_descriptor_p descriptor = {
    if not_bool(code_segment_descriptorbits_get_msb_of_type(descriptor) == 1) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(code_segment_descriptorbits_get_s(descriptor) == 1) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(code_segment_descriptorbits_get_p(descriptor) == 1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(code_segment_descriptorbits_get_l(descriptor) == 1) then {
        (false, (":IA32E-MODE-OFF", descriptor))
    } else if not_bool(code_segment_descriptorbits_get_d(descriptor) == 0) then {
        (false, (":IA32E-DEFAULT-OPERAND-SIZE-INCORRECT", descriptor))
    } else {
        (true, ("", 0 : int))
    }
}

val ia32e_valid_ldt_segment_descriptor_p : int -> (bool, (string, int))

function ia32e_valid_ldt_segment_descriptor_p descriptor = {
    let sailtype = system_segment_descriptorbits_get_type(descriptor) : int;
    if not_bool(sailtype == 2) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(system_segment_descriptorbits_get_s(descriptor) == 0) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(system_segment_descriptorbits_get_p(descriptor) == 1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(system_segment_descriptorbits_get_all_zeroes?(descriptor) == 0) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", 0 : int))
    }
}

val ia32e_valid_call_gate_segment_descriptor_p : int -> (bool, (string, int))

function ia32e_valid_call_gate_segment_descriptor_p descriptor = {
    let sailtype = call_gate_descriptorbits_get_type(descriptor) : int;
    if not_bool(sailtype == 12) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(call_gate_descriptorbits_get_s(descriptor) == 0) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(call_gate_descriptorbits_get_p(descriptor) == 1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(call_gate_descriptorbits_get_all_zeroes?(descriptor) == 0) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", 0 : int))
    }
}

val make_code_segment_attr_field : int -> int

function make_code_segment_attr_field descriptor = {
    let a = code_segment_descriptorbits_get_a(descriptor) : int;
    let r = code_segment_descriptorbits_get_r(descriptor) : int;
    let c = code_segment_descriptorbits_get_c(descriptor) : int;
    let msb_of_type = code_segment_descriptorbits_get_msb_of_type(descriptor) : int;
    let s = code_segment_descriptorbits_get_s(descriptor) : int;
    let dpl = code_segment_descriptorbits_get_dpl(descriptor) : int;
    let p = code_segment_descriptorbits_get_p(descriptor) : int;
    let avl = code_segment_descriptorbits_get_avl(descriptor) : int;
    let l = code_segment_descriptorbits_get_l(descriptor) : int;
    let g = code_segment_descriptorbits_get_g(descriptor) : int;
    change_code_segment_descriptor_attributesbits(0, Some(a), Some(r), Some(c), Some(msb_of_type), Some(s), Some(dpl), Some(p), Some(avl), Some(l), None(), Some(g), None())
}

val make_system_segment_attr_field : int -> int

function make_system_segment_attr_field descriptor = {
    let sailtype = system_segment_descriptorbits_get_type(descriptor) : int;
    let s = system_segment_descriptorbits_get_s(descriptor) : int;
    let dpl = system_segment_descriptorbits_get_dpl(descriptor) : int;
    let p = system_segment_descriptorbits_get_p(descriptor) : int;
    let avl = system_segment_descriptorbits_get_avl(descriptor) : int;
    let g = system_segment_descriptorbits_get_g(descriptor) : int;
    change_system_segment_descriptor_attributesbits(0, Some(sailtype), Some(s), Some(dpl), Some(p), Some(avl), Some(g), None())
}
