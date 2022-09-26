$include "./segmentation_structures.sail"
$include "./linear_memory.sail"

val segment_base_and_bounds : (range(0, 4), range(0, 5)) -> (bits(64), int, int)

function segment_base_and_bounds (proc_mode, seg_reg) = {
    (match proc_mode {
      0 => {
          if seg_reg == 4 then {
              (msrs[1], 0, 0)
          } else if seg_reg == 5 then {
              (msrs[2], 0, 0)
          } else {
              (0x0000000000000000, 0, 0)
          }
      },
      1 => {
          let (elem0, elem1, elem2) : (bits(32), int, int) = {
              let base : int = loghead(64, unsigned(seg_hidden_bases[seg_reg]));
              let limit : int = loghead(32, unsigned(seg_hidden_limits[seg_reg]));
              let attr : int = loghead(16, unsigned(seg_hidden_attrs[seg_reg]));
              let d_b : bits(1) = {
                  if seg_reg == 1 then {
                      Mk_code_segment_descriptor_attributesbits(bits_of_int(attr, 16))[d]
                  } else {
                      Mk_data_segment_descriptor_attributesbits(bits_of_int(attr, 16))[d_b]
                  }
              };
              let e : bits(1) = {
                  if seg_reg == 1 then 0b0 else {
                      Mk_data_segment_descriptor_attributesbits(bits_of_int(attr, 16))[e]
                  }
              };
              let lower : int = {
                  if e == 0b1 then {
                      1 + limit
                  } else 0
              };
              let upper : int = {
                  if e == 0b1 then {
                      if d_b == 0b1 then 4294967295 else 65535
                  } else {
                      limit
                  }
              };
              (bits_of_int(base, 32), lower, upper)
          };
          (sail_zero_extend(elem0, 64), elem1, elem2)
      },
      _ => (0x0000000000000000, 0, 0)
    }) : (bits(64), int, int)
}

val ea_to_la : (range(0, 4), sbits(64), range(0, 5), {|1, 2, 4, 6, 8, 10, 16, 32, 64|}) -> (option(string), sbits(64))

function ea_to_la (proc_mode, eff_addr, seg_reg, nbytes) = {
    (match proc_mode {
      0 => {
          let lin_addr : sbits(64) = {
              if seg_reg == 4 | seg_reg == 5 then {
                  let (base, _, _) : (bits(64), int, int) = segment_base_and_bounds(0, seg_reg);
                  let lin_addr : sbits(64) = bits_of_int(unsigned(base) + unsigned(eff_addr), 64);
                  lin_addr
              } else {
                  eff_addr
              }
          };
          if not_bool(canonical_address_p(signed(lin_addr))) then {
              (Some(":NON-CANONICAL-ADDRESS"), 0x0000000000000000)
          } else {
              (None() : option(string), lin_addr)
          }
      },
      1 => {
          let (elem0, elem1) : (option(string), bits(32)) = {
              if not_bool(seg_reg == 1) & not_bool(seg_reg == 2) & unsigned(seg_visibles[seg_reg]) < 4 then {
                  (Some(":NULL-SEGMENT-SELECTOR"), 0x00000000)
              } else {
                  let (base, lower_bound, upper_bound) : (bits(64), int, int) = segment_base_and_bounds(proc_mode, seg_reg);
                  let base : bits(32) = truncate(base, 32);
                  let lower_bound : bits(33) = bits_of_int(lower_bound, 33);
                  let upper_bound : bits(32) = bits_of_int(upper_bound, 32);
                  let first_addr : sbits(64) = eff_addr;
                  let last_addr : int = signed(eff_addr) + (nbytes + -1);
                  if not_bool(unsigned(lower_bound) <= signed(first_addr) & last_addr <= unsigned(upper_bound)) then {
                      (Some(":SEGMENT-LIMIT-FAIL"), 0x00000000)
                  } else {
                      let lin_addr : bits(32) = bits_of_int(unsigned(base) + unsigned(truncate(eff_addr, 32)), 32);
                      (None() : option(string), lin_addr)
                  }
              }
          };
          (elem0, sail_zero_extend(elem1, 64))
      },
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), 0x0000000000000000)
    }) : (option(string), sbits(64))
}

val ia32e_valid_code_segment_descriptor_p : bits(64) -> (bool, (string, bits(64)))

function ia32e_valid_code_segment_descriptor_p descriptor = {
    if not_bool(Mk_code_segment_descriptorbits(descriptor)[msb_of_type] == 0b1) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(Mk_code_segment_descriptorbits(descriptor)[s] == 0b1) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(Mk_code_segment_descriptorbits(descriptor)[p] == 0b1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(Mk_code_segment_descriptorbits(descriptor)[l] == 0b1) then {
        (false, (":IA32E-MODE-OFF", descriptor))
    } else if not_bool(Mk_code_segment_descriptorbits(descriptor)[d] == 0b0) then {
        (false, (":IA32E-DEFAULT-OPERAND-SIZE-INCORRECT", descriptor))
    } else {
        (true, ("", 0x0000000000000000))
    }
}

val ia32e_valid_ldt_segment_descriptor_p : bits(128) -> (bool, (string, bits(128)))

function ia32e_valid_ldt_segment_descriptor_p descriptor = {
    let sailtype : bits(4) = Mk_system_segment_descriptorbits(descriptor)[sailtype];
    if not_bool(sailtype == 0x2) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(Mk_system_segment_descriptorbits(descriptor)[s] == 0b0) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(Mk_system_segment_descriptorbits(descriptor)[p] == 0b1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(Mk_system_segment_descriptorbits(descriptor)[all_zeroes?] == 0b00000) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", 0x00000000000000000000000000000000))
    }
}

val ia32e_valid_call_gate_segment_descriptor_p : bits(128) -> (bool, (string, bits(128)))

function ia32e_valid_call_gate_segment_descriptor_p descriptor = {
    let sailtype : bits(4) = Mk_call_gate_descriptorbits(descriptor)[sailtype];
    if not_bool(sailtype == 0xc) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(Mk_call_gate_descriptorbits(descriptor)[s] == 0b0) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(Mk_call_gate_descriptorbits(descriptor)[p] == 0b1) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(Mk_call_gate_descriptorbits(descriptor)[all_zeroes?] == 0b00000) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", 0x00000000000000000000000000000000))
    }
}

val make_code_segment_attr_field : bits(64) -> code_segment_descriptor_attributesbits

function make_code_segment_attr_field descriptor = {
    let a : bits(1) = Mk_code_segment_descriptorbits(descriptor)[a];
    let r : bits(1) = Mk_code_segment_descriptorbits(descriptor)[r];
    let c : bits(1) = Mk_code_segment_descriptorbits(descriptor)[c];
    let msb_of_type : bits(1) = Mk_code_segment_descriptorbits(descriptor)[msb_of_type];
    let s : bits(1) = Mk_code_segment_descriptorbits(descriptor)[s];
    let dpl : bits(2) = Mk_code_segment_descriptorbits(descriptor)[dpl];
    let p : bits(1) = Mk_code_segment_descriptorbits(descriptor)[p];
    let avl : bits(1) = Mk_code_segment_descriptorbits(descriptor)[avl];
    let l : bits(1) = Mk_code_segment_descriptorbits(descriptor)[l];
    let g : bits(1) = Mk_code_segment_descriptorbits(descriptor)[g];
    [Mk_code_segment_descriptor_attributesbits(0x0000) with a, r, c, msb_of_type, s, dpl, p, avl, l, g]
}

val make_system_segment_attr_field : bits(128) -> system_segment_descriptor_attributesbits

function make_system_segment_attr_field descriptor = {
    let sailtype : bits(4) = Mk_system_segment_descriptorbits(descriptor)[sailtype];
    let s : bits(1) = Mk_system_segment_descriptorbits(descriptor)[s];
    let dpl : bits(2) = Mk_system_segment_descriptorbits(descriptor)[dpl];
    let p : bits(1) = Mk_system_segment_descriptorbits(descriptor)[p];
    let avl : bits(1) = Mk_system_segment_descriptorbits(descriptor)[avl];
    let g : bits(1) = Mk_system_segment_descriptorbits(descriptor)[g];
    [Mk_system_segment_descriptor_attributesbits(0x0000) with sailtype, s, dpl, p, avl, g]
}
