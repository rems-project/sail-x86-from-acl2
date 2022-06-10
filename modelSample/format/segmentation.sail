$include "./segmentation_structures.sail"
$include "./linear_memory.sail"

val segment_base_and_bounds : (range(0, 4), range(0, 5)) -> (bits(64), int, int)

function segment_base_and_bounds (proc_mode, seg_reg) = {
    (match proc_mode {
      0 => if get_slice_int(8, seg_reg, 0) == get_slice_int(8, 4, 0) then {
          (msrs[1], 0 : {|0|}, 0 : {|0|})
      } else if get_slice_int(8, seg_reg, 0) == get_slice_int(8, 5, 0) then {
          (msrs[2], 0 : {|0|}, 0 : {|0|})
      } else {
          (get_slice_int(64, 0, 0), 0 : {|0|}, 0 : {|0|})
      },
      1 => let (elem0, elem1, elem2) =
        (let base = loghead(64, unsigned(seg_hidden_bases[seg_reg])) : int in
        let limit = loghead(32, unsigned(seg_hidden_limits[seg_reg])) : int in
        let attr = loghead(16, unsigned(seg_hidden_attrs[seg_reg])) : int in
        let d_b =
          (if seg_reg == 1 then {
              code_segment_descriptor_attributesbits_get_d(get_slice_int(16, attr, 0))
          } else {
              data_segment_descriptor_attributesbits_get_d_b(get_slice_int(16, attr, 0))
          }) : bits(1)
        in
        let e =
          (if seg_reg == 1 then {
              get_slice_int(1, 0, 0)
          } else {
              data_segment_descriptor_attributesbits_get_e(get_slice_int(16, attr, 0))
          }) : bits(1)
        in
        let lower =
          (if unsigned(e) == 1 then {
              1 + limit
          } else 0) : int
        in
        let upper =
          (if unsigned(e) == 1 then {
              if unsigned(d_b) == 1 then 4294967295 else 65535
          } else {
              limit
          }) : int
        in
          (n32(get_slice_int(32, base, 0)), lower, upper)) : (bits(32), int, int)
      in
        (the_bits(64, elem0), elem1, elem2),
      _ => (get_slice_int(64, 0, 0), 0 : {|0|}, 0 : {|0|})
    }) : (bits(64), int, int)
}

val ea_to_la : (range(0, 4), sbits(64), range(0, 5), {|1, 2, 4, 6, 8, 10, 16|}) -> (option(string), sbits(67))

function ea_to_la (proc_mode, eff_addr, seg_reg, nbytes) = {
    (match proc_mode {
      0 => let (elem0, elem1) =
        (let lin_addr =
          (if get_slice_int(8, seg_reg, 0) == get_slice_int(8, 4, 0) | get_slice_int(8, seg_reg, 0) == get_slice_int(8, 5, 0) then {
              let (base, _, _) = segment_base_and_bounds(0, seg_reg) : (bits(64), int, int);
              let lin_addr = I64(get_slice_int(64, unsigned(base) + unsigned(n64(eff_addr)), 0)) : sbits(64);
              lin_addr
          } else {
              eff_addr
          }) : sbits(64)
        in
          if not_bool(canonical_address_p(the_sbits(48, lin_addr))) then {
              (Some(":NON-CANONICAL-ADDRESS"), get_slice_int(65, 0, 0))
          } else {
              (None() : option(string), the_sbits(65, lin_addr))
          }) : (option(string), sbits(65))
      in
        (elem0, the_sbits(67, elem1)),
      1 => let (elem0, elem1) =
        (if not_bool(seg_reg == 1) & not_bool(seg_reg == 2) & unsigned(seg_visibles[seg_reg]) < 4 then {
            (Some(":NULL-SEGMENT-SELECTOR"), get_slice_int(32, 0, 0))
        } else {
            let (base, lower_bound, upper_bound) = segment_base_and_bounds(proc_mode, seg_reg) : (bits(64), int, int);
            let base = the_bits(32, base) : bits(32);
            let lower_bound = the_bits(33, lower_bound) : bits(33);
            let upper_bound = the_bits(32, upper_bound) : bits(32);
            let first_addr = eff_addr : sbits(64);
            let last_addr = (signed(eff_addr) + (nbytes + -1)) : int;
            if not_bool(unsigned(lower_bound) <= signed(first_addr) & last_addr <= unsigned(upper_bound)) then {
                (Some(":SEGMENT-LIMIT-FAIL"), get_slice_int(32, 0, 0))
            } else {
                let lin_addr = n32(get_slice_int(32, unsigned(base) + unsigned(the_bits(32, eff_addr)), 0)) : bits(32);
                (None() : option(string), lin_addr)
            }
        }) : (option(string), bits(32))
      in
        (elem0, the_bits(67, elem1)),
      _ => (Some(":UNIMPLEMENTED-PROC-MODE"), get_slice_int(67, 0, 0))
    }) : (option(string), sbits(67))
}

val ia32e_valid_code_segment_descriptor_p : bits(64) -> (bool, (string, bits(64)))

function ia32e_valid_code_segment_descriptor_p descriptor = {
    if not_bool(the_bits(8, code_segment_descriptorbits_get_msb_of_type(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(the_bits(8, code_segment_descriptorbits_get_s(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(the_bits(8, code_segment_descriptorbits_get_p(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(the_bits(8, code_segment_descriptorbits_get_l(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":IA32E-MODE-OFF", descriptor))
    } else if not_bool(the_bits(8, code_segment_descriptorbits_get_d(descriptor)) == get_slice_int(8, 0, 0)) then {
        (false, (":IA32E-DEFAULT-OPERAND-SIZE-INCORRECT", descriptor))
    } else {
        (true, ("", get_slice_int(64, 0, 0)))
    }
}

val ia32e_valid_ldt_segment_descriptor_p : bits(128) -> (bool, (string, bits(128)))

function ia32e_valid_ldt_segment_descriptor_p descriptor = {
    let sailtype = system_segment_descriptorbits_get_type(descriptor) : bits(4);
    if not_bool(the_bits(8, sailtype) == get_slice_int(8, 2, 0)) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(the_bits(8, system_segment_descriptorbits_get_s(descriptor)) == get_slice_int(8, 0, 0)) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(the_bits(8, system_segment_descriptorbits_get_p(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(the_bits(8, system_segment_descriptorbits_get_all_zeroes?(descriptor)) == get_slice_int(8, 0, 0)) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", get_slice_int(128, 0, 0)))
    }
}

val ia32e_valid_call_gate_segment_descriptor_p : bits(128) -> (bool, (string, bits(128)))

function ia32e_valid_call_gate_segment_descriptor_p descriptor = {
    let sailtype = call_gate_descriptorbits_get_type(descriptor) : bits(4);
    if not_bool(the_bits(8, sailtype) == get_slice_int(8, 12, 0)) then {
        (false, (":INVALID-TYPE", descriptor))
    } else if not_bool(the_bits(8, call_gate_descriptorbits_get_s(descriptor)) == get_slice_int(8, 0, 0)) then {
        (false, (":INVALID-SEGMENT-TYPE", descriptor))
    } else if not_bool(the_bits(8, call_gate_descriptorbits_get_p(descriptor)) == get_slice_int(8, 1, 0)) then {
        (false, (":SEGMENT-NOT-PRESENT", descriptor))
    } else if not_bool(the_bits(8, call_gate_descriptorbits_get_all_zeroes?(descriptor)) == get_slice_int(8, 0, 0)) then {
        (false, (":ALL-ZEROES-ABSENT", descriptor))
    } else {
        (true, ("", get_slice_int(128, 0, 0)))
    }
}

val make_code_segment_attr_field : bits(64) -> bits(16)

function make_code_segment_attr_field descriptor = {
    let a = code_segment_descriptorbits_get_a(descriptor) : bits(1);
    let r = code_segment_descriptorbits_get_r(descriptor) : bits(1);
    let c = code_segment_descriptorbits_get_c(descriptor) : bits(1);
    let msb_of_type = code_segment_descriptorbits_get_msb_of_type(descriptor) : bits(1);
    let s = code_segment_descriptorbits_get_s(descriptor) : bits(1);
    let dpl = code_segment_descriptorbits_get_dpl(descriptor) : bits(2);
    let p = code_segment_descriptorbits_get_p(descriptor) : bits(1);
    let avl = code_segment_descriptorbits_get_avl(descriptor) : bits(1);
    let l = code_segment_descriptorbits_get_l(descriptor) : bits(1);
    let g = code_segment_descriptorbits_get_g(descriptor) : bits(1);
    change_code_segment_descriptor_attributesbits(get_slice_int(16, 0, 0), Some(a), Some(r), Some(c), Some(msb_of_type), Some(s), Some(dpl), Some(p), Some(avl), Some(l), None(), Some(g), None())
}

val make_system_segment_attr_field : bits(128) -> bits(16)

function make_system_segment_attr_field descriptor = {
    let sailtype = system_segment_descriptorbits_get_type(descriptor) : bits(4);
    let s = system_segment_descriptorbits_get_s(descriptor) : bits(1);
    let dpl = system_segment_descriptorbits_get_dpl(descriptor) : bits(2);
    let p = system_segment_descriptorbits_get_p(descriptor) : bits(1);
    let avl = system_segment_descriptorbits_get_avl(descriptor) : bits(1);
    let g = system_segment_descriptorbits_get_g(descriptor) : bits(1);
    change_system_segment_descriptor_attributesbits(get_slice_int(16, 0, 0), Some(sailtype), Some(s), Some(dpl), Some(p), Some(avl), Some(g), None())
}
