$include "./register_readers_and_writers.sail"

val n64_bit_modep : unit -> bool

function n64_bit_modep () = {
    let ia32_efer = n12(the_bits(12, msrs[0])) : bits(12);
    let ia32_efer_lma = ia32_eferbits_get_lma(ia32_efer) : bits(1);
    let cs_attr = seg_hidden_attrs[1] : bits(16);
    let cs_l = code_segment_descriptor_attributesbits_get_l(cs_attr) : bits(1);
    the_bits(8, ia32_efer_lma) == get_slice_int(8, 1, 0) & the_bits(8, cs_l) == get_slice_int(8, 1, 0)
}

val x86_operation_mode : unit -> {|0, 1|}

function x86_operation_mode () = {
    if n64_bit_modep() then 0 else 1
}
