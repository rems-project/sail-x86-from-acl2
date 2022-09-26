$include "./register_readers_and_writers.sail"

val n64_bit_modep : unit -> bool

function n64_bit_modep () = {
    let ia32_efer : bits(12) = truncate(msrs[0], 12);
    let ia32_efer_lma : bits(1) = Mk_ia32_eferbits(ia32_efer)[lma];
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    let cs_l : bits(1) = Mk_code_segment_descriptor_attributesbits(cs_attr)[l];
    ia32_efer_lma == 0b1 & cs_l == 0b1
}

val x86_operation_mode : unit -> {|0, 1|}

function x86_operation_mode () = {
    if n64_bit_modep() then 0 else 1
}
