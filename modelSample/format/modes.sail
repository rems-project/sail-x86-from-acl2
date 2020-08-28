$include "./register_readers_and_writers.sail"

val n64_bit_modep : int -> bool effect {escape, rreg}

function n64_bit_modep x86 = {
    let ia32_efer = n12(msri(0, 0)) : int;
    let ia32_efer_lma = ia32_eferbits_get_lma(ia32_efer) : int;
    let cs_attr = seg_hidden_attri(1, 0) : int;
    let cs_l = code_segment_descriptor_attributesbits_get_l(cs_attr) : int;
    ia32_efer_lma == 1 & cs_l == 1
}

val x86_operation_mode : int -> int effect {escape, rreg}

function x86_operation_mode x86 = {
    0
    //if n64_bit_modep(0) then 0 else 1
}
