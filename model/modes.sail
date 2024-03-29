$include "./register_readers_and_writers.sail"

/*! Check whether we are in 64-bit mode.
<p> 
 Given the modeling assumption stated in @ ( see x86-modes ) , 
 this predicate discriminates between 
 64-bit mode and the other two modes ( collectively, 32-bit mode ) . 
 Based on Intel manual, Mar ' 17, Vol. 3A, Sec. 2.2 ( near Fig. 2-3 ) , 
 the discrimination is based on the IA32_EFER.LME and CS.L bits: 
 if they are both 1, we are in 64-bit mode, 
 otherwise we are in 32-bit mode 
 ( protected mode if IA32_EFER.LME is 0, 
 compatibility mode if IA32_EFER.LME is 1 and CS.L is 0; 
 note that when IA32_EFER.LME is 0, CS.L should be 0, 
 according to Intel manual, Mar ' 17, Vol. 3A, Sec. 3.4.5 ) . 
 </p> 
 <p> 
 This predicate does not include state invariants such as 
 the constraints imposed by the 64-bit mode consistency checks 
 described in Intel manual, Mar ' 17, Vol. 3A, Sec. 9.8.5. 
 </p> 
 <p> 
 This predicate is useful as a hypothesis of theorems 
 about either 64-bit or 32-bit mode. 
 </p> 
 <p> 
 Since @ ( ' ( xr :msr ... x86 ) ' ) returns a 64-bit value 
 but the IA32_EFER register consists of 12 bits. 
 So we use @ ( tsee n12 ) to make @ ( ' ia32_eferBits ' ) functions applicable. 
 </p> */
function n64_bit_modep () = {
    let ia32_efer : bits(12) = truncate(read_msr(0), 12);
    let ia32_efer_lma : bits(1) = Mk_ia32_eferbits(ia32_efer)[lma];
    let cs_attr : bits(16) = seg_hidden_attrs[1];
    let cs_l : bits(1) = Mk_code_segment_descriptor_attributesbits(cs_attr)[l];
    ia32_efer_lma == 0b1 & cs_l == 0b1
}
