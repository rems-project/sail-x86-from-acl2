$ifndef __X86_TWO_BYTE_OPCODES_DISPATCH
$define __X86_TWO_BYTE_OPCODES_DISPATCH
$include <prelude.sail>
$include <string.sail>
$include "three_byte_opcodes_dispatch.sail"
/*Two-byte opcode dispatch function.*/
/*<p>@ ( ' two-byte-opcode-execute ' ) is the doorway to the two-byte 
 opcode map, and will lead to the three-byte opcode map if @ ( ' opcode ' ) is 
 either @ ( ' #x38 ' ) or @ ( ' #x3A ' ) .</p>*/
val two_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), bits(8), modr_m, sib) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function two_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib) =
(match opcode {
0 => { if (((modr_m)[reg]) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b000)) & (((modr_m)[mod]) == (0b11))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & (((modr_m)[mod]) == (0b11))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b010)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_lldt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b011)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b100)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b101)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
1 => { if (((modr_m)[reg]) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b000)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b001)))
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if (((modr_m)[reg]) == (0b000)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b010)))
then let fault_var : option(string) = { (if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b000)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b011)))
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if (((modr_m)[reg]) == (0b000)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b100)))
then let fault_var : option(string) = { (if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b000)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MONITOR"|])) == (0)) | (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0)))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b001)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MONITOR"|])) == (0)) | (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0)))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b010)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":SMAP"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b011)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":SMAP"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b111)))
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if (((modr_m)[reg]) == (0b010)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_lgdt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_lidt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b000)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":XSAVE"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[osxsave]) == (0b0)))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b001)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":XSAVE"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[osxsave]) == (0b0)))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b100)))
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b101)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":RTM"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((0x66) == ((prefixes)[opr])) | (((0xf3) == ((prefixes)[rep])) | ((0xf2) == ((prefixes)[rep])))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b110)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":HLE", ":RTM"|])) == (0)) | ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b111)))
then let fault_var : option(string) = { (if ((Mk_cr0bits(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)))[ts]) == (0b1)
then Some(":NM")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b100)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b100)) & (((modr_m)[mod]) == (0b11))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if (not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))) & (((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[umip]) == (0b0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b110)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b111)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((proc_mode) == (0)) & ((((modr_m)[reg]) == (0b111)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b000))))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b111)) & ((((modr_m)[mod]) == (0b11)) & (((modr_m)[r_m]) == (0b001)))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":RDTSCP"|])) == (0)) | ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
2 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
3 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
5 => { if (proc_mode) == (0)
then let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | (((Mk_ia32_eferbits(truncate(msrs[0], 12)))[sce]) == (0b0))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_syscall_both_views(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
6 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
7 => { if (proc_mode) == (0)
then let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | (((Mk_ia32_eferbits(truncate(msrs[0], 12)))[sce]) == (0b0))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_sysret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
8 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
9 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
11 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"UD2 encountered!\" START-RIP TEMP-RIP X86)")),
13 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
16 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movss_movsd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf2)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movss_movsd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
17 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movss_movsd_op_en_mr(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf2)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movss_movsd_op_en_mr(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
18 => { if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf2)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
19 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
20 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_unpck?ps_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_unpck?pd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
21 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_unpck?ps_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_unpck?pd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
22 => { if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
23 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|])) == (0)) | (((modr_m)[mod]) == (0b11))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
24 => { if (((modr_m)[reg]) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b010)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[reg]) == (0b011)) & (not_bool(((modr_m)[mod]) == (0b11)))
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b100)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if ((modr_m)[reg]) == (0b101)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if ((modr_m)[reg]) == (0b110)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if ((modr_m)[reg]) == (0b111)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if ((modr_m)[mod]) == (0b11)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
25 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
26 => { if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | (if (proc_mode) == (0)
then (((modr_m)[mod]) == (0b00)) & ((((modr_m)[r_m]) == (0b101)) | ((((modr_m)[r_m]) == (0b100)) & ((((sib)[base]) == (0b101)) & (((sib)[index]) == (0b100)))))
else if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (if (feature_flags_fn([|":MPX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x66))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))) | (((4) <= (unsigned(reg_index((modr_m)[r_m], rex_byte, 0b00)))) | (if (not_bool((proc_mode) == (0))) & (((prefixes)[adr]) == (0x67))
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x66))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))) | (((4) <= (unsigned(reg_index((modr_m)[r_m], rex_byte, 0b00)))) | (if (not_bool((proc_mode) == (0))) & (((prefixes)[adr]) == (0x67))
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | ((not_bool((proc_mode) == (0))) & (if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0)))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf2)
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | ((not_bool((proc_mode) == (0))) & (if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0)))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
27 => { if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | (if (proc_mode) == (0)
then (((modr_m)[mod]) == (0b00)) & ((((modr_m)[r_m]) == (0b101)) | ((((modr_m)[r_m]) == (0b100)) & ((((sib)[base]) == (0b101)) & (((sib)[index]) == (0b100)))))
else if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x00))
then let fault_var : option(string) = { (if (feature_flags_fn([|":MPX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0x66))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))) | (((4) <= (unsigned(reg_index((modr_m)[r_m], rex_byte, 0b00)))) | (if (not_bool((proc_mode) == (0))) & (((prefixes)[adr]) == (0x67))
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0x66))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))) | (((4) <= (unsigned(reg_index((modr_m)[r_m], rex_byte, 0b00)))) | (if (not_bool((proc_mode) == (0))) & (((prefixes)[adr]) == (0x67))
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (not_bool(((modr_m)[mod]) == (0b11))) & ((mandatory_prefix) == (0xf3))
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | (if (proc_mode) == (0)
then (((modr_m)[mod]) == (0b00)) & ((((modr_m)[r_m]) == (0b101)) | ((((modr_m)[r_m]) == (0b100)) & ((((sib)[base]) == (0b101)) & (((sib)[index]) == (0b100)))))
else if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (((modr_m)[mod]) == (0b11)) & ((mandatory_prefix) == (0xf3))
then let fault_var : option(string) = { (if (feature_flags_fn([|":MPX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf2)
then let fault_var : option(string) = { (if ((feature_flags_fn([|":MPX"|])) == (0)) | (((0xf0) == ((prefixes)[lck])) | (((4) <= (unsigned(reg_index((modr_m)[reg], rex_byte, 0b10)))) | ((not_bool((proc_mode) == (0))) & (if ((prefixes)[adr]) == (0x67)
then (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b1)
else (let cs_attr : bits(16) = (seg_hidden_attrs[1]) in
(Mk_code_segment_descriptor_attributesbits(cs_attr))[d]) == (0b0)))))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
28 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
29 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
30 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
31 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_nop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
32 => { let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | (let reg : bits(3) = ((modr_m)[reg]) in
if ((proc_mode) == (0)) & (logbitp(2, rex_byte))
then not_bool((reg) == (0b000))
else ((reg) == (0b001)) | (((reg) == (0b101)) | (((reg) == (0b110)) | ((reg) == (0b111)))))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_mov_control_regs_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
33 => { let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | ((((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[de]) == (0b1)) & ((((modr_m)[reg]) == (0b100)) | (((modr_m)[reg]) == (0b101))))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
34 => { let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | (let reg : bits(3) = ((modr_m)[reg]) in
if ((proc_mode) == (0)) & (logbitp(2, rex_byte))
then not_bool((reg) == (0b000))
else ((reg) == (0b001)) | (((reg) == (0b101)) | (((reg) == (0b110)) | ((reg) == (0b111)))))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
35 => { let fault_var : option(string) = { (if ((0xf0) == ((prefixes)[lck])) | ((((Mk_cr4bits(bits_of_int(loghead(22, unsigned(ctrs[4])), 22)))[de]) == (0b1)) & ((((modr_m)[reg]) == (0b100)) | (((modr_m)[reg]) == (0b101))))
then Some(":UD")
else if not_bool((unsigned({ ();
(Mk_segment_selectorbits(seg_visibles[1]))[rpl]
})) == (0))
then Some(":GP")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
40 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
41 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
64 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
65 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
66 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
67 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
68 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
69 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
70 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
71 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
72 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
73 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
74 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
75 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
76 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
77 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
78 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
79 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
110 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movd_movq_to_xmm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
111 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
127 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
128 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
129 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
130 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
131 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
132 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
133 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
134 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
135 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
136 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
137 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
138 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
139 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
140 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
141 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
142 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
143 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
144 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
145 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
146 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
147 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
148 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
149 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
150 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
151 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
152 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
153 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
154 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
155 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
156 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
157 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
158 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
159 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
160 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
163 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_bt_0f_a3(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
164 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
165 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
168 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
172 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
173 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
175 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_imul_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
176 => { let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
177 => { let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
182 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
183 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
184 => { if (mandatory_prefix) == (0x00)
then ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (if ((feature_flags_fn([|":POPCNT"|])) == (0)) | ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
186 => { if ((modr_m)[reg]) == (0b100)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_bt_0f_ba(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b101)
then let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b110)
then let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if ((modr_m)[reg]) == (0b111)
then let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
189 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0xf3)
then let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
190 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
191 => { let fault_var : option(string) = { (if (0xf0) == ((prefixes)[lck])
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
} },
192 => { let fault_var : option(string) = { (if (((modr_m)[mod]) == (0b11)) & ((0xf0) == ((prefixes)[lck]))
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
} },
198 => { if (mandatory_prefix) == (0x00)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shufps_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else if (mandatory_prefix) == (0x66)
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit
}
else { x86_shufpd_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
4 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
10 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
12 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
14 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
15 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
36 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
37 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
38 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
39 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
42 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
43 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
44 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
45 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
46 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
47 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
48 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
49 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
50 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
51 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
52 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
53 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
54 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
55 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
56 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
57 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
58 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
59 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
60 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
61 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
62 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
63 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
80 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
81 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
82 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
83 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
84 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
85 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
86 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
87 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
88 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
89 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
90 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
91 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
92 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
93 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
94 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
95 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
96 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
97 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
98 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
99 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
100 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
101 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
102 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
103 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
104 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
105 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
106 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
107 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
108 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
109 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
112 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
113 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
114 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
115 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
116 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
117 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
118 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
119 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
120 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
121 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
122 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
123 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
124 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
125 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
126 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
161 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
162 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
166 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
167 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
169 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
170 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
171 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
174 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
178 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
179 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
180 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
181 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
185 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
187 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
188 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
193 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
194 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
195 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
196 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
197 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
199 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
200 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
201 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
202 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
203 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
204 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
205 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
206 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
207 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
208 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
209 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
210 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
211 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
212 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
213 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
214 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
215 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
216 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
217 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
218 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
219 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
220 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
221 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
222 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
223 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
224 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
225 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
226 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
227 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
228 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
229 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
230 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
231 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
232 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
233 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
234 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
235 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
236 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
237 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
238 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
239 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
240 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
241 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
242 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
243 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
244 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
245 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
246 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
247 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
248 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
249 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
250 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
251 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
252 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
253 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
254 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
255 => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib),
_ => ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}) : unit

/*Decoder and dispatch function for two-byte opcodes*/
/*<p>Source: Intel Manual, Volume 2, Appendix A-2</p>*/
val two_byte_opcode_decode_and_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8)) -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function two_byte_opcode_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, escape_byte) =
let ctx : string = ("two-byte-opcode-decode-and-execute") in
let (flg0, opcode) : (option(string), bits(8)) = (rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :OPCODE-BYTE-ACCESS-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
{ if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
let mandatory_prefix : bits(8) = (compute_mandatory_prefix_for_two_byte_opcode(proc_mode, opcode, prefixes)) in
let modr_m? : bool = (two_byte_opcode_modr_m_p(proc_mode, mandatory_prefix, opcode)) in
let (flg1, modr_m) : (option(string), bits(8)) = { (if modr_m?
then rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
else (None() : option(string), 0x00)) } in
{ if is_some(flg1)
then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = { (if modr_m?
then add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x0000_0000_0001)
else (None() : option(string), temp_rip)) } in
{ if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
let sib? : bool = ((modr_m?) & (let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let n16_bit_addressp : bool = ((2) == (select_address_size(proc_mode, Some(prefixes)))) in
x86_decode_sib_p(Mk_modr_m(modr_m), n16_bit_addressp))) in
let (flg2, sib) : (option(string), bits(8)) = { (if sib?
then rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
else (None() : option(string), 0x00)) } in
{ if is_some(flg2)
then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"))
else ();
let (flg, temp_rip) : (option(string), sbits(49)) = { (if sib?
then add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x0000_0000_0001)
else (None() : option(string), temp_rip)) } in
{ if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
two_byte_opcode_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, mandatory_prefix, rex_byte, opcode, Mk_modr_m(modr_m), Mk_sib(sib))
}
}
}
}
}
}

$endif
