$include <prelude.sail>
$include <string.sail>
$include "three_byte_opcodes_dispatch.sail"
/*Two-byte opcode dispatch function.*/
/*<p>@ ( ' two-byte-opcode-execute ' ) is the doorway to the two-byte 
 opcode map, and will lead to the three-byte opcode map if @ ( ' opcode ' ) is 
 either @ ( ' #x38 ' ) or @ ( ' #x3A ' ) .</p>*/
val two_byte_opcode_execute : (int, int, int, int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function two_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib, x86) =
(match opcode {
0 => if ((modr_m_get_reg(modr_m)) == (0)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (0)) & ((modr_m_get_mod(modr_m)) == (3))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & ((modr_m_get_mod(modr_m)) == (3))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_lldt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (4)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (5)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
1 => if ((modr_m_get_reg(modr_m)) == (0)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (0)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (1)))
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (0)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (2)))
then let fault_var = (if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (0)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (3)))
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (0)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (4)))
then let fault_var = (if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (0)))
then let fault_var = (if ((feature_flags_fn([|":MONITOR"|], 0)) == (0)) | (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (1)))
then let fault_var = (if ((feature_flags_fn([|":MONITOR"|], 0)) == (0)) | (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (2)))
then let fault_var = (if ((feature_flags_fn([|":SMAP"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (3)))
then let fault_var = (if ((feature_flags_fn([|":SMAP"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (7)))
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (2)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_lgdt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_reg(modr_m)) == (3)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_lidt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (0)))
then let fault_var = (if ((feature_flags_fn([|":XSAVE"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | ((cr4bits_get_osxsave(loghead(22, ctri(4, 0)))) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (1)))
then let fault_var = (if ((feature_flags_fn([|":XSAVE"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | ((cr4bits_get_osxsave(loghead(22, ctri(4, 0)))) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (4)))
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (5)))
then let fault_var = (if ((feature_flags_fn([|":RTM"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((102) == (prefixes_get_opr(prefixes))) | (((243) == (prefixes_get_rep(prefixes))) | ((242) == (prefixes_get_rep(prefixes))))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (6)))
then let fault_var = (if ((feature_flags_fn([|":HLE", ":RTM"|], 0)) == (0)) | ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (7)))
then let fault_var = (if (cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (4)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (4)) & ((modr_m_get_mod(modr_m)) == (3))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if (not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))) & ((cr4bits_get_umip(loghead(22, ctri(4, 0)))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (6)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (7)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((proc_mode) == (0)) & (((modr_m_get_reg(modr_m)) == (7)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (0))))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (7)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (1)))
then let fault_var = (if ((feature_flags_fn([|":RDTSCP"|], 0)) == (0)) | ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
2 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
3 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
6 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
8 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
9 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
11 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: UD2 encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
13 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
16 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movss_movsd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (242)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movss_movsd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
17 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movss_movsd_op_en_mr(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (242)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movss_movsd_op_en_mr(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
18 => if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (0))
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (0))
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (242)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
19 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
20 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_unpck?ps_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_unpck?pd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
21 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_unpck?ps_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_unpck?pd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
22 => if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (0))
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (0))
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE3"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
23 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if ((feature_flags_fn([|":SSE2"|], 0)) == (0)) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
24 => if ((modr_m_get_reg(modr_m)) == (0)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (1)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (2)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_reg(modr_m)) == (3)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (4)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (5)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (6)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (7)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_mod(modr_m)) == (3)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
26 => if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (0))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | (if (proc_mode) == (0)
then ((modr_m_get_mod(modr_m)) == (0)) & (((modr_m_get_r_m(modr_m)) == (5)) | (((modr_m_get_r_m(modr_m)) == (4)) & (((sib_get_base(sib)) == (5)) & ((sib_get_index(sib)) == (4)))))
else if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (0))
then let fault_var = (if (feature_flags_fn([|":MPX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (102))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | ((((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))) | (((4) <= (reg_index(modr_m_get_r_m(modr_m), rex_byte, 0))) | (if (not_bool((proc_mode) == (0))) & ((prefixes_get_adr(prefixes)) == (103))
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (102))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | ((((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))) | (((4) <= (reg_index(modr_m_get_r_m(modr_m), rex_byte, 0))) | (if (not_bool((proc_mode) == (0))) & ((prefixes_get_adr(prefixes)) == (103))
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (243)
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | ((not_bool((proc_mode) == (0))) & (if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0)))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (242)
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | ((not_bool((proc_mode) == (0))) & (if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0)))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
27 => if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (0))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | (if (proc_mode) == (0)
then ((modr_m_get_mod(modr_m)) == (0)) & (((modr_m_get_r_m(modr_m)) == (5)) | (((modr_m_get_r_m(modr_m)) == (4)) & (((sib_get_base(sib)) == (5)) & ((sib_get_index(sib)) == (4)))))
else if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (0))
then let fault_var = (if (feature_flags_fn([|":MPX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (102))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | ((((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))) | (((4) <= (reg_index(modr_m_get_r_m(modr_m), rex_byte, 0))) | (if (not_bool((proc_mode) == (0))) & ((prefixes_get_adr(prefixes)) == (103))
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (102))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | ((((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))) | (((4) <= (reg_index(modr_m_get_r_m(modr_m), rex_byte, 0))) | (if (not_bool((proc_mode) == (0))) & ((prefixes_get_adr(prefixes)) == (103))
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (not_bool((modr_m_get_mod(modr_m)) == (3))) & ((mandatory_prefix) == (243))
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | (if (proc_mode) == (0)
then ((modr_m_get_mod(modr_m)) == (0)) & (((modr_m_get_r_m(modr_m)) == (5)) | (((modr_m_get_r_m(modr_m)) == (4)) & (((sib_get_base(sib)) == (5)) & ((sib_get_index(sib)) == (4)))))
else if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if ((modr_m_get_mod(modr_m)) == (3)) & ((mandatory_prefix) == (243))
then let fault_var = (if (feature_flags_fn([|":MPX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (242)
then let fault_var = (if ((feature_flags_fn([|":MPX"|], 0)) == (0)) | (((240) == (prefixes_get_lck(prefixes))) | (((4) <= (reg_index(modr_m_get_reg(modr_m), rex_byte, 2))) | ((not_bool((proc_mode) == (0))) & (if (prefixes_get_adr(prefixes)) == (103)
then (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (1)
else (let cs_attr = (seg_hidden_attri(1, 0)) : int in
code_segment_descriptor_attributesbits_get_d(cs_attr)) == (0)))))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
28 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
29 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
30 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
31 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_nop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
32 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (let reg = (modr_m_get_reg(modr_m)) : int in
if ((proc_mode) == (0)) & (logbitp(2, rex_byte))
then not_bool((reg) == (0))
else ((reg) == (1)) | (((reg) == (5)) | (((reg) == (6)) | ((reg) == (7)))))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_control_regs_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
33 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (((cr4bits_get_de(loghead(22, ctri(4, 0)))) == (1)) & (((modr_m_get_reg(modr_m)) == (4)) | ((modr_m_get_reg(modr_m)) == (5))))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
34 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (let reg = (modr_m_get_reg(modr_m)) : int in
if ((proc_mode) == (0)) & (logbitp(2, rex_byte))
then not_bool((reg) == (0))
else ((reg) == (1)) | (((reg) == (5)) | (((reg) == (6)) | ((reg) == (7)))))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
35 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (((cr4bits_get_de(loghead(22, ctri(4, 0)))) == (1)) & (((modr_m_get_reg(modr_m)) == (4)) | ((modr_m_get_reg(modr_m)) == (5))))
then Some(":UD")
else if not_bool((let x86 = (0) : int in
segment_selectorbits_get_rpl(seg_visiblei(1, 0))) == (0))
then Some(":GP")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
40 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
41 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
64 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
65 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
66 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
67 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
68 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
69 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
70 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
71 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
72 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
73 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
74 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
75 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
76 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
77 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
78 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
79 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
110 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
111 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
127 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":MMX"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (243)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
128 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
129 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
130 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
131 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
132 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
133 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
134 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
135 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
136 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
137 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
138 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
139 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
140 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
141 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
142 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
143 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
144 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
145 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
146 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
147 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
148 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
149 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
150 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
151 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
152 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
153 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
154 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
155 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
156 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
157 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
158 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
159 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
160 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
163 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_bt_0f_a3(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
164 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
165 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
168 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
172 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
173 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
175 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_imul_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
176 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
177 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
182 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
183 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
184 => if (mandatory_prefix) == (0)
then throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (243)
then let fault_var = (if ((feature_flags_fn([|":POPCNT"|], 0)) == (0)) | ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
186 => if (modr_m_get_reg(modr_m)) == (4)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_bt_0f_ba(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (5)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (6)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
189 => if (mandatory_prefix) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (mandatory_prefix) == (243)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
190 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
191 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
192 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
198 => if (mandatory_prefix) == (0)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shufps_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (mandatory_prefix) == (102)
then let fault_var = (let chk_ex = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib, 0)) : option(string) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":SSE2"|], 0)) == (0)
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_shufpd_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
4 => throw(Emsg("Translation error: two-byte opcode 4 not translated")),
5 => throw(Emsg("Translation error: two-byte opcode 5 not translated")),
7 => throw(Emsg("Translation error: two-byte opcode 7 not translated")),
10 => throw(Emsg("Translation error: two-byte opcode 10 not translated")),
12 => throw(Emsg("Translation error: two-byte opcode 12 not translated")),
14 => throw(Emsg("Translation error: two-byte opcode 14 not translated")),
15 => throw(Emsg("Translation error: two-byte opcode 15 not translated")),
25 => throw(Emsg("Translation error: two-byte opcode 25 not translated")),
36 => throw(Emsg("Translation error: two-byte opcode 36 not translated")),
37 => throw(Emsg("Translation error: two-byte opcode 37 not translated")),
38 => throw(Emsg("Translation error: two-byte opcode 38 not translated")),
39 => throw(Emsg("Translation error: two-byte opcode 39 not translated")),
42 => throw(Emsg("Translation error: two-byte opcode 42 not translated")),
43 => throw(Emsg("Translation error: two-byte opcode 43 not translated")),
44 => throw(Emsg("Translation error: two-byte opcode 44 not translated")),
45 => throw(Emsg("Translation error: two-byte opcode 45 not translated")),
46 => throw(Emsg("Translation error: two-byte opcode 46 not translated")),
47 => throw(Emsg("Translation error: two-byte opcode 47 not translated")),
48 => throw(Emsg("Translation error: two-byte opcode 48 not translated")),
49 => throw(Emsg("Translation error: two-byte opcode 49 not translated")),
50 => throw(Emsg("Translation error: two-byte opcode 50 not translated")),
51 => throw(Emsg("Translation error: two-byte opcode 51 not translated")),
52 => throw(Emsg("Translation error: two-byte opcode 52 not translated")),
53 => throw(Emsg("Translation error: two-byte opcode 53 not translated")),
54 => throw(Emsg("Translation error: two-byte opcode 54 not translated")),
55 => throw(Emsg("Translation error: two-byte opcode 55 not translated")),
56 => throw(Emsg("Translation error: two-byte opcode 56 not translated")),
57 => throw(Emsg("Translation error: two-byte opcode 57 not translated")),
58 => throw(Emsg("Translation error: two-byte opcode 58 not translated")),
59 => throw(Emsg("Translation error: two-byte opcode 59 not translated")),
60 => throw(Emsg("Translation error: two-byte opcode 60 not translated")),
61 => throw(Emsg("Translation error: two-byte opcode 61 not translated")),
62 => throw(Emsg("Translation error: two-byte opcode 62 not translated")),
63 => throw(Emsg("Translation error: two-byte opcode 63 not translated")),
80 => throw(Emsg("Translation error: two-byte opcode 80 not translated")),
81 => throw(Emsg("Translation error: two-byte opcode 81 not translated")),
82 => throw(Emsg("Translation error: two-byte opcode 82 not translated")),
83 => throw(Emsg("Translation error: two-byte opcode 83 not translated")),
84 => throw(Emsg("Translation error: two-byte opcode 84 not translated")),
85 => throw(Emsg("Translation error: two-byte opcode 85 not translated")),
86 => throw(Emsg("Translation error: two-byte opcode 86 not translated")),
87 => throw(Emsg("Translation error: two-byte opcode 87 not translated")),
88 => throw(Emsg("Translation error: two-byte opcode 88 not translated")),
89 => throw(Emsg("Translation error: two-byte opcode 89 not translated")),
90 => throw(Emsg("Translation error: two-byte opcode 90 not translated")),
91 => throw(Emsg("Translation error: two-byte opcode 91 not translated")),
92 => throw(Emsg("Translation error: two-byte opcode 92 not translated")),
93 => throw(Emsg("Translation error: two-byte opcode 93 not translated")),
94 => throw(Emsg("Translation error: two-byte opcode 94 not translated")),
95 => throw(Emsg("Translation error: two-byte opcode 95 not translated")),
96 => throw(Emsg("Translation error: two-byte opcode 96 not translated")),
97 => throw(Emsg("Translation error: two-byte opcode 97 not translated")),
98 => throw(Emsg("Translation error: two-byte opcode 98 not translated")),
99 => throw(Emsg("Translation error: two-byte opcode 99 not translated")),
100 => throw(Emsg("Translation error: two-byte opcode 100 not translated")),
101 => throw(Emsg("Translation error: two-byte opcode 101 not translated")),
102 => throw(Emsg("Translation error: two-byte opcode 102 not translated")),
103 => throw(Emsg("Translation error: two-byte opcode 103 not translated")),
104 => throw(Emsg("Translation error: two-byte opcode 104 not translated")),
105 => throw(Emsg("Translation error: two-byte opcode 105 not translated")),
106 => throw(Emsg("Translation error: two-byte opcode 106 not translated")),
107 => throw(Emsg("Translation error: two-byte opcode 107 not translated")),
108 => throw(Emsg("Translation error: two-byte opcode 108 not translated")),
109 => throw(Emsg("Translation error: two-byte opcode 109 not translated")),
112 => throw(Emsg("Translation error: two-byte opcode 112 not translated")),
113 => throw(Emsg("Translation error: two-byte opcode 113 not translated")),
114 => throw(Emsg("Translation error: two-byte opcode 114 not translated")),
115 => throw(Emsg("Translation error: two-byte opcode 115 not translated")),
116 => throw(Emsg("Translation error: two-byte opcode 116 not translated")),
117 => throw(Emsg("Translation error: two-byte opcode 117 not translated")),
118 => throw(Emsg("Translation error: two-byte opcode 118 not translated")),
119 => throw(Emsg("Translation error: two-byte opcode 119 not translated")),
120 => throw(Emsg("Translation error: two-byte opcode 120 not translated")),
121 => throw(Emsg("Translation error: two-byte opcode 121 not translated")),
122 => throw(Emsg("Translation error: two-byte opcode 122 not translated")),
123 => throw(Emsg("Translation error: two-byte opcode 123 not translated")),
124 => throw(Emsg("Translation error: two-byte opcode 124 not translated")),
125 => throw(Emsg("Translation error: two-byte opcode 125 not translated")),
126 => throw(Emsg("Translation error: two-byte opcode 126 not translated")),
161 => throw(Emsg("Translation error: two-byte opcode 161 not translated")),
162 => throw(Emsg("Translation error: two-byte opcode 162 not translated")),
166 => throw(Emsg("Translation error: two-byte opcode 166 not translated")),
167 => throw(Emsg("Translation error: two-byte opcode 167 not translated")),
169 => throw(Emsg("Translation error: two-byte opcode 169 not translated")),
170 => throw(Emsg("Translation error: two-byte opcode 170 not translated")),
171 => throw(Emsg("Translation error: two-byte opcode 171 not translated")),
174 => throw(Emsg("Translation error: two-byte opcode 174 not translated")),
178 => throw(Emsg("Translation error: two-byte opcode 178 not translated")),
179 => throw(Emsg("Translation error: two-byte opcode 179 not translated")),
180 => throw(Emsg("Translation error: two-byte opcode 180 not translated")),
181 => throw(Emsg("Translation error: two-byte opcode 181 not translated")),
185 => throw(Emsg("Translation error: two-byte opcode 185 not translated")),
187 => throw(Emsg("Translation error: two-byte opcode 187 not translated")),
188 => throw(Emsg("Translation error: two-byte opcode 188 not translated")),
193 => throw(Emsg("Translation error: two-byte opcode 193 not translated")),
194 => throw(Emsg("Translation error: two-byte opcode 194 not translated")),
195 => throw(Emsg("Translation error: two-byte opcode 195 not translated")),
196 => throw(Emsg("Translation error: two-byte opcode 196 not translated")),
197 => throw(Emsg("Translation error: two-byte opcode 197 not translated")),
199 => throw(Emsg("Translation error: two-byte opcode 199 not translated")),
200 => throw(Emsg("Translation error: two-byte opcode 200 not translated")),
201 => throw(Emsg("Translation error: two-byte opcode 201 not translated")),
202 => throw(Emsg("Translation error: two-byte opcode 202 not translated")),
203 => throw(Emsg("Translation error: two-byte opcode 203 not translated")),
204 => throw(Emsg("Translation error: two-byte opcode 204 not translated")),
205 => throw(Emsg("Translation error: two-byte opcode 205 not translated")),
206 => throw(Emsg("Translation error: two-byte opcode 206 not translated")),
207 => throw(Emsg("Translation error: two-byte opcode 207 not translated")),
208 => throw(Emsg("Translation error: two-byte opcode 208 not translated")),
209 => throw(Emsg("Translation error: two-byte opcode 209 not translated")),
210 => throw(Emsg("Translation error: two-byte opcode 210 not translated")),
211 => throw(Emsg("Translation error: two-byte opcode 211 not translated")),
212 => throw(Emsg("Translation error: two-byte opcode 212 not translated")),
213 => throw(Emsg("Translation error: two-byte opcode 213 not translated")),
214 => throw(Emsg("Translation error: two-byte opcode 214 not translated")),
215 => throw(Emsg("Translation error: two-byte opcode 215 not translated")),
216 => throw(Emsg("Translation error: two-byte opcode 216 not translated")),
217 => throw(Emsg("Translation error: two-byte opcode 217 not translated")),
218 => throw(Emsg("Translation error: two-byte opcode 218 not translated")),
219 => throw(Emsg("Translation error: two-byte opcode 219 not translated")),
220 => throw(Emsg("Translation error: two-byte opcode 220 not translated")),
221 => throw(Emsg("Translation error: two-byte opcode 221 not translated")),
222 => throw(Emsg("Translation error: two-byte opcode 222 not translated")),
223 => throw(Emsg("Translation error: two-byte opcode 223 not translated")),
224 => throw(Emsg("Translation error: two-byte opcode 224 not translated")),
225 => throw(Emsg("Translation error: two-byte opcode 225 not translated")),
226 => throw(Emsg("Translation error: two-byte opcode 226 not translated")),
227 => throw(Emsg("Translation error: two-byte opcode 227 not translated")),
228 => throw(Emsg("Translation error: two-byte opcode 228 not translated")),
229 => throw(Emsg("Translation error: two-byte opcode 229 not translated")),
230 => throw(Emsg("Translation error: two-byte opcode 230 not translated")),
231 => throw(Emsg("Translation error: two-byte opcode 231 not translated")),
232 => throw(Emsg("Translation error: two-byte opcode 232 not translated")),
233 => throw(Emsg("Translation error: two-byte opcode 233 not translated")),
234 => throw(Emsg("Translation error: two-byte opcode 234 not translated")),
235 => throw(Emsg("Translation error: two-byte opcode 235 not translated")),
236 => throw(Emsg("Translation error: two-byte opcode 236 not translated")),
237 => throw(Emsg("Translation error: two-byte opcode 237 not translated")),
238 => throw(Emsg("Translation error: two-byte opcode 238 not translated")),
239 => throw(Emsg("Translation error: two-byte opcode 239 not translated")),
240 => throw(Emsg("Translation error: two-byte opcode 240 not translated")),
241 => throw(Emsg("Translation error: two-byte opcode 241 not translated")),
242 => throw(Emsg("Translation error: two-byte opcode 242 not translated")),
243 => throw(Emsg("Translation error: two-byte opcode 243 not translated")),
244 => throw(Emsg("Translation error: two-byte opcode 244 not translated")),
245 => throw(Emsg("Translation error: two-byte opcode 245 not translated")),
246 => throw(Emsg("Translation error: two-byte opcode 246 not translated")),
247 => throw(Emsg("Translation error: two-byte opcode 247 not translated")),
248 => throw(Emsg("Translation error: two-byte opcode 248 not translated")),
249 => throw(Emsg("Translation error: two-byte opcode 249 not translated")),
250 => throw(Emsg("Translation error: two-byte opcode 250 not translated")),
251 => throw(Emsg("Translation error: two-byte opcode 251 not translated")),
252 => throw(Emsg("Translation error: two-byte opcode 252 not translated")),
253 => throw(Emsg("Translation error: two-byte opcode 253 not translated")),
254 => throw(Emsg("Translation error: two-byte opcode 254 not translated")),
255 => throw(Emsg("Translation error: two-byte opcode 255 not translated")),
_ => throw(Emsg("Translation error: invalid two-byte opcode"))
}) : int

/*Decoder and dispatch function for two-byte opcodes*/
/*<p>Source: Intel Manual, Volume 2, Appendix A-2</p>*/
val two_byte_opcode_decode_and_execute : (int, int, int, int, int, int, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function two_byte_opcode_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, escape_byte, x86) =
let ctx = ("two-byte-opcode-decode-and-execute") : string in
let (flg0, opcode, x86) = (rme08(proc_mode, temp_rip, 1, ":X", 0)) : (option(string), int, int) in
if is_some(flg0)
then throw(Emsg("Model state error: :OPCODE-BYTE-ACCESS-ERROR"))
else let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else let mandatory_prefix = (compute_mandatory_prefix_for_two_byte_opcode(proc_mode, opcode, prefixes)) : int in
let modr_m? = (two_byte_opcode_modr_m_p(proc_mode, mandatory_prefix, opcode)) : bool in
let (flg1, modr_m, x86) = (if modr_m?
then rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg1)
then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"))
else let (flg, temp_rip) = (if modr_m?
then add_to_iptr(proc_mode, temp_rip, 1, 0)
else (None() : option(string), temp_rip)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else let sib? = ((modr_m?) & (let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let n16_bit_addressp = ((2) == (select_address_size(proc_mode, p4?, 0))) : bool in
x86_decode_sib_p(modr_m, n16_bit_addressp))) : bool in
let (flg2, sib, x86) = (if sib?
then rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg2)
then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"))
else let (flg, temp_rip) = (if sib?
then add_to_iptr(proc_mode, temp_rip, 1, 0)
else (None() : option(string), temp_rip)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib, 0)

