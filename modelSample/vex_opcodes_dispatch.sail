$ifndef __X86_VEX_OPCODES_DISPATCH
$define __X86_VEX_OPCODES_DISPATCH
$include <prelude.sail>
$include <string.sail>



/*Dispatch function for VEX-encoded instructions in the two-byte opcode map*/
val vex_0f_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit effect {escape}
function vex_0f_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) =
(match opcode {
16 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
17 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
18 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((modr_m)[mod]) == (0b11))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
19 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
20 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
21 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
22 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((modr_m)[mod]) == (0b11))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
23 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
40 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
41 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
42 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
43 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
44 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
45 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
46 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
47 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
65 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
66 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
68 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
69 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
70 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
71 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
74 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
75 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
80 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
81 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
82 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
83 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
84 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
85 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
86 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
87 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
88 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
89 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
90 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
91 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
92 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
93 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
94 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
95 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
96 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
97 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
98 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
99 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
100 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
101 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
102 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
103 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
104 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
105 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
106 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
107 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
108 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
109 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
110 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
111 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
112 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
113 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b100))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b100))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
114 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b100))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b100))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
115 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b011))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b011))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b110))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b111))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[reg]) == (0b111))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
116 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
117 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
118 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
119 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-8", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-8", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
120 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
121 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
122 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
123 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
124 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
125 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
126 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
127 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
144 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
145 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K21", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
146 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
147 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
152 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
153 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
174 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((modr_m)[reg]) == (0b010))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((vex_get_pp(vex_prefixes)) == (0b000)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((modr_m)[reg]) == (0b011))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
194 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
196 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
197 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
198 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
208 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
209 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
210 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
211 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
212 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
213 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
214 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
215 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
216 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
217 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
218 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
219 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
220 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
221 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
222 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
223 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
224 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
225 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
226 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
227 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
228 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
229 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
230 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
231 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
232 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
233 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
234 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
235 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
236 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
237 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
238 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
239 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
240 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
241 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
242 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
243 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
244 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
245 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
246 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
247 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
248 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
249 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
250 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
251 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
252 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
253 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
254 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
_ => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
}) : unit

/*Dispatch function for VEX-encoded instructions in the first 
 three-byte opcode map*/
val vex_0f38_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit effect {escape}
function vex_0f38_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) =
(match opcode {
0 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
1 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
2 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
3 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
4 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
5 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
6 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
7 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
8 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
9 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
10 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
11 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
12 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
13 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
14 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
15 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
16 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
17 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
18 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
19 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-11", [|":F16C", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":F16C", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-11", [|":F16C", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":F16C", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
20 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
21 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
22 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
23 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
24 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
25 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
26 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
27 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
28 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
29 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
30 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
31 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
32 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
33 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
34 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
35 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
36 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
37 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
38 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
39 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
40 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
41 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
42 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-1", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
43 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
44 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
45 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
46 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
47 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
48 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
49 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
50 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
51 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
52 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
53 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
54 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
55 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
56 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
57 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
58 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
59 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
60 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
61 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
62 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
63 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
64 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
65 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
66 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
67 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
68 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
69 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
70 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
71 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
76 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
77 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
78 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
79 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
82 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
83 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
88 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
89 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
90 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
91 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
100 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
101 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
102 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
117 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
118 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
119 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
120 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
121 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-7", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
122 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
123 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
124 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
125 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
126 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
127 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
131 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
136 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
137 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
138 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
139 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
140 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
141 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
142 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (not_bool(((modr_m)[mod]) == (0b11))))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
144 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
145 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
146 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
147 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-12", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
150 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
151 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
152 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
153 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
154 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
155 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
156 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
157 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
158 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
159 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
160 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
161 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
162 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
163 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
166 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
167 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
168 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
169 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
170 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
171 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
172 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
173 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
174 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
175 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
180 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
181 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
182 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
183 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
184 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
185 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
186 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
187 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
188 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
189 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
190 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
191 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":FMA", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":FMA", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
196 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
198 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
199 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
200 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
202 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
203 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
204 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
205 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
219 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
220 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
221 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
222 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
223 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
242 => { if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
243 => { if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[reg]) == (0b001)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[reg]) == (0b001)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[reg]) == (0b010)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[reg]) == (0b010)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[reg]) == (0b011)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[reg]) == (0b011)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
245 => { if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
246 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
247 => { if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((vex_get_pp(vex_prefixes)) == (0b000)) & ((not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI1", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI1", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x2)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
_ => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
}) : unit

/*Dispatch function for VEX-encoded instructions in the second 
 three-byte opcode map*/
val vex_0f3a_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes, bits(8), modr_m, sib) -> unit effect {escape}
function vex_0f3a_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib) =
(match opcode {
0 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
1 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
2 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
3 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
4 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
5 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
6 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
8 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
9 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
10 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
11 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-3", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
12 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
13 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
14 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
15 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
20 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
21 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
22 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
23 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
24 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
25 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
26 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
27 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
29 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-11", [|":F16C", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":F16C", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-11", [|":F16C", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":F16C", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
30 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
31 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
32 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
33 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (not_bool(((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((modr_m)[mod]) == (0b11))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
34 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-5", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
35 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
37 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
38 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
39 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
48 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
49 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
50 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512DQ"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512F"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
51 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (((modr_m)[mod]) == (0b11)))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-K20", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX512BW"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
56 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
57 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
58 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
59 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
62 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
63 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
64 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
65 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-2", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
66 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
67 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
68 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":PCLMULQDQ", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":PCLMULQDQ", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
70 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-6", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
74 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
75 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
76 => { if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if (not_bool((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111))) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b001)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX2"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
80 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
81 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
84 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
85 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
86 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
87 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
96 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
97 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
98 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
99 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
102 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
103 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
223 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((vex_get_pp(vex_prefixes)) == (0b001)) & (false)))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-4", [|":AES", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":AES", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
240 => { if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b0)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else if ((sail_sign_extend(vex_get_vvvv(vex_prefixes), 6)) == (0b00_1111)) & (((sail_sign_extend(vex_get_l(vex_prefixes), 3)) == (0b000)) & (((sail_sign_extend(vex_get_pp(vex_prefixes), 4)) == (0x3)) & (((vex_get_w(vex_prefixes)) == (0b1)) & (false))))
then let fault_var : option(string) = { (let chk_ex : option(string) = (chk_exc_fn(":LEGACY", ":TYPE-13", [|":BMI2", ":AVX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib)) in
if is_some(chk_ex)
then chk_ex
else if (feature_flags_fn([|":BMI2", ":AVX"|])) == (0)
then Some(":UD")
else None() : option(string)) } in
if is_some(fault_var)
then { (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
_ => ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}) : unit
}
else { ext_vex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, opcode, modr_m, sib)
}
else throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")) },
_ => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
}) : unit

/*<p>@ ( ' vex-decode-and-execute ' ) dispatches control to VEX-encoded 
 instructions.</p> 
 
 <p><i>Reference: Intel Vol. 2A, Section 2.3: Intel Advanced Vector 
 Extensions ( Intel AVX ) </i></p>*/
val vex_decode_and_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), vex_prefixes) -> unit effect {escape}
function vex_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes) =
let ctx : string = ("vex-decode-and-execute") in
{ if not_bool((rex_byte) == (0x00))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :rex rex-byte)"))
else ();
if ((prefixes)[lck]) == (0xf0)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :lock-prefix)"))
else ();
if ((prefixes)[rep]) == (0xf2)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :F2-prefix)"))
else ();
if ((prefixes)[rep]) == (0xf3)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :F3-prefix)"))
else ();
if ((prefixes)[opr]) == (0x66)
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :66-prefix)"))
else ();
let vex2_prefix? : bool = (((vex_prefixes)[byte0]) == (0xc5)) in
let vex3_prefix? : bool = (((vex_prefixes)[byte0]) == (0xc4)) in
let vex_byte1 : bits(8) = ((vex_prefixes)[byte1]) in
let (vex3_0f_map?, vex3_0f38_map?, vex3_0f3a_map?) : (bool, bool, bool) = { (if vex3_prefix?
then (((Mk_vex3_byte1(vex_byte1))[m_mmmm]) == (0b0_0001), ((Mk_vex3_byte1(vex_byte1))[m_mmmm]) == (0b0_0010), ((Mk_vex3_byte1(vex_byte1))[m_mmmm]) == (0b0_0011))
else (false, false, false)) } in
{ if (vex3_prefix?) & (not_bool((vex3_0f_map?) | ((vex3_0f38_map?) | (vex3_0f3a_map?))))
then throw(Emsg("A fault occurred.  Original ACL2 AST: (!!fault-fresh :ud :vex-prefixes vex-prefixes :m-mmmm vex-byte1)"))
else ();
let (flg0, byte2_next_byte) : (option(string), bits(8)) = (rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")) in
{ if is_some(flg0)
then throw(Emsg("Model state error: :VEX-BYTE2/NEXT-BYTE-READ-ERROR"))
else ();
let (flg1, temp_rip) : (option(string), sbits(49)) = (add_to_iptr(proc_mode, temp_rip, 0x0000_0000_0001)) in
{ if is_some(flg1)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
let vex_prefixes : vex_prefixes = { (if vex3_prefix?
then [(vex_prefixes) with byte2 = (byte2_next_byte)]
else vex_prefixes) } in
let (flg2, next_byte) : (option(string), bits(8)) = { (if vex3_prefix?
then rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
else (None() : option(string), 0x00)) } in
{ if is_some(flg2)
then throw(Emsg("Model state error: :NEXT-BYTE-READ-ERROR"))
else ();
let (flg3, temp_rip) : (option(string), sbits(49)) = { (if vex3_prefix?
then add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x0000_0000_0001)
else (None() : option(string), temp_rip)) } in
{ if is_some(flg3)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
let opcode : bits(8) = { (if vex3_prefix?
then next_byte
else byte2_next_byte) } in
let modr_m? : bool = (vex_opcode_modr_m_p(vex_prefixes, opcode)) in
let (flg4, modr_m) : (option(string), bits(8)) = { (if modr_m?
then rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
else (None() : option(string), 0x00)) } in
{ if is_some(flg4)
then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"))
else ();
let (flg5, temp_rip) : (option(string), sbits(49)) = { (if modr_m?
then add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x0000_0000_0001)
else (None() : option(string), temp_rip)) } in
{ if is_some(flg5)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
let sib? : bool = ((modr_m?) & (let p4? : bool = ((0x67) == ((prefixes)[adr])) in
let n16_bit_addressp : bool = ((2) == (select_address_size(proc_mode, p4?))) in
x86_decode_sib_p(Mk_modr_m(modr_m), n16_bit_addressp))) in
let (flg6, sib) : (option(string), bits(8)) = { (if sib?
then rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
else (None() : option(string), 0x00)) } in
{ if is_some(flg6)
then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"))
else ();
let (flg7, temp_rip) : (option(string), sbits(49)) = { (if sib?
then add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x0000_0000_0001)
else (None() : option(string), temp_rip)) } in
{ if is_some(flg7)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else ();
if vex_prefixes_map_p(0x000f, vex_prefixes)
then vex_0f_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, rex_byte, vex_prefixes, opcode, Mk_modr_m(modr_m), Mk_sib(sib))
else if vex_prefixes_map_p(0x0f38, vex_prefixes)
then vex_0f38_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, rex_byte, vex_prefixes, opcode, Mk_modr_m(modr_m), Mk_sib(sib))
else if vex_prefixes_map_p(0x0f3a, vex_prefixes)
then vex_0f3a_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, rex_byte, vex_prefixes, opcode, Mk_modr_m(modr_m), Mk_sib(sib))
else throw(Emsg("Model state error: :ILLEGAL-VALUE-OF-VEX-M-MMMM"))
}
}
}
}
}
}
}
}
}
}

$endif
