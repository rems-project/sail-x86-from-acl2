$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <string.sail>
$include "handwritten2.sail"
$include "utils.sail"

$include "top.sail"
$include "two_byte_opcodes_dispatch.sail"

/*Fetch and store legacy and REX prefixes, if any, of an instruction*/
/*<p>The function @ ( ' get-prefixes ' ) fetches the legacy and REX prefixes 
 of an instruction and also returns the first byte following the last such 
 prefix. The input @ ( ' start-rip ' ) points to the first byte of an instruction, 
 which may potentially be a legacy prefix. The initial value of @ ( ' cnt ' ) 
 should be @ ( ' 15 ' ) so that the result @ ( ' ( - 15 cnt ) ' ) returned at the end of 
 the recursion is the correct number of legacy and/or REX bytes parsed by this 
 function.</p> 
 
 <h3>Legacy Prefixes</h3> 
 
 <p>From Intel Manual, Vol. 2, May 2018, Section 2.1.1 ( Instruction 
 Prefixes ) :</p> 
 
 <p><em>Instruction prefixes are divided into four groups, each with a set of 
 allowable prefix codes. For each instruction, it is only useful to include 
 up to one prefix code from each of the four groups ( Groups 1, 2, 3, 
 4 ) . Groups 1 through 4 may be placed in any order relative to each 
 other.</em></p> 
 
 <p>Despite the quote from the Intel Manual above, the order of the legacy 
 prefixes does matter when there is more than one prefix from the same group 
 --- <b>all but the last prefix from a single prefix group are ignored</b>. 
 The only <b>exception</b> in this case is for <b>Group 1</b> prefixes --- see 
 below for details.</p> 
 
 <ul> 
 <li>@ ( ' 0x64_88_00 ' ) is @ ( ' mov byte ptr fs:[rax], al ' ) </li> 
 <li>@ ( ' 0x65_88_00 ' ) is @ ( ' mov byte ptr gs:[rax], al ' ) </li> 
 <li>@ ( ' 0x64_65_88_00 ' ) is @ ( ' mov byte ptr gs:[rax], al ' ) </li> 
 <li>@ ( ' 0x65_64_88_00 ' ) is @ ( ' mov byte ptr fs:[rax], al ' ) </li> 
 </ul> 
 
 <ul> 
 <li>@ ( ' 0xf2_a4 ' ) is @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf3_a4 ' ) is @ ( ' repe movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf2_f3_a4 ' ) is @ ( ' repe movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf3_f2_a4 ' ) is @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 </ul> 
 
 <p>We now discuss the Group 1 exception below.</p> 
 
 <p>@ ( ' 0xf0_f2_a4 ' ) is <b>NOT</b> <br/> 
 @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) <br/> 
 It is: <br/> 
 @ ( ' lock repne movsb byte ptr [rdi], byte ptr [rsi] ' ) <br/> 
 
 Note that lock and rep/repne are Group 1 prefixes. It is important to record 
 the lock prefix, even if it is overshadowed by a rep/repne prefix, because 
 the former instruction will not @ ( ' #UD ' ) , but the latter instruction will. 
 This is akin to the lock prefix being in a separate group than the rep/repne 
 prefixes; in fact, AMD manuals ( Section 1.2.1: Summary of Legacy Prefixes, 
 Vol. 3 May 2018 Edition ) treat them as such.</p> 
 
 <p>For details about how mandatory prefixes are picked from legacy prefixes, 
 see @ ( see mandatory-prefixes-computation ) .</p> 
 
 <h3>REX Prefixes</h3> 
 
 <p>A REX prefix ( applicable only to 64-bit mode ) is treated as a null prefix 
 if it is followed by a legacy prefix. Here is an illustrative example ( using 
 Intel ' s XED, x86 Encoder Decoder --- see 
 @ ( ' https://intelxed.github.io/ ' ) ) :</p> 
 
 <ul> 
 
 <li>@ ( ' xed -64 -d 48670100 ' ) is @ ( ' add dword ptr [eax], eax ' ) ; the REX.W 
 prefix does not have any effect on the operand size, which remains 32 ( i.e., 
 the default operand size in the 64-bit mode ) .</li> 
 
 <li>@ ( ' xed -64 -d 67480100 ' ) is @ ( ' add qword ptr [eax], rax ' ) ; the REX prefix 
 has the intended effect of promoting the operand size to 64 bits.</li> 
 
 </ul> 
 
 <p>Note that the prefixes structure output of this function does not include 
 the REX byte ( which is a separate return value of this function ) , but its 
 @ ( ' :num-prefixes ' ) field includes a count of the REX prefixes encountered. 
 This is because adding an 8-bit field to the prefixes structure to store a 
 REX byte will make it a bignum, thereby impacting execution efficiency.</p>*/
val get_prefixes : (int, int, int, int, int, int) -> (option(string), int, int, int) effect {rreg, rmem, escape}
function get_prefixes (proc_mode, start_rip, prefixes, rex_byte, cnt, x86) =
if (cnt) == (0)
then (Some("This error was 't' in ACL2 but a Some(string) in Sail"), prefixes, rex_byte, 0 : int)
else let ctx = ("get-prefixes") : string in
let (flg, byte, x86) = (rme08(proc_mode, start_rip, 1, ":X", 0)) : (option(string), int, int) in
if is_some(flg)
then (Some(ctx), byte, rex_byte, 0 : int)
else let prefix_byte_group_code = (get_one_byte_prefix_array_code(byte)) : int in
(match prefix_byte_group_code {
0 => let rex? = (((proc_mode) == (0)) & ((ash(byte, -4)) == (4))) : bool in
if rex?
then let (flg, next_rip) = (add_to_iptr(proc_mode, start_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, prefixes, rex_byte, 0 : int)
else get_prefixes(proc_mode, next_rip, prefixes, byte, (cnt) - (1), 0)
else let prefixes = (set_prefixes_get_nxt(byte, prefixes)) : int in
(None() : option(string), set_prefixes_get_num((15) - (cnt), prefixes), rex_byte, 0 : int),
1 => let (flg, next_rip) = (add_to_iptr(proc_mode, start_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, prefixes, rex_byte, 0 : int)
else let prefixes = (if (byte) == (240)
then set_prefixes_get_lck(byte, prefixes)
else set_prefixes_get_rep(byte, prefixes)) : int in
get_prefixes(proc_mode, next_rip, prefixes, 0, (cnt) - (1), 0),
2 => let (flg, next_rip) = (add_to_iptr(proc_mode, start_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, prefixes, rex_byte, 0 : int)
else if (((proc_mode) == (0)) & (((byte) == (100)) | ((byte) == (101)))) | (not_bool((proc_mode) == (0)))
then get_prefixes(proc_mode, next_rip, set_prefixes_get_seg(byte, prefixes), 0, (cnt) - (1), 0)
else get_prefixes(proc_mode, next_rip, prefixes, 0, (cnt) - (1), 0),
3 => let (flg, next_rip) = (add_to_iptr(proc_mode, start_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, prefixes, rex_byte, 0 : int)
else get_prefixes(proc_mode, next_rip, set_prefixes_get_opr(byte, prefixes), 0, (cnt) - (1), 0),
4 => let (flg, next_rip) = (add_to_iptr(proc_mode, start_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then (flg, prefixes, rex_byte, 0 : int)
else get_prefixes(proc_mode, next_rip, set_prefixes_get_adr(byte, prefixes), 0, (cnt) - (1), 0),
_ => (Some("This error was 't' in ACL2 but a Some(string) in Sail"), prefixes, rex_byte, 0 : int)
}) : (option(string), int, int, int)

/*Top-level dispatch function.*/
/*<p>@ ( ' one-byte-opcode-execute ' ) is the doorway to all the opcode 
 maps ( for non-AVX/AVX512 instructions ) .</p>*/
val one_byte_opcode_execute : (int, int, int, int, int, int, int, int, int) -> int effect {rreg, undef, wmv, rmem, eamem, escape, wreg}
function one_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, x86) =
(match opcode {
0 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
1 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
2 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
3 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
4 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
5 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
6 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: PUSH ES is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
7 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: POP ES is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
8 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
9 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
10 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
11 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
12 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
13 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
14 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: PUSH CS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
15 => two_byte_opcode_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, 0),
16 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
17 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
18 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
19 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
20 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
21 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
22 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: PUSH SS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
23 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: POP SS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
24 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
25 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
26 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
27 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
28 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
29 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
30 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: PUSH DS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
31 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: POP DS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
32 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
33 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
34 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
35 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
36 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
37 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
38 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
39 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: DAA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
40 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
41 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
42 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
43 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
44 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
45 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
46 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
47 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: DAS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
48 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
49 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
50 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
51 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
52 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
53 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
54 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
55 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: AAA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
56 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
57 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
58 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
59 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
60 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
61 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
62 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
63 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: AAS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
64 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
65 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
66 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
67 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
68 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
69 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
70 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
71 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
72 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
73 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
74 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
75 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
76 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
77 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
78 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
79 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
80 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
81 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
82 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
83 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
84 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
85 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
86 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
87 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
88 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
89 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
90 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
91 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
92 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
93 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
94 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
95 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
96 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pusha(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: PUSHA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
97 => if not_bool((proc_mode) == (0))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_popa(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: POPA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
98 => if not_bool((proc_mode) == (0))
then let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | ((modr_m_get_mod(modr_m)) == (3))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
else if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
99 => if (proc_mode) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movsx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if not_bool((proc_mode) == (0))
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
100 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
101 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
102 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
103 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
104 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
105 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
106 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
107 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
108 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
109 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
110 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
111 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
112 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
113 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
114 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
115 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
116 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
117 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
118 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
119 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
120 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
121 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
122 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
123 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
124 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
125 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
126 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
127 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
128 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (4)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
129 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (4)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
130 => if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (0))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (1))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (2))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (3))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (4))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (5))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (6))
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (not_bool((proc_mode) == (0))) & ((modr_m_get_reg(modr_m)) == (7))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: Opcode 0x82 is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
131 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (4)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
132 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
133 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
134 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
135 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
136 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
137 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
138 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
139 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
140 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
141 => let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) | ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_lea(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
142 => let fault_var = (if ((modr_m_get_reg(modr_m)) == (1)) | ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
143 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pop_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
144 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
145 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
146 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
147 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
148 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
149 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
150 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
151 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
152 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cbw_cwd_cdqe(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
153 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cwd_cdq_cqo(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
154 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: far CALL is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
155 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
156 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_pushf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
157 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_popf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
158 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (((proc_mode) == (0)) & ((feature_flag_fn(":LAHF-SAHF", 0)) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
159 => let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | (((proc_mode) == (0)) & ((feature_flag_fn(":LAHF-SAHF", 0)) == (0)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_lahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
160 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
161 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
162 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
163 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
164 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
165 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
166 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
167 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
168 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
169 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
170 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
171 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
172 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
173 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
174 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
175 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
176 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
177 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
178 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
179 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
180 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
181 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
182 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
183 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
184 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
185 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
186 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
187 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
188 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
189 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
190 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
191 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
192 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
193 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
194 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
195 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
196 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | ((modr_m_get_mod(modr_m)) == (3))
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
197 => if (proc_mode) == (0)
then throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else if not_bool((proc_mode) == (0))
then let fault_var = (if ((240) == (prefixes_get_lck(prefixes))) | ((modr_m_get_mod(modr_m)) == (3))
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
198 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_reg(modr_m)) == (7)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (0)))
then let fault_var = (if ((feature_flags_fn([|":RTM"|], 0)) == (0)) | ((240) == (prefixes_get_lck(prefixes)))
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
199 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_reg(modr_m)) == (7)) & (((modr_m_get_mod(modr_m)) == (3)) & ((modr_m_get_r_m(modr_m)) == (0)))
then let fault_var = (if ((feature_flags_fn([|":RTM"|], 0)) == (0)) | ((240) == (prefixes_get_lck(prefixes)))
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
200 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
201 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_leave(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
202 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
203 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
204 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
205 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
206 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: INTO is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
207 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
208 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
209 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
210 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
211 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
212 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: AAM is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
213 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: AAD is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
215 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
216 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
217 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
218 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
219 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
220 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
221 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
222 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
223 => let fault_var = (if ((cr0bits_get_ts(loghead(32, ctri(0, 0)))) == (1)) | ((cr0bits_get_em(loghead(32, ctri(0, 0)))) == (1))
then Some(":NM")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
224 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
225 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
226 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
227 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_jrcxz(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
228 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
229 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
230 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
231 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
232 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_call_e8_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
233 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
234 => if not_bool((proc_mode) == (0))
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
else if (proc_mode) == (0)
then let fault_var = (if true
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: JMP is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
235 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
236 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
237 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
238 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
239 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
240 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
241 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
242 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
243 => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
244 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
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
else x86_hlt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
245 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
246 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (6)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
247 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
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
else x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (6)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (7)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
248 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
249 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
250 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
251 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
252 => x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
253 => let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0),
254 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
255 => if (modr_m_get_reg(modr_m)) == (0)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (1)
then let fault_var = (if ((modr_m_get_mod(modr_m)) == (3)) & ((240) == (prefixes_get_lck(prefixes)))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (2)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_call_ff_2_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (3)
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
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_near_jmp_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if ((modr_m_get_reg(modr_m)) == (5)) & (not_bool((modr_m_get_mod(modr_m)) == (3)))
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_far_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else if (modr_m_get_reg(modr_m)) == (6)
then let fault_var = (if (240) == (prefixes_get_lck(prefixes))
then Some(":UD")
else None() : option(string)) : option(string) in
if is_some(fault_var)
then (match fault_var {
Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
_ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
}) : int
else x86_push_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib, 0)
else throw(Emsg("A fault occurred.  Original ACL2 AST:
['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
214 => throw(Emsg("Translation error: one-byte opcode 214 not translated")),
_ => throw(Emsg("Translation error: invalid one-byte opcode"))
}) : int

/*Top-level step function*/
/*<p>@ ( ' x86-fetch-decode-execute ' ) is the step function of our x86 
 interpreter. It fetches one instruction by looking up the memory address 
 indicated by the instruction pointer @ ( ' rip ' ) , decodes that instruction, and 
 dispatches control to the appropriate instruction semantic function.</p>*/
val x86_fetch_decode_execute : (int) -> int effect {rreg, undef, wmv, wreg, rmem, eamem, escape}
function x86_fetch_decode_execute (x86) =
let ctx = ("x86-fetch-decode-execute") : string in
if (ms(0)) | (fault(0))
then 0
else let proc_mode = (x86_operation_mode(0)) : int in
let n64_bit_modep_var = ((proc_mode) == (0)) : bool in
let start_rip = (read_iptr(proc_mode, 0)) : int in
let (flg, prefixes, rex_byte, x86) = (get_prefixes(proc_mode, start_rip, 0, 0, 15, 0)) : (option(string), int, int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :ERROR-IN-READING-PREFIXES"))
else let opcode_vex_evex_byte = (prefixes_get_nxt(prefixes)) : int in
let prefix_length = (prefixes_get_num(prefixes)) : int in
let (flg, temp_rip) = (add_to_iptr(proc_mode, start_rip, (1) + (prefix_length), 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else let vex_byte0? = (((opcode_vex_evex_byte) == (197)) | ((opcode_vex_evex_byte) == (196))) : bool in
let (flg, les_lds_distinguishing_byte : int, x86) = (if vex_byte0?
then rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :LES/LDS-DISTINGUISHING-BYTE-READ-ERROR"))
else if (vex_byte0?) & ((n64_bit_modep_var) | ((not_bool(n64_bit_modep_var)) & ((unsigned(get_slice_int((7) - (6), les_lds_distinguishing_byte : int, 6))) == (3))))
then let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :VEX-BYTE1-INCREMENT-ERROR"))
else let vex_prefixes = (set_vex_prefixes_get_byte0(opcode_vex_evex_byte, 0)) : int in
let vex_prefixes = (set_vex_prefixes_get_byte1(les_lds_distinguishing_byte : int, vex_prefixes)) : int in
unimplemented_vex_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes, 0)
else let opcode_evex_byte = (opcode_vex_evex_byte) : int in
let evex_byte0? = ((opcode_evex_byte) == (98)) : bool in
let (flg, bound_distinguishing_byte, x86) = (if evex_byte0?
then rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :BOUND-DISTINGUISHING-BYTE-READ-ERROR"))
else if (evex_byte0?) & ((n64_bit_modep_var) | ((not_bool(n64_bit_modep_var)) & ((unsigned(get_slice_int((7) - (6), bound_distinguishing_byte, 6))) == (3))))
then let (flg, temp_rip) = (add_to_iptr(proc_mode, temp_rip, 1, 0)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :EVEX-BYTE1-INCREMENT-ERROR"))
else let evex_prefixes = (set_evex_prefixes_get_byte0(opcode_evex_byte, 0)) : int in
let evex_prefixes = (set_evex_prefixes_get_byte1(bound_distinguishing_byte, evex_prefixes)) : int in
unimplemented_evex_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, 0)
else let opcode_byte = (opcode_evex_byte) : int in
let modr_m? = (one_byte_opcode_modr_m_p(proc_mode, opcode_byte)) : bool in
let (flg, modr_m, x86) = (if modr_m?
then if (vex_byte0?) | (evex_byte0?)
then (None() : option(string), les_lds_distinguishing_byte : int, 0 : int)
else rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"))
else let (flg, temp_rip) = (if modr_m?
then add_to_iptr(proc_mode, temp_rip, 1, 0)
else (None() : option(string), temp_rip)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else let sib? = ((modr_m?) & (let p4? = ((103) == (prefixes_get_adr(prefixes))) : bool in
let n16_bit_addressp = ((2) == (select_address_size(proc_mode, p4?, 0))) : bool in
x86_decode_sib_p(modr_m, n16_bit_addressp))) : bool in
let (flg, sib, x86) = (if sib?
then rme08(proc_mode, temp_rip, 1, ":X", 0)
else (None() : option(string), 0 : int, 0 : int)) : (option(string), int, int) in
if is_some(flg)
then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"))
else let (flg, temp_rip) = (if sib?
then add_to_iptr(proc_mode, temp_rip, 1, 0)
else (None() : option(string), temp_rip)) : (option(string), int) in
if is_some(flg)
then throw(Emsg("Model state error: :INCREMENT-ERROR"))
else one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode_byte, modr_m, sib, 0)

