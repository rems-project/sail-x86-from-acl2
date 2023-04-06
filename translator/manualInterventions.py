from lex_parse import ACL2quote, CodeTopLevel, pp_acl2
from SailASTelems import *
import specialTokens
import handwritten_tokens
import transform
import re

"""
Sometimes the heuristics used by the automatic areas of the translator are not
correct (although most of the time they are).  In these cases it is possible
to manually intervene.

Each function below represents a manual intervention.  Each function takes the
ACL2 AST and the current environment as input and can use any state therein to
decide whether the intervention should be applied.  Most of the time this
decision is simply made by matching on the current AST and by examining the
current function being defined (by using `env.getDefineSlot()`).

Each function returns a flag indicating if an intervention was applied (and if
it was, then the corresponding translated Sail AST).

Thus, the type of each function is:

	( [ACL2astElem], Env ) -> ( bool, [SailASTelem], Env ) 

An intervention itself will often be some change made to the ACL2 AST before
handing over to a specific translation function (recursing), but sometimes
translation may happen first before a change is made to the translated Sail
AST.

The functions are collected together in replacePatterns() at the bottom.

Two important manual interventions are as follows:

 1. In implemented_opcode()  we can choose which opcodes to include in the
	translation.
 2. x86_token() translates the x86 variable which is threaded through most of
	the model as a dummy unit literal as state in Sail is global.
"""


#
# Manual intervention functions
#

def x86_token(ACL2ast, env):
	"""
	In ACL2 a state variable is threaded through many function calls.  It
	follows the informal discipline of naming this variable `x86`.  In Sail
	the state is global.  We translate `x86` simply as `()` in the first
	instance, which will later get simplified away in _bstar_helper in most
	cases.
	"""
	if isinstance(ACL2ast, str) and ACL2ast.lower() == 'x86':
		return True, [SailUnitLit()], env


def and_macro(ACL2ast, env):
	"""
	Usually `AND` is used as we would expect: it returns a boolean.
	Technically, however, it returns its last argument if none of its
	arguments are nil.  Mostly this property isn't used, but in
	`check-instruction-length` it is, which is what we handle here.
	"""
	if env.getDefineSlot().lower() == 'check-instruction-length' and isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'and':
		# Assume we have `(and (> length 15) length)` which macro translates to `(IF (< '15 LENGTH) LENGTH 'NIL)`
		# Make the return type option(int)
		lengthBVfn = env.lookup('length')
		(lengthBV, env, _) = lengthBVfn([], env)
		lengthBV = coerceExpr(lengthBV[0], Sail_t_int())

		sailAST = SailIf(
			ifTerm=[SailApp(
				fn=SailHandwrittenFn(
					name='<',
					typ=Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_bool()),
					infix=True
				),
				actuals=[SailNumLit(15), lengthBV],
				infix=True
			)],
			thenTerm=[someHelper(lengthBV)],
			elseTerm=[noneHelper(lengthBV.getType())]
		)

		return True, [sailAST], env

def wb_1(ACL2ast, env):
	"""
	Make sure that the `t` of function `wb-1` returns an option(string) rather
	than bool.
	"""
	if env.getDefineSlot().lower() == 'wb-1' and isinstance(ACL2ast, str) and ACL2ast == 't':
		return True, [someHelper(SailStringLit("Empty error"))], env

def dispatch_creator(ACL2ast, env):
	"""
	For some reason we need to use `(include-book "dispatch-creator")` before
	using the function `CREATE-DISPATCH-FOR-OPCODES`.  Thus, we need to do
	this before the make-event in x86.lisp, which uses this function.
	"""
	if env.getFile() in ['x86', 'two-byte-opcodes-dispatch'] and isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'make-event':
		env.evalACL2(['include-book', '"projects/x86isa/machine/dispatch-creator"', ':dir', ':system'])
		sailAST, env, _ = specialTokens.tr_make_event(ACL2ast, env)

		return True, sailAST, env

def insert_one_byte_nop(ACL2ast, env):
	if isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and ACL2ast[0].lower() == 'def-inst' and ACL2ast[1].lower() == 'x86-two-byte-nop':
		one_byte_nop = transform.loadPatch('x86-one-byte-nop', handwritten_tokens.x86_one_byte_nop_fn.getType(), env)
		(two_byte_nop, env, _) = specialTokens.tr_def_inst(ACL2ast, env)
		return True, [one_byte_nop] + two_byte_nop, env
	else:
		return None

def call_one_byte_nop(pattern, expr, env):
	(patternSail, env, _) = transform.transformACL2asttoSail(pattern, env)
	if env.getDefineSlot().lower() == 'one-byte-opcode-execute' and isinstance(patternSail[0], SailNumLit) and patternSail[0].getNum() == 144:
		def patch_instr(acl2):
			if is_acl2_app(acl2, 'x86-xchg'):
				nop_call = ['x86-one-byte-nop'] + acl2[1:]
				return ['if', ['logbitp', '0', 'rex-byte'], acl2, nop_call]
			elif is_acl2_list(acl2):
				return [patch_instr(e) for e in acl2]
			else:
				return acl2
		expr = patch_instr(expr)
	(exprSail, env, _) = transform.transformACL2asttoSail(expr, env)
	return (exprSail, env)

def implemented_opcode(ACL2ast, env):
	"""
	The two large tables below represent the translated one- and two- byte
	opcodes.  Numbers can be removed to disable certain opcodes.
	"""
	# Note #1:	These opcodes are missing from the ACL2 one byte dispatch table
	# 			in the ACL2 model: 214
	# Note #2:	pusha (96) and popa (97) are not valid in 64 bit mode and so
	# 			could be removed.
	casesToExcludeOneByte = []

	# Note #1:	These opcodes are missing from the ACL2 two byte dispatch
	#           table: 4, 10, 12, 14, 15, 36-9, 54, 57, 59-63, 122, 123, 166,
	#           167, 255
	#           That gives 21 non-existent instruction and thus 235 overall.
	# Note #2:	Opcodes 5, 7 are related to syscalls.
	# Note #3:	Floating point opcodes we implement: 16, 17, 18, 19, 20, 21,
	#           22, 23, 40, 41, 111, 127, 198
	# Note #4:  56 and 58 are the three-byte opcode escape codes
	casesToExcludeTwoByte = [42, 44, 45, 46, 47, 56, 58, 81, 84, 85, 86, 87, 88, 89, 90, 92, 93, 94, 95, 116, 174, 188, 194, 199, 215, 219, 223, 235, 239]

	if env.getDefineSlot().lower() in ['one-byte-opcode-execute', 'two-byte-opcode-execute'] and \
			is_acl2_case_match(ACL2ast, match_var='opcode'):
		if env.getDefineSlot().lower() == 'one-byte-opcode-execute':
			casesToExclude = casesToExcludeOneByte
			msg = 'one'
		else:
			casesToExclude = casesToExcludeTwoByte
			msg = 'two'

		# Exclude the opcodes listed above
		newACL2ast = ACL2ast[:2] + [case for case in ACL2ast[2:-1] if int(case[0]) not in casesToExclude] + [ACL2ast[-1]]
		sailAST, env, _ = specialTokens.tr_case(newACL2ast, env, caseOverride=call_one_byte_nop)

		# Make the missing opcodes throw exceptions
		matchesList = sailAST[0].getMatches()
		for i in casesToExclude:
			matchesList.insert(-1, (SailNumLit(i), specialTokens.errorHelper(f"Translation error: {msg}-byte opcode {i} not translated")))

		return True, sailAST, env

def get_prefixes_flag(ACL2ast, env):
	"""
	Remove the error flag from return values of `get-prefixes`
	"""
	if env.getDefineSlot().lower() == 'get-prefixes' and \
			isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'mv' and \
			isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 't':
		# Hack: Remove error flag
		new_acl2 = [ACL2ast[0]] + ACL2ast[2:]
		sailAST, env, _ = specialTokens.tr_mv(new_acl2, env)
		return True, sailAST, env

def push_and_pop_errors(ACL2ast, env):
	"""
	`push-and-pop.lisp` and `subroutine.lisp` inspect error messages passed
	up from function calls. Under our approximation of errors we can't do
	this, so simply translate as a custom error.  Fortunately, all occurrences
	of `cond` in that file correspond to such patterns.  Ths same is true in
	`subroutine.lisp`.
	"""
	if env.getFile().lower() in ['push-and-pop', 'subroutine'] and \
			isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'cond':
		return True, [specialTokens.errorHelper(
			"This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`")], env


def x86_hlt_return_type(ACL2ast, env):
	"""
	Force return type of x86-hlt.
	"""
	if env.getDefineSlot().lower() == 'x86-hlt' and \
			isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == '!!ms-fresh':
		# Translate as normal
		sailAST, env, _ = specialTokens.tr_ms_fresh(ACL2ast, env)

		# Set type
		sailAST[0].getFn().setType(Sail_t_fn([], Sail_t_unit()))

		return True, sailAST, env


def seg_descriptor_type(ACL2ast, env):
	"""
	Force some types in `ia32e-valid-call-gate-segment-descriptor-p` etc.
	"""
	if env.getDefineSlot().lower() in ['ia32e-valid-call-gate-segment-descriptor-p', 'ia32e-valid-code-segment-descriptor-p',
								  'ia32e-valid-ldt-segment-descriptor-p']:
		if transform.listStartsWith(ACL2ast, ['mv', 't', '0'], convertCase=True):
			return True, [
				SailTuple([SailBoolLit(True), SailTuple([SailStringLit(""), SailTuple([SailNumLit(0)])])])], env

def div_type(ACL2ast, env):
	"""
	Force type of `idiv-spec` and `div-spec`.
	"""
	if (env.getDefineSlot().lower().startswith('idiv-spec-') or env.getDefineSlot().lower().startswith('div-spec-')) and \
		isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'list':

		return True, [SailBoolLit(True)], env


def chk_exc_fn_type(ACL2ast, env):
	"""
	Force type of `chk-exc-fn` to be option(string), and add a success case for `cond`.
	"""
	if env.getDefineSlot().lower() == 'chk-exc-fn':
		if isinstance(ACL2ast, str) and ACL2ast.lower() in [':ud', ':nm'] and env.peekContext2 != 'cond':
			sail, env, _ = specialTokens.tr_fault_fresh([ACL2ast], env)
			return True, sail, env
		elif isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'cond':
			sail, env, _ = specialTokens.tr_cond(ACL2ast + [['t', 'nil']], env)
			return True, sail, env

def bitvector_merges(ACL2ast, env):
	pattern = re.compile(r"(bitops::)?merge-(\d{1,3})-u(\d{1,3})s")
	if isinstance(ACL2ast, list) and len(ACL2ast) > 1:
		match = pattern.match(ACL2ast[0].lower())
		if match:
			nElems = int(match.group(2))
			nBits = int(match.group(3))
			elemTyp = Sail_t_bits(nBits)
			def aux(ACL2elems, env):
				sailElem, env, _ = transform.transformACL2asttoSail(ACL2elems[0], env)
				sailElem = coerceExpr(sailElem[0], elemTyp)
				if sailElem is None:
					sys.exit(f"bitvector_merge: cannot coerce {ACL2elems[0]} to {elemTyp.pp()}")
				if len(ACL2elems) > 1:
					# TODO: Left or right associate?
					sailRest, env = aux(ACL2elems[1:], env)
					retType = Sail_t_bits(sailElem.getType().length + sailRest.getType().length)
					fnType = Sail_t_fn([sailElem.getType(), sailRest.getType()], retType)
					fn = SailHandwrittenFn('@', fnType, infix=True)
					return SailApp(fn, [sailElem, sailRest], infix=True), env
				else:
					return sailElem, env
			# elems = []
			# for e in ACL2ast[1:]:
			# 	sailAST, env, _ = transform.transformACL2asttoSail(e, env)
			# 	sailAST = coerceExpr(sailAST[0], elemTyp)
			# 	elems.append(e)
			# retTyp = Sail_t_bits(nElems * nBits)
			sail, env = aux(ACL2ast[1:], env)
			return True, [sail], env
		else:
			return None

def syscall_numbers(ACL2ast, env):
	if env.getFile().lower() == "syscall-numbers" and \
			isinstance(ACL2ast, list) and len(ACL2ast) == 4 and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == "if" and \
			isinstance(ACL2ast[1], list) and len(ACL2ast[1]) == 2 and \
			isinstance(ACL2ast[1][0], str) and ACL2ast[1][0].lower() == "app-view" and \
			isinstance(ACL2ast[3], str) and ACL2ast[3].lower() == "nil":
		# TODO: Assert app-view?
		ifTerm, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)
		thenTerm, env, _ = transform.transformACL2asttoSail(ACL2ast[2], env)
		return True, thenTerm, env

base_decodes = ['one-byte-opcode-execute', 'two-byte-opcode-execute']
vex_decodes = ['vex-0f-execute', 'vex-0f38-execute', 'vex-0f3a-execute']
evex_decodes = ['evex-0f-execute', 'evex-0f38-execute', 'evex-0f3a-execute']
all_decodes = base_decodes + vex_decodes + evex_decodes

def gen_ext_opcode_execute_call(env):
	name = env.getDefineSlot().lower()
	if name == 'one-byte-opcode-execute':
		fn = handwritten_tokens.ext_one_byte_opcode_execute_fn
		arg_names = ["proc-mode", "start-rip", "temp-rip", "prefixes", "rex-byte", "opcode", "modr/m", "sib"]
	elif name == 'two-byte-opcode-execute':
		fn = handwritten_tokens.ext_two_byte_opcode_execute_fn
		arg_names = ["proc-mode", "start-rip", "temp-rip", "prefixes", "mandatory-prefix", "rex-byte", "opcode", "modr/m", "sib"]
	elif name in vex_decodes:
		fn = handwritten_tokens.ext_vex_execute_fn(name)
		arg_names = ["proc-mode", "start-rip", "temp-rip", "prefixes", "rex-byte", "vex-prefixes", "opcode", "modr/m", "sib"]
	elif name in evex_decodes:
		fn = handwritten_tokens.ext_evex_execute_fn(name)
		arg_names = ["proc-mode", "start-rip", "temp-rip", "prefixes", "rex-byte", "evex-prefixes", "opcode", "modr/m", "sib"]
	else:
		assert False
	args = []
	for n in arg_names:
		sail, env, _ = env.lookup(n)(n, env)
		args += sail
	return [SailApp(fn, args)], env

def ext_opcode_hooks(ACL2ast, env):
	if env.getDefineSlot().lower() in all_decodes and is_acl2_case_match(ACL2ast, 'opcode'):
		# Apply any filters for implemented opcodes defined above
		decodes = implemented_opcode(ACL2ast, env)
		if decodes:
			(_, decodes, env) = decodes
		else:
			# Use default translation if we don't have filters
			(decodes, env, _) = specialTokens.tr_case(ACL2ast, env)
		# Add a check for opcode extension hooks
		ext_call, env = gen_ext_opcode_execute_call(env)
		early_return = [SailReturn(SailUnitLit())]
		# return True, [SailBlock([SailIf(ext_call, early_return, [SailUnitLit()])] + decodes)], env
		return True, [SailIf(ext_call, early_return, decodes)], env
	elif is_acl2_app(ACL2ast, 'defbitstruct', min_len=3) and is_acl2_symbol(ACL2ast[1], 'prefixes'):
	# elif isinstance(ACL2ast, list) and len(ACL2ast) > 2 and ACL2ast[0].lower() == 'defbitstruct' and ACL2ast[1].lower() == 'prefixes':
		# Process the definition of the `prefixes` type and add it to the environemnt,
		# but don't output the generated Sail, because we have a copy in the handwritten
		# files so that the type can be overridden by extensions
		_, env, _ = specialTokens.tr_defbitstruct(ACL2ast, env)
		return True, [], env
	else:
		return None

# Replace proc_mode checks with calls to hooks
def proc_mode_hooks(ACL2ast, env):
	if is_acl2_list(ACL2ast, min_len=1) and isinstance(ACL2ast[0], str):
		proc_modes = ['*64-bit-mode*', '*compatibility-mode*', '*protected-mode*', '*real-address-mode*', '*smm-mode*']
		hooks = {
			'*64-bit-mode*': 'in_64bit_mode',
			'*compatibility-mode*': 'in_compatibility_mode',
			'*protected-mode*': 'in_protected_mode',
			'*real-address-mode*': 'in_real_mode',
			'*smm-mode*': 'in_system_management_mode'
		}
		def getConstName(n):
			return n[2:] if n.startswith('#.') else n
		# Check for direct proc_mode comparisons
		is_comparison = is_acl2_app(ACL2ast, ['equal', 'eql', 'eq', '=', '/='])
		lhs_is_proc_mode = (is_comparison and is_acl2_symbol(ACL2ast[1], 'proc-mode'))
		rhs_is_proc_mode_const = (is_comparison and isinstance(ACL2ast[2], str) and getConstName(ACL2ast[2].lower()) in hooks)
		# The mode constants might already have been evaluated to integers by ACL2, e.g. as part of macro expansion in tr_def_inst
		rhs_is_proc_mode_num = (is_comparison and isinstance(ACL2ast[2], str) and ACL2ast[2].isdecimal() and 0 <= int(ACL2ast[2]) and int(ACL2ast[2]) <= 4)
		if is_comparison and rhs_is_proc_mode_const or (lhs_is_proc_mode and rhs_is_proc_mode_num):
			(arg, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
			proc_mode_arg = ACL2ast[2].lower()
			proc_mode = getConstName(proc_mode_arg) if rhs_is_proc_mode_const else proc_modes[int(proc_mode_arg)]
			fnName = hooks[proc_mode]
			fnTyp = Sail_t_fn([handwritten_tokens.proc_mode_typ], Sail_t_bool())
			call = SailApp(SailHandwrittenFn(fnName, fnTyp), arg)
			if ACL2ast[0].lower() == '/=':
				call = SailApp(handwritten_tokens.not_fn, [call])
			return True, [call], env
		# Also check for proc_mode pattern matches
		elif is_acl2_app(ACL2ast, 'case', min_len=3) and is_acl2_symbol(ACL2ast[1], 'proc-mode'):
			(match_exp, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
			def rewrite_clauses(clauses, env):
				if not is_acl2_list(clauses, min_len=1):
					return (None, env)
				clause = clauses[0]
				rest = clauses[1:]
				if not is_acl2_list(clause, min_len=2):
					return (None, env)
				is_proc_mode_const = (isinstance(clause[0], str) and getConstName(clause[0].lower()) in hooks)
				is_proc_mode_num = (isinstance(clause[0], str) and clause[0].isdecimal() and 0 <= int(clause[0]) and int(clause[0]) <= 4)
				is_wildcard = is_acl2_symbol(clause[0], 'otherwise')
				(clause_rhs, env, _) = transform.transformACL2asttoSail(clause[1], env)
				if is_proc_mode_const or is_proc_mode_num:
					proc_mode = getConstName(clause[0].lower()) if is_proc_mode_const else proc_modes[int(clause[0])]
					fnName = hooks[proc_mode]
					fnTyp = Sail_t_fn([handwritten_tokens.proc_mode_typ], Sail_t_bool())
					call = SailApp(SailHandwrittenFn(fnName, fnTyp), match_exp)
					if len(rest) == 0:
						return (clause_rhs, env)
					else:
						(rest_exp, env) = rewrite_clauses(rest, env)
						return ([SailIf([call], clause_rhs, rest_exp)], env) if rest_exp else (None, env)
				elif is_wildcard:
					return (clause_rhs, env)
				else:
					return (None, env)
			(sail, env) = rewrite_clauses(ACL2ast[2:], env)
			return (True, sail, env) if sail else None
		else:
			return None
	else:
		return None

def ext_memory_hooks(ACL2ast, env):
	memory_readers = ["rme08", "rme16", "rme32", "rme48", "rme64", "rme80", "rme128", "rme-size"]
	memory_readers += [f.replace("rme", "rime") for f in memory_readers]
	memory_writers = ["wme08", "wme16", "wme32", "wme48", "wme64", "wme80", "wme128", "wme-size"]
	memory_writers += [f.replace("wme", "wime") for f in memory_writers]
	operand_accessors = ["x86-operand-from-modr/m-and-sib-bytes", "x86-operand-to-reg/mem", "x86-operand-to-xmm/mem"]
	targets = ["x86-effective-addr"] + operand_accessors + memory_readers + memory_writers
	if isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() in targets:
		fnName = ACL2ast[0].lower()
		(sail, env, _) = transform.transformFunCall(ACL2ast, env)
		fnArgs = sail[0].getActuals()
		fnArgNames = [a.getName().lower() if isinstance(a, SailBoundVar) else None for a in sail[0].getFn().getFormals()]

		# Prepare some arguments for replacement
		prefixesTyp = env.lookupBitfieldType("prefixes")
		try:
			prefixesAST, env, _ = env.lookup("prefixes")(["prefixes"], env)
			prefixesVar = coerceExpr(prefixesAST[0], prefixesTyp)
			havePrefixes = True
		except KeyError:
			prefixesVar = SailBoundVar("prefixes", prefixesTyp)
			havePrefixes = False

		# Perform patches
		if fnName == "x86-operand-from-modr/m-and-sib-bytes":
			fnArgs[6] = prefixesVar
		elif fnName in ["x86-operand-to-reg/mem", "x86-operand-to-xmm/mem"]:
			fnArgs.insert(fnArgNames.index("rex-byte"), prefixesVar)
			sibTyp = env.lookupBitfieldType("sib")
			sibAST, env, _ = env.lookup("sib")(["sib"], env)
			sibVar = coerceExpr(sibAST[0], sibTyp)
			fnArgs.append(sibVar)
		elif fnName == "x86-effective-addr":
			fnArgs[1] = prefixesVar
		elif fnName in memory_readers + memory_writers:
			# Prepare some more extra arguments
			# Generate a call to `select-address-size` using the existing translation;
			# the `p4?` argument in ACL2 will be translated to `Some(prefixes)`.
			(addr_size, env, _) = transform.transformFunCall(["select-address-size", "proc-mode", "p4?" if havePrefixes else "nil"], env)
			base_reg = noneHelper(Sail_t_synonym("base_reg_idx", Sail_t_range(0, 15))) # TODO
			fnArgs.insert(fnArgNames.index("proc-mode") + 1, addr_size[0])
			fnArgs.insert(fnArgNames.index("seg-reg") + 1, base_reg)
			if fnName in ["rme32", "rime32", "rme-size", "rime-size", "wme32", "wime32", "wme-size", "wime-size"]:
				fnArgs[-1] = fnArgs[-1].exprs[0][1]  # Extract mem_ptr? argument from struct
		return True, sail, env
	return None

def moffset_hooks(ACL2ast, env):
	in_moffset_mov_inst = env.getDefineSlot().lower() in ["x86-mov-Op/En-FD".lower(), "x86-mov-Op/En-TD".lower()]
	in_bstar = is_acl2_app(ACL2ast, "b*") and is_acl2_list(ACL2ast[1])
	def is_offset_size_bind(b):
		return is_acl2_symbol(b[0], "offset-size") or is_acl2_the_bind(b[0], "offset-size") and is_acl2_app(b[1], "select-address-size")
	has_offset_size = in_bstar and any(is_offset_size_bind(b) for b in ACL2ast[1])
	if in_moffset_mov_inst and has_offset_size:
		binds = [("offset-size", ["select-moffset-size"] + b[1][1:]) if is_offset_size_bind(b) else b for b in ACL2ast[1]]
		(sail, env, _) = specialTokens.tr_bstar([ACL2ast[0], binds] + ACL2ast[2:], env)
		return True, sail, env
	else:
		return None

def literal_exceptions(ACL2ast, env):
	if isinstance(ACL2ast, ACL2quote) and isinstance(ACL2ast.getAST(), str) and ACL2ast.getAST().lower() in [':ud', ':gp', ':nm']:
		sail, env, _ = specialTokens.tr_fault_fresh([ACL2ast.getAST()], env)
		sail = coerceExpr(sail[0], Sail_t_unit())
		return True, [sail], env
	return None

interventionsList = [
	x86_token,
	and_macro,
	# wb_1,
	dispatch_creator,
	ext_opcode_hooks,
	get_prefixes_flag,
	proc_mode_hooks,
	ext_memory_hooks,
	push_and_pop_errors,
	x86_hlt_return_type,
	insert_one_byte_nop,
	seg_descriptor_type,
	div_type,
	chk_exc_fn_type,
	bitvector_merges,
	syscall_numbers,
	moffset_hooks,
	literal_exceptions
]

def replacePatterns(ACL2ast, env):
	"""
	This function collates all the above functions.  If any of them return
	something other than None, the manual intervention implemented by that
	function is returned.
	"""
	# Only check for manual interventions if the input is a list, string or quote
	if not isinstance(ACL2ast, (list, str, ACL2quote)):
		return False, [], env

	# Check manual interventions
	for fn in interventionsList:
		maybe_intervention = fn(ACL2ast, env)
		if maybe_intervention is not None:
			return maybe_intervention

	# Indicate no manual interventions were applied
	return False, [], env

def patch_linear_address_size(ACL2ast, env):
	if is_acl2_app(ACL2ast, "signed-byte") and len(ACL2ast) == 2:
		lin_addr_pattern = re.compile(r"#\.\*max-linear-address-size(\+\d+)?\*")
		lin_addr_match = lin_addr_pattern.match(ACL2ast[1].lower())
		if lin_addr_match:
			incr = int(lin_addr_match.group(1)[1:]) if lin_addr_match.group(1) else 0
			width = 64 + incr
			return ["signed-byte", str(width)]
	return ACL2ast

acl2_interventions = [
	patch_linear_address_size
]

def patch_acl2_node(ACL2ast, env):
	for fn in acl2_interventions:
		ACL2ast = fn(ACL2ast, env)

	return ACL2ast

def patch_acl2_tree(ACL2ast, env):
	if isinstance(ACL2ast, list):
		children = [patch_acl2_tree(t, env) for t in ACL2ast]
		return patch_acl2_node(children, env)
	elif isinstance(ACL2ast, CodeTopLevel):
		children = [patch_acl2_tree(t, env) for t in ACL2ast.topLevels]
		return patch_acl2_node(CodeTopLevel(children), env)
	else:
		return patch_acl2_node(ACL2ast, env)
