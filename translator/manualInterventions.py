from lex_parse import ACL2quote
from SailASTelems import *
import specialTokens
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

def or_macro(ACL2ast, env):
	"""
	Usually `OR` behaves well - i.e. has exactly two (boolean) arguments.  In
	the result of `create-dispatch-for-opcodes` however, it often only has one
	argument.  In this case, the OR is redundant (`:trans (or x)` is just `x`)
	so we remove it
	"""
	if isinstance(ACL2ast, list) and len(ACL2ast) == 2 and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'or':
		sailAST, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)
		return True, sailAST, env

def chk_ex_or(ACL2ast, env):
	"""
	Similar issue to `OR` above in two byte opcode dispatch.  Here, OR is
	expected to return its first non-nil argument, rather than a boolean.
	"""
	if isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'or' and \
			isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 'chk-ex':
		chk_ex_BV_fn = env.lookup('chk-ex')
		chk_ex_BV, env, _ = chk_ex_BV_fn([], env)
		chk_ex_BV = chk_ex_BV[0]

		elseTerm, env, _ = transform.transformACL2asttoSail(ACL2ast[2], env)
		sailAST = SailIf(
			ifTerm=[chk_ex_BV],  # Internal `convertToBool` does the work for us here
			thenTerm=[chk_ex_BV],
			elseTerm=elseTerm
		)

		return True, [sailAST], env

def implemented_opcode(ACL2ast, env):
	"""
	The two large tables below represent the translated one- and two- byte
	opcodes.  Numbers can be removed to disable certain opcodes.
	"""
	# Note #1:	These opcodes are missing from the ACL2 one byte dispatch table
	# 			in the ACL2 model: 214
	# Note #2:	pusha (96) and popa (97) are not valid in 64 bit mode and so
	# 			could be removed.
	casesToIncludeOneByte =\
						[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
						26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
						50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72,
						73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
						96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
						116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
						135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
						154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172,
						173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
						192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
						211, 212, 213, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230,
						231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
						250, 251, 252, 253, 254, 255]

	# Note #1:	These opcodes are missing from the ACL2 two byte dispatch
	# 			table: 4, 10, 12, 14, 15, 36-9, 54, 57, 59-63, 122, 123, 166,
	# 			167, 255
	#			That gives 21 non-existent instruction and thus 235 overall.
	# Note #2:	Opcodes 5, 7 are related to syscalls.
	# Note #3:	Floating point opcodes implemented: 16, 17, 18, 19, 20, 21,
	# 			22, 23, 40, 41, 111, 127, 198
	casesToIncludeTwoByte = \
						[0, 1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
						32, 33, 34, 35, 40, 41, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 110,
						111, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
						144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 163,
						164, 165, 168, 172, 173, 175, 176, 177, 182, 183, 184, 186, 189, 190, 191, 192, 198]

	if env.getDefineSlot().lower() in ['one-byte-opcode-execute', 'two-byte-opcode-execute'] and \
			isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and \
			ACL2ast[0].lower() == 'case' and \
			isinstance(ACL2ast[1], str) and \
			ACL2ast[1].lower() == 'opcode':
		# First two elements are 'case opcode' so include them
		if env.getDefineSlot().lower() == 'one-byte-opcode-execute':
			casesToInclude = casesToIncludeOneByte
			msg = 'one'
		else:
			casesToInclude = casesToIncludeTwoByte
			msg = 'two'

		# Exclude the final case which has a 'T' condition
		newACL2ast = ACL2ast[:2] + [case for case in ACL2ast[2:-1] if int(case[0]) in casesToInclude]
		sailAST, env, _ = specialTokens.tr_case(newACL2ast, env)

		# Make the missing opcodes throw exceptions
		matchesList = sailAST[0].getMatches()
		for i in range(256):
			if i not in casesToInclude:
				matchesList.append(
					(SailNumLit(i), specialTokens.errorHelper(f"Translation error: {msg}-byte opcode {i} not translated")))
		matchesList.append(
			(SailUnderScoreLit(), specialTokens.errorHelper(f"Translation error: invalid {msg}-byte opcode")))

		return True, sailAST, env

def t_in_get_prefixes(ACL2ast, env):
	"""
	`get-prefixes` in `x86.lisp` sometimes returns `t` as part of it's `mv`.
	This `t` really wants to be interpreted as an option(string) rather than
	a bool.
	"""
	if env.getDefineSlot().lower() == 'get-prefixes' and \
			isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'mv' and \
			isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 't':
		# Translate as per normal
		sailAST, env, _ = specialTokens.tr_mv(ACL2ast, env)

		# Replace the 't' literal with an option(string)
		itemsList = sailAST[0].getItems()
		itemsList[0] = someHelper(SailStringLit("This error was 't' in ACL2 but a Some(string) in Sail"))

		return True, sailAST, env

def fault_var_int_dispatch_creator(ACL2ast, env):
	"""
	In `create-dispatch-for-opcodes` we want to make sure `fault-var` is an
	option(string).
	"""
	if isinstance(ACL2ast, ACL2quote) and \
			isinstance(ACL2ast.getAST(), str) and \
			ACL2ast.getAST().upper() == ":UD":
		toRet = someHelper(SailStringLit(':UD'))
		print(toRet)
		return True, [toRet], env

def x86_illegal_instruction(ACL2ast, env):
	"""
	Want to force some instances of X86-ILLEGAL-INSTRUCTION to int.

	We can't extract the correct form from the ACL2 code because the patterns
	we would want to use are often not passed through the translator
	top-level, instead they are broken down in `case` and `cond` translator
	functions.  Instead, we match on a `cond`, let the translation happen,
	then use `getChildrenByPred` to extract the bottom-most Sail `if`, then
	change its type.
	"""
	if isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'cond' and \
			isinstance(ACL2ast[-1], list) and \
			isinstance(ACL2ast[-1][0], str) and ACL2ast[-1][0].lower() == 't' and \
			isinstance(ACL2ast[-1][1], list) and isinstance(ACL2ast[-1][1][0], str) and \
			ACL2ast[-1][1][0].lower() == 'x86-illegal-instruction':

		# Translate as normal
		sailAST, env, _ = specialTokens.tr_cond(ACL2ast, env)

		# Get the bottom-most if expression and modify the type appropriately
		pred = lambda e: isinstance(e, SailIf) and \
						 isinstance(e.elseTerm[0], SailApp) and \
						 e.elseTerm[0].getFn().getName().lower() == 'throw'

		bottom = sailAST[0].getChildrenByPred(pred)

		for _ in range(len(bottom)):
			b = bottom.pop()
			b.elseTerm[0].getFn().setType(Sail_t_fn([], Sail_t_unit(), {'escape'}))

		return True, sailAST, env


def x86_step_unimplemented(ACL2ast, env):
	"""
	Force type of all 'x86-step-unimplemented' to cope with some being left
	dangling.  E.g. opcode 108.
	"""
	if isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'x86-step-unimplemented':
		# Translate as normal
		sailAST, env, _ = specialTokens.tr_ms_fresh(ACL2ast, env)

		# Set type to unit
		sailAST[0].getFn().setType(Sail_t_fn([], Sail_t_unit(), {'escape'}))

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
		sailAST[0].getFn().setType(Sail_t_fn([], Sail_t_unit(), {'escape'}))

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
			return True, [someHelper(SailStringLit(ACL2ast.upper()))], env
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

interventionsList = [
	x86_token,
	and_macro,
	wb_1,
	dispatch_creator,
	or_macro,
	chk_ex_or,
	implemented_opcode,
	t_in_get_prefixes,
	fault_var_int_dispatch_creator,
	x86_illegal_instruction,
	x86_step_unimplemented,
	push_and_pop_errors,
	x86_hlt_return_type,
	seg_descriptor_type,
	div_type,
	chk_exc_fn_type,
	bitvector_merges,
	syscall_numbers
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
