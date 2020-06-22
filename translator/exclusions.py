from lex_parse import ACL2String, ACL2quote
from SailASTelems import *
import sys

'''
This file contains patterns which we ignore at the top-level stage.  Each
exclusion should have an associated justification.

The usage is intended to be specific, not for sweeping statements such as
'ignore all `defthms`s'.  These should be placed in manualCode.py instead.
'''

# Top level file to translate
translateFile = 'x86.lisp'

outputFolder = '/Users/Patrick/Desktop/x86_cleaned/model'
utilitiesFile = "/Users/patrick/Desktop/x86_cleaned/model/utils.sail"

unknownTokensFile = "/Users/patrick/Desktop/x86_cleaned/errorFile.txt"
unresolvedTypesFile = '/Users/patrick/Desktop/x86_cleaned/unresolvedTypes.txt'

if any([path is None for path in [outputFolder, unknownTokensFile, unresolvedTypesFile, utilitiesFile]]):
	sys.exit("Please specify file paths in exclusions.py")

# List of files to exclude
exclusions_list = [
	# Functions which are used in very few places and would otherwise cause hassle
	['defun', 'power-of-2p-measure'], # Only used in `defn is-power-of-2p`, which we also ignore
	['defn', 'is-power-of-2p'], # Only used once: in a `defthm` - we ignore `defthm`s
	['defun', 'find-best-fitting-m'], # Only used once: in a `defthm`

	# Functions which are used in code generation and are otherwise hard to detect
	['defun', 'gpr-arith/logic-spec-gen-fn'],

	# Some include-book in structures.lisp
	['include-book', ACL2String('basic-structs')],
	['std::make-define-config'],

	# An include-book in shifts.lisp
	['include-book', ACL2String('centaur/bitops/fast-rotate')],
]

# Dictionary of {file : [pattern]}
# The file `file` is not translated apart from the patterns in `[pattern]`
include_list = {
	# Allow through these include-book to get to some file further down
	'other-non-det' : [
		['include-book', ACL2String('syscalls')],
		# ['define', 'HW_RND_GEN'],
		# ['define', 'HW_RND_GEN-logic']
	],
	'syscalls' : [['include-book', ACL2String('environment')],],
	'environment' : [['include-book', ACL2String('top-level-memory')]],
	'paging' : [['include-book', ACL2String('physical-memory')]],
	'physical-memory': [['include-book', ACL2String('modes')]],
	'modes': [
		['include-book', ACL2String('register-readers-and-writers')],
		['define', '64-bit-modep'],
		['define', 'x86-operation-mode'],
	],
	'state-field-thms' : [['include-book', ACL2String('abstract-state')]],
	'abstract-state' : [['include-book', ACL2String('concrete-memory')]],
	'concrete-memory' : [['include-book', ACL2String('concrete-state')]],
	'concrete-state' : [['include-book', ACL2String('structures')]],

	# These patterns are specifically to do with taking a slice from x86.lisp downwards, and should be removed
	# to all a full translation
	# TODO: remove this!
	'x86' : [
		['include-book', ACL2String('instructions/top')],
		['include-book', ACL2String('two-byte-opcodes-dispatch')],
		['include-book', ACL2String('three-byte-opcodes-dispatch')],
		['include-book', ACL2String('vex-opcodes-dispatch')],
		# ['include-book', ACL2String('evex-opcodes-dispatch')],
		['define', 'get-prefixes'],
		['define', 'x86-fetch-decode-execute'],
		['make-event'], # Deceptive, but actually the main route into the semantic functions, makes one-byte-opcode-execute
	],
	'top' : [
		['include-book', ACL2String('arith-and-logic')],
		['include-book', ACL2String('bit')],
		['include-book', ACL2String('push-and-pop')],
		['include-book', ACL2String('move')],
		['include-book', ACL2String('subroutine')],
		['include-book', ACL2String('multiply')],
		['include-book', ACL2String('conditional')],
		['include-book', ACL2String('exchange')],
		['include-book', ACL2String('signextend')],
		['include-book', ACL2String('string')],
		['include-book', ACL2String('rotate-and-shift')],
		['include-book', ACL2String('jump-and-loop')],
		['include-book', ACL2String('divide')],
		['include-book', ACL2String('segmentation')],
		['include-book', ACL2String('fp/top')],
		# TODO: syscall; fp
		# ['define', 'x86-illegal-instruction'],
		# ['define', 'x86-step-unimplemented'],
		['def-inst', 'x86-hlt'],
		['def-inst', 'x86-cmc/clc/stc/cld/std'],
		['def-inst', 'x86-sahf'],
		['def-inst', 'x86-lahf'],
		# ['def-inst', 'x86-rdrand'],
	],
	'two-byte-opcodes-dispatch': [
		['include-book', ACL2String('three-byte-opcodes-dispatch')],
		['include-book', ACL2String('dispatch-creator')],
		['make-event'],  # Makes two-byte-opcode-execute
		['define', 'two-byte-opcode-decode-and-execute'],
	],
	'three-byte-opcodes-dispatch' : [['include-book', ACL2String('prefix-modrm-sib-decoding')]],
	'vex-opcodes-dispatch' : [
		['include-book', ACL2String('instructions/top')],
		['include-book', ACL2String('prefix-modrm-sib-decoding')],
		['include-book', ACL2String('dispatch-macros')],

		['make-event'], # Makes the execution functions
		['define', 'vex-decode-and-execute'],
	],
	'evex-opcodes-dispatch' : [
		['include-book', ACL2String('instructions/top')],
		['include-book', ACL2String('prefix-modrm-sib-decoding')],
		['include-book', ACL2String('dispatch-macros')],

		['make-event'], # Makes the execution functions
		['define', 'evex-decode-and-execute'],
	],
	'prefix-modrm-sib-decoding' : [
		['include-book', ACL2String('dispatch-macros')],

		['defsection', 'legacy-prefixes-decoding'],
		['define', 'get-one-byte-prefix-array-code'],
		['define', 'one-byte-opcode-modr/m-p'],

		['defsection', 'ModR/M-decoding'],
		['with-output'],
		['define', 'one-byte-opcode-modr/m-p'],
		['define', '64-bit-mode-one-byte-opcode-ModR/M-p'],
		['define', '32-bit-mode-one-byte-opcode-ModR/M-p'],
		['define', 'two-byte-opcode-modr/m-p'],
		['define', '64-bit-mode-two-byte-opcode-ModR/M-p'],
		['define', '32-bit-mode-two-byte-opcode-ModR/M-p'],
		['define', 'vex-opcode-modr/m-p'],

		['defsection', 'SIB-decoding'],
		['define', 'x86-decode-SIB-p'],

		['defsection', 'mandatory-prefixes-computation'],
		['define', 'compute-mandatory-prefix-for-two-byte-opcode'],
	],
	'dispatch-macros' : [
		['defmacro', 'ud-Lock-used'],
		['defmacro', 'ud-ModR/M.Mod-indicates-Register'],
		['defmacro', 'ud-Lock-used-mod-indicates-register'],
		['defmacro', 'ud-Lock-used-Dest-not-Memory-Op'],
		['defmacro', 'ud-second-operand-is-a-register'],
		['defmacro', 'ud-source-operand-is-a-register'],
		['defmacro', 'gp-cpl-not-0'],
		['defmacro', 'cplx86'],
		['defmacro', 'NM-CR0-TS-IS-1'],
		['defmacro', 'NM-CR0-EM-IS-1'],
		['defmacro', 'CR0'],
		['defmacro', 'gp-cr4-umip-is-1'],
		['defmacro', 'cr4'],
		['defmacro', 'ud-cpl-is-not-zero'],
		['defmacro', 'ud-opr-used'],
		['defmacro', 'ud-reps-used'],
		['defmacro', 'chk-exc'],
		['define', 'chk-exc-fn'],
		['defmacro', 'CS.D'],
	],
	'arith-and-logic' : [
		['include-book', ACL2String('arith-and-logic-spec')],
		['include-book', ACL2String('../decoding-and-spec-utils')],

		# The below-listed is everything else in arith-and-logic btw
		['def-inst', 'x86-add/adc/sub/sbb/or/and/xor/cmp/test-E-G'],
		['def-inst', 'x86-add/adc/sub/sbb/or/and/xor/cmp-G-E'],
		['def-inst', 'x86-add/adc/sub/sbb/or/and/xor/cmp-test-E-I'],
		['def-inst', 'x86-add/adc/sub/sbb/or/and/xor/cmp-test-rAX-I'],
		['def-inst', 'x86-inc/dec-FE-FF'],
		['def-inst', 'x86-inc/dec-4x'],
		['def-inst', 'x86-not/neg-F6-F7']
	],
	'bit' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		# The below-listed is everything else in arith bit
		['def-inst', 'x86-bt-0F-A3'],
		['def-inst', 'x86-bt-0F-BA'],
	],
	'push-and-pop' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		# The below-listed is everything else in push-and-pop
		['def-inst', 'x86-push-general-register'],
		['def-inst', 'x86-push-Ev'],
		['def-inst', 'x86-push-I'],
		['def-inst', 'x86-push-segment-register'],
		['def-inst', 'x86-pop-general-register'],
		['def-inst', 'x86-pop-Ev'],
		['def-inst', 'x86-pushf'],
		['def-inst', 'x86-popf'],
		['def-inst', 'x86-pusha'],
		['def-inst', 'x86-popa'],
	],
	'move' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		['def-inst', 'x86-mov-Op/En-MR'],
		['def-inst', 'x86-mov-Op/En-RM'],
		['def-inst', 'x86-mov-Op/En-FD'],
		['def-inst', 'x86-mov-Op/En-OI'],
		['def-inst', 'x86-mov-Op/En-MI'],
		['def-inst', 'x86-mov-Op/En-TD'],
		['def-inst', 'x86-lea'],
		['def-inst', 'x86-movsx'],
		['def-inst', 'x86-movsxd'],
		['def-inst', 'x86-movzx'],
		['def-inst', 'x86-mov-control-regs-Op/En-MR'],
	],
	'subroutine' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		['def-inst', 'x86-call-E8-Op/En-M'],
		['def-inst', 'x86-call-FF/2-Op/En-M'],
		['def-inst', 'x86-ret'],
		['def-inst', 'x86-leave'],
	],
	'multiply' : [
		['include-book', ACL2String('multiply-spec')],
		['def-inst', 'x86-mul'],
		['def-inst', 'x86-imul-Op/En-M'],
		['def-inst', 'x86-imul-Op/En-RM'],
		['def-inst', 'x86-imul-Op/En-RMI'],
	],
	'conditional' : [
		['define', 'jcc/cmovcc/setcc-spec'],
		['def-inst', 'x86-one-byte-jcc'],
		['def-inst', 'x86-two-byte-jcc'],
		['def-inst', 'x86-jrcxz'],
		['def-inst', 'x86-cmovcc'],
		['def-inst', 'x86-setcc'],
	],
	'exchange' : [
		['include-book', ACL2String('arith-and-logic-spec')],
		['def-inst', 'x86-xchg'],
		['def-inst', 'x86-cmpxchg'],
		['def-inst', 'x86-two-byte-nop'],
	],
	'signextend' : [
		['def-inst', 'x86-cbw/cwd/cdqe'],
		['def-inst', 'x86-cwd/cdq/cqo'],
	],
	'string' : [
		['def-inst', 'x86-movs'],
		['def-inst', 'x86-cmps'],
		['def-inst', 'x86-stos'],
	],
	'rotate-and-shift' : [
		['include-book', ACL2String('shifts-spec')],
		['include-book', ACL2String('rotates-spec')],
		['def-inst', 'X86-SAL/SAR/SHL/SHR/RCL/RCR/ROL/ROR'],
		['def-inst', 'x86-shld/shrd'],
	],
	'jump-and-loop' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		['def-inst', 'x86-near-jmp-Op/En-D'],
		['def-inst', 'x86-near-jmp-Op/En-M'],
		['def-inst', 'x86-far-jmp-Op/En-D'],
		['def-inst', 'x86-loop'],
	],
	'divide' : [
		['include-book', ACL2String('divide-spec')],
		['include-book', ACL2String('../decoding-and-spec-utils')],
		['def-inst', 'x86-div'],
		['def-inst', 'x86-idiv'],
	],
	'utilities' : [
		['define', 'trunc'],
	],
	# We call it this to avoid collision with the other segmentation file
	'segmentationInst' : [
		['include-book', ACL2String('../decoding-and-spec-utils')],
		['def-inst', 'x86-lgdt'],
		['def-inst', 'x86-lidt'],
		['def-inst', 'x86-lldt'],
	],
	# Same with floating point top
	'topFP' : [
		['include-book', ACL2String('mov')],
		['include-book', ACL2String('shuffle-and-unpack')],
		# ['include-book', ACL2String('convert')],
	],
	'base' : [
		['include-book', ACL2String('fp-structures')],
	],
	'mov' : [
		['include-book', ACL2String('../../decoding-and-spec-utils')],
		['def-inst', 'x86-movss/movsd-Op/En-RM'],
		['def-inst', 'x86-movss/movsd-Op/En-MR'],
		['def-inst', 'x86-movaps/movapd-Op/En-RM'],
		['def-inst', 'x86-movaps/movapd-Op/En-MR'],
		['def-inst', 'x86-movups/movupd/movdqu-Op/En-RM'],
		['def-inst', 'x86-movups/movupd/movdqu-Op/En-MR'],
		['def-inst', 'x86-movlps/movlpd-Op/En-RM'],
		['def-inst', 'x86-movlps/movlpd-Op/En-MR'],
		['def-inst', 'x86-movhps/movhpd-Op/En-RM'],
		['def-inst', 'x86-movhps/movhpd-Op/En-MR'],
	],
	'shuffle-and-unpack' : [
		['include-book', ACL2String('../../decoding-and-spec-utils')],
		['define', 'extract-32-bits'],
		['define', 'extract-64-bits'],
		['def-inst', 'x86-shufps-Op/En-RMI'],
		['def-inst', 'x86-shufpd-Op/En-RMI'],
		['def-inst', 'x86-unpck?ps-Op/En-RM'],
		['def-inst', 'x86-unpck?pd-Op/En-RM'],
	],
	# 'convert' : [
	# 	['include-book', ACL2String('../../decoding-and-spec-utils')],
	# 	['include-book', ACL2String('cvt-spec')],
	#
	# ],
	# 'cvt-spec' : [
	#
	# ],

	# Now onto some functions
	'decoding-and-spec-utils': [
		['include-book', ACL2String('other-non-det')],
		['define', 'select-operand-size'],
		['define', 'select-address-size'],
		['define', 'select-segment-register'],

		['defsection', 'read-operands-and-write-results'],
		['define', 'x86-operand-from-modr/m-and-sib-bytes'],
		['define', 'alignment-checking-enabled-p'],
		['define', 'x86-operand-to-reg/mem'],
		['define', 'x86-operand-to-xmm/mem'],

		['defsection', 'effective-address-computations'],
		['define', 'x86-effective-addr'],
		['define', 'x86-effective-addr-16'],
		['define', 'x86-effective-addr-16-disp'],
		['define', 'x86-effective-addr-32/64'],
		['define', 'x86-effective-addr-from-sib'],

		['defsection', 'instruction-pointer-operations'],
		['define', 'add-to-*ip'],
		['define', 'write-*ip'],
		['define', 'read-*ip'],

		['defsection', 'stack-pointer-operations'],
		['define', 'read-*sp'],
		['define', 'add-to-*sp'],
		['define', 'write-*sp'],

		['define', 'check-instruction-length'],
	],
	'register-readers-and-writers' : [
		['defsection', 'GPRs-Reads-and-Writes'],
		['define', 'rr08'],
		['define', 'rr16'],
		['define', 'rr32'],
		['define', 'rr64'],
		['define', 'wr08'],
		['define', 'wr16'],
		['define', 'wr32'],
		['define', 'wr64'],
		['define', 'rgfi-size'],
		['define', '!rgfi-size'],

		['defsection', 'GPR-indices'],
		['define', 'reg-index'],

		['defsection', 'XMMs-Reads-and-Writes'],
		['define', 'xmmi-size'],
		['define', 'rx32'],
		['define', 'rx64'],
		['define', 'rx128'],

		['defsection', 'ZMMs-Reads-and-Writes'],
		['define', 'rz32'],
		['define', 'rz64'],
		['define', 'rz128'],

		['defsection', 'rflags-Reads-and-Writes'],
		['define', 'write-user-rflags'],
		['define', 'undef-flg'],
		['define', 'undef-flg-logic'],
		['define', 'undef-read'],
		['defmacro', '!flgi-undefined'],
		['defmacro', '!flgi'],
		['defmacro', 'flgi'],

		['defsection', 'xmms-reads-and-writes'],
		['define', '!xmmi-size'],
		['define', 'wx32'],
		['define', 'wx64'],
		['define', 'wx128'],
		['define', 'wz32'],
		['define', 'wz64'],
		['define', 'wz128'],
	],
	'top-level-memory' : [
		['include-book', ACL2String('segmentation')],

		['defmacro', 'rme-size-opt'],
		['defmacro', 'wme-size-opt'],
		['defmacro', 'wime-size-opt'],
		['defmacro', 'rme16-opt'],
		['defmacro', 'rime-size-opt'],

		['define', 'rime-size'],
		['define', 'wime-size'],
		['define', 'address-aligned-p'],
		['define', 'rme-size'],
		['define', 'wme-size'],
		['defmacro', 'rme08-opt'],
		['make-event'],
			# Generated by the make-events
			['define', 'rme08'],
			['define', 'rime08'],
			['define', 'rme16'],
			['define', 'rime16'],
			['define', 'rme32'],
			['define', 'rime32'],
			['define', 'rme48'],
			['define', 'rme64'],
			['define', 'rime64'],
			['define', 'rme80'],
			['define', 'rme128'],

			['define', 'wme08'],
			['define', 'wime08'],
			['define', 'wme16'],
			['define', 'wime16'],
			['define', 'wme32'],
			['define', 'wime32'],
			['define', 'wme48'],
			['define', 'wme64'],
			['define', 'wime64'],
			['define', 'wme80'],
			['define', 'wme128'],
			
	],
	'segmentation' : [
		['include-book', ACL2String('linear-memory')],
		['define', 'ea-to-la'],
		['define', 'segment-base-and-bounds'],

		['include-book', ACL2String('segmentation-structures')],

		['define', 'make-code-segment-attr-field'],
		['define', 'ia32e-valid-call-gate-segment-descriptor-p'],
		['define', 'ia32e-valid-code-segment-descriptor-p'],
		['define', 'ia32e-valid-ldt-segment-descriptor-p'],
		['define', 'make-system-segment-attr-field'],
	],
	'application-level-memory' : [
		['include-book', ACL2String('state-field-thms')],

		['define', 'canonical-address-p'],
		['define', 'rvm08'],
		['define', 'wvm08'],
	],
	'linear-memory' : [
		['include-book', ACL2String('paging')],

		['defsection', 'Parametric-Memory-Reads-and-Writes'],
		['define', 'riml-size'],
		['define', 'riml08'],
		['define', 'riml16'],
		['define', 'riml32'],
		['define', 'riml64'],
		['define', 'rml08'],
		['define', 'rml16'],
		['define', 'rml32'],
		['define', 'rml64'],
		['define', 'rml-size'],
		['define', 'rml48'],
		['define', 'rml80'],
		['define', 'rml128'],

		['define', 'wml-size'],
		['define', 'wml08'],
		['define', 'wml16'],
		['define', 'wml32'],
		['define', 'wml48'],
		['define', 'wml64'],
		['define', 'wml80'],
		['define', 'wml128'],

		['define', 'wiml-size'],
		['define', 'wiml08'],
		['define', 'wiml16'],
		['define', 'wiml32'],
		['define', 'wiml64'],

		['defsection', 'reasoning-about-memory-reads-and-writes'],
		['define', 'rb'],
		['define', 'rb-1'],
		['define', 'wb'],
		['define', 'wb-1'],
	],
}

# include_instead = {
# 	'decoding-and-spec-utils' : {
# 			('include-book', ACL2String('other-non-det')): ['include-book', ACL2String('register-readers-and-writers')]
# 	}
# }

# Used so we don't try to automatically type check recursive functions
# and get into an infinite recursion because of it
forced_return_types = {
	'rb-1' : Sail_t_tuple([	Sail_t_option(Sail_t_string()),
							Sail_t_int(),
							Sail_t_int()
							]),

	'wb-1' : Sail_t_tuple([	Sail_t_option(Sail_t_string()),
							Sail_t_int()
							]),

	'get-prefixes' : Sail_t_tuple([	Sail_t_option(Sail_t_string()), # Error list
									Sail_t_int(),					# Number of prefoxes
									Sail_t_int(),					# Rex byte
									Sail_t_int()					# x86 dummy
									])
}


# These files handle state updates, for which hwe write custom functions
exclusions_files = [
	# Temporary
	'fp-structures',
]

def replacePatterns(ACL2ast, env):
	if not isinstance(ACL2ast, (list, str, ACL2quote)):
		return False, [], env

	# Usually `and` is used as we would expect: it returns a boolean.  Technically, however, it returns its last
	# argument if none of its arguments are nil.  Mostly this property isn't used, but in `check-instruction-length` it
	# is, which is what we deal with here.
	if env.defineSlot.lower() == 'check-instruction-length' and isinstance(ACL2ast, list) and\
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'and':
		# Assume we have `(and (> length 15) length)` which macro translates to `(IF (< '15 LENGTH) LENGTH 'NIL)`
		# Make the return type option(int)
		lengthBVfn = env.lookup('length')
		(lengthBV, env, _) = lengthBVfn([], env)
		lengthBV = lengthBV[0]

		sailAST = SailIf(
			ifTerm=[SailApp(
				fn=SailHandwrittenFn(
					name='<',
					typ=Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_bool())
				),
				actuals=[SailNumLit(15), lengthBV],
				infix=True
			)],
			thenTerm=[someHelper(lengthBV)],
			elseTerm=[noneHelper(lengthBV.getType())]
		)

		return True, [sailAST], env

	# Make sure that the `t` of `wb-1` returns an option(string) rather than bool
	if env.defineSlot.lower() == 'wb-1' and isinstance(ACL2ast, str) and ACL2ast == 't':
		return True, [manualCode.someHelper(SailStringLit("Empty error"))], env

	# For some reason we need to use `(include-book "dispatch-creator")` before using the function
	# `CREATE-DISPATCH-FOR-OPCODES`.  Thus, we need to do this before the make-event in x86.lisp, which uses this
	# function
	if env.getFile() in ['x86', 'two-byte-opcodes-dispatch'] and isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'make-event':

		env.evalACL2(['include-book', '"dispatch-creator"'])
		sailAST, env, _ = manualCode._make_event_fn(ACL2ast, env)

		return True, sailAST, env

	# Usually 'OR" behaves well - i.e. has exactly two (boolean) arguments.  In the result of
	# `create-dispatch-for-opcodes` however, it often only has one argument.  In this case, the OR is redundant
	# (if you don't believe me, compare `:trans (or x)` with `:trans (or x y)`) so we remove it
	if isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'or' and len(ACL2ast) == 2:
		sailAST, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)
		return True, sailAST, env

	# Similar issue in two byte opcode dispatch
	if isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
		isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'or' and \
		isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 'chk-ex':

		chk_ex_BV_fn = env.lookup('chk-ex')
		chk_ex_BV, env, _ = chk_ex_BV_fn([], env)
		chk_ex_BV = chk_ex_BV[0]

		elseTerm, env, _ = transform.transformACL2asttoSail(ACL2ast[2], env)
		sailAST = SailIf(
			ifTerm=[chk_ex_BV], # Internal `convertToBool` does the work for us here
			thenTerm=[chk_ex_BV],
			elseTerm=elseTerm
		)

		return True, [sailAST], env

	# While we're in development, suppress the execution of most opcodes
	# TODO: remove
	# These opcodes are missing from the ACL2 one byte dispatch table: 214

	# pusha (96) and popa (97) are not valid in 64 bit mode so could remove.

	casesToIncludeOneB = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
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
	# These opcodes are missing from the ACL2 two byte dispatch table: 4, 10, 12, 14, 15, 36-9, 54, 57, 59-63, 122, 123, 166, 167, 255
	# That gives 21 non-existent instruction and thus 235 overall.
	# Opcodes 5, 7 are related to syscalls.
	# Floating point opcodes: 16, 17, 18, 19, 20, 21, 22, 23, 40, 41, 111, 127, 198
	casesToIncludeTwoB = [0, 1, 2, 3, 6, 8, 9, 11, 13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 24, 26, 27, 28, 29, 30, 31,
						  32, 33, 34, 35, 40, 41, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 110,
						  111, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
						  144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 163,
						  164, 165, 168, 172, 173, 175, 176, 177, 182, 183, 184, 186, 189, 190, 191, 192, 198]

	if env.defineSlot.lower() in ['one-byte-opcode-execute', 'two-byte-opcode-execute'] and \
			isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and \
			ACL2ast[0].lower() == 'case' and \
			isinstance(ACL2ast[1], str) and \
			ACL2ast[1].lower() == 'opcode':
		# First two elements are 'case opcode' so include them
		if env.defineSlot.lower() == 'one-byte-opcode-execute':
			casesToInclude = casesToIncludeOneB
			msg = 'one'
		else:
			casesToInclude = casesToIncludeTwoB
			msg = 'two'

		# Exclude the final case which has a 'T' condition
		newACL2ast = ACL2ast[:2] + [case for case in ACL2ast[2:-1] if int(case[0]) in casesToInclude]
		sailAST, env, _ = manualCode._case_fn(newACL2ast, env)

		# Make the missing opcodes throw exceptions
		matchesList = sailAST[0].getMatches()
		for i in range(256):
			if i not in casesToInclude:
				matchesList.append((SailNumLit(i), manualCode.errorHelper(f"Translation error: {msg}-byte opcode {i} not translated")))
		matchesList.append((SailUnderScoreLit(), manualCode.errorHelper(f"Translation error: invalid {msg}-byte opcode")))

		return True, sailAST, env



	# `get-prefixes` in `x86.lisp` sometimes returns `t` as part of it's `mv`.
	# This `t` really wants to be interpreted as an `option(string)` rather than a `bool`.
	if env.defineSlot.lower() == 'get-prefixes' and \
			isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'mv' and \
			isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 't':

		# Translate as per normal
		sailAST, env, _ = manualCode._mv_fn(ACL2ast, env)

		# Replace the 't' literal with an option(string)
		itemsList = sailAST[0].getItems()
		itemsList[0] = someHelper(SailStringLit("This error was 't' in ACL2 but a Some(string) in Sail"))

		return True, sailAST, env

	# In `create-dispatch-for-opcodes` we want to make sure `fault-var` is an option(string)
	if isinstance(ACL2ast, ACL2quote) and \
			isinstance(ACL2ast.getAST(), str) and \
			ACL2ast.getAST().upper() == ":UD":

		toRet = someHelper(SailStringLit(':UD'))
		print(toRet)
		return True, [toRet], env

	# Also want to force the case statement to an int
	# if isinstance(ACL2ast, list) and len(ACL2ast) >= 2 and \
	# 		isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'case' and \
	# 		isinstance(ACL2ast[1], str) and ACL2ast[1].lower() == 'fault-var':
	#
	# 	# Translate as normal
	# 	sailAST, env, _ = manualCode._case_fn(ACL2ast, env)
	#
	# 	# Force the type
	# 	sailAST[0].setForceType(Sail_t_int())
	#
	# 	return True, sailAST, env

	# Also want to force some instances of X86-ILLEGAL-INSTRUCTION to int
	# We can't extract the correct form from the ACL2 code because the patterns we would want to use are often not
	# passed through the translator top-level, instead they are broken down in `case` and `cond` translator functions.
	# Instead, we match on a `cond`, let the translation happen, then use `getChildrenByPred` to extract the bottom
	# most Sail `if`, then change its type.
	if isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'cond' and \
			isinstance(ACL2ast[-1], list) and \
			isinstance(ACL2ast[-1][0], str) and ACL2ast[-1][0].lower() == 't' and \
			isinstance(ACL2ast[-1][1], list) and isinstance(ACL2ast[-1][1][0], str) and \
			ACL2ast[-1][1][0].lower() == 'x86-illegal-instruction':

		# Translate as normal
		sailAST, env, _ = manualCode._cond_fn(ACL2ast, env)

		# Get the bottom-most if expression and modify the type appropriately
		pred = lambda e : isinstance(e, SailIf) and \
						  isinstance(e.elseTerm[0], SailApp) and \
						  e.elseTerm[0].getFn().getName().lower() == 'throw'

		bottom = sailAST[0].getChildrenByPred(pred)

		for _ in range(len(bottom)):
			b = bottom.pop()
			b.elseTerm[0].getFn().setType(Sail_t_fn([], Sail_t_int(), {'escape'}))

		return True, sailAST, env

	# Force type of all 'x86-step-unimplemented' to cope with some being left dangling.  E.g. opcode 108
	if isinstance(ACL2ast, list) and \
			isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'x86-step-unimplemented':

		# Translate as normal
		sailAST, env, _ = manualCode._ms_fresh_fn(ACL2ast, env)

		# Set type to int
		sailAST[0].getFn().setType(Sail_t_fn([], Sail_t_int(), {'escape'}))

		return True, sailAST, env

	# `push-and-pop.lisp` inspects error messages passed up from function calls.  Under our approximation of errors
	# we can't do this, so simply translate as a custom error.  Fortunately, all occurrences of `cond` in that file
	# correspond to such patterns.
	# Ths same is true in `subroutine.lisp`.
	if env.getFile().lower() in ['push-and-pop', 'subroutine'] and \
			isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'cond':

		return True, [manualCode.errorHelper("This error is generated by the translator and represents a more detailed error that occurred in `push-and-pop.lisp`")], env

	# Don't need this any more as we convert to bool in the condition where this is used
	# Wrap calls to `check_instruction_length` in an `is_some` in `subroutine.lisp`
	# if env.getFile().lower() == 'subroutine' and \
	# 		isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and \
	# 		ACL2ast[0].lower() == 'check-instruction-length':
	#
	# 	# Translate as normal but wrap in is_some
	# 	sailAST, env, _ = env.lookup(ACL2ast[0])(ACL2ast, env)
	# 	sailAST = convertToBool(sailAST)
	#
	# 	return True, sailAST, env

	# Force the type of the `nil` in `jcc/cmovcc/setcc-spec` to bool
	# if env.defineSlot.lower() == 'jcc/cmovcc/setcc-spec' and isinstance(ACL2ast, str) and ACL2ast.lower() == 'nil':
	# 	return True, [SailBoolLit(False)], env

	# Force return type of x86-hlt
	if env.defineSlot.lower() == 'x86-hlt' and \
			isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == '!!ms-fresh':

		# Translate as normal
		sailAST, env, _= manualCode._ms_fresh_fn(ACL2ast, env)

		# Set type
		sailAST[0].getFn().setType(Sail_t_fn([], Sail_t_int(), {'escape'}))

		return True, sailAST, env

	# Force some types in ia32e-valid-call-gate-segment-descriptor-p etc.
	if env.defineSlot.lower() in ['ia32e-valid-call-gate-segment-descriptor-p', 'ia32e-valid-code-segment-descriptor-p', 'ia32e-valid-ldt-segment-descriptor-p']:
		# if isinstance(ACL2ast, str) and ACL2ast.lower() == 'nil':
		# 	return True, [SailBoolLit(False)], env
		if transform.listStartsWith(ACL2ast, ['mv', 't', '0'], convertCase=True):
			return True, [SailTuple([SailBoolLit(True), SailTuple([SailStringLit(""), SailTuple([SailNumLit(0)])])])], env

	# Force types in divide
	# if env.defineSlot.lower() in ['div-spec', 'idiv-spec'] and isinstance(ACL2ast, str) and ACL2ast.lower() == 'nil':
	# 	return True, [SailBoolLit(False)], env

	if (env.defineSlot.lower().startswith('idiv-spec-') or env.defineSlot.lower().startswith('div-spec-')) and \
		isinstance(ACL2ast, list) and isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'list':

		return True, [SailBoolLit(True)], env

	# Force type of `chk-exc-fn` to be option(string)
	if env.defineSlot.lower() == 'chk-exc-fn' and isinstance(ACL2ast, str) and ACL2ast.lower() in [':ud', ':nm'] and\
		env.peekContext2 != 'cond':

		return True, [someHelper(SailStringLit(ACL2ast))], env

	# # In shld-spec/shrd-spec force a (mv 0 nil 0 0) to return a bool for the nil instead of option(string)
	# if env.defineSlot.lower() in ['shld-spec', 'shrd-spec'] and isinstance(ACL2ast, list) and len(ACL2ast) == 5 and \
	# 	isinstance(ACL2ast[0], str) and ACL2ast[0].lower() == 'mv' and \
	# 	all([isinstance(a, str) and a.lower() == '0' for a in [ACL2ast[1]] + ACL2ast[3:]]) and\
	# 	isinstance(ACL2ast[2], str) and ACL2ast[2].lower() == 'nil':
	#
	# 	return True, [SailTuple([SailNumLit(0), SailBoolLit(False), SailNumLit(0), SailNumLit(0)])], env

	# In VEX dispatch force a (mv nil nil nil) to return bools instead of option(strings)
	# if env.defineSlot.lower() == 'vex-decode-and-execute' and isinstance(ACL2ast, list) and len(ACL2ast) == 4 and\
	# 	all([isinstance(a, str) for a in ACL2ast]) and \
	# 	ACL2ast[0].lower() == 'mv' and \
	# 	all(a.lower() == 'nil' for a in ACL2ast[1:]):
	#
	# 	return True, [SailTuple([SailBoolLit(False), SailBoolLit(False), SailBoolLit(False)])], env

	return False, [], env

