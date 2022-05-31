from lex_parse import ACL2String
from SailTypes import *

'''
This file configures File names or Lisp patterns to either include or exclude.
The usage is intended to be specific, not for sweeping statements such as
"ignore all `defthms`s".  These should be placed in specialTokens.py instead.

Lisp Patterns.  Mostly used to exclude certain forms or files in order to
translate only a slice of the model, rather than the whole thing.

 -  forced_return_types:	used to avoid errors when inferring types of some 
							recursive functions.
 -  exclusions_files:		Exclude these files from the translation.
 -  exclude_lisp:			Exclude forms beginning with Lisp patterns
							specified here in certain files.
 -  only_translate:			Include only forms beginning with Lisp patterns
							specified here in certain files.
						
Note that `exclude_lisp` and `only_translate` are complementary.  For a
given file we can choose to specify what to translate (in `only_translate`)
or what not to translate (in `exclude_lisp`).  In general, the decision is
made by choosing the one which will have fewer entries.
'''


###############################################################################
'''
Lisp patterns and files. 							
'''
###############################################################################


'''
The current type inference algorithm occasionally fails on recursive
functions.  Here we manually specify the return types of such functions.
'''
forced_return_types = {
	'rb-1' : Sail_t_tuple([	Sail_t_option(Sail_t_string()),
							Sail_t_int(),
							Sail_t_int()
							]),

	'wb-1' : Sail_t_tuple([	Sail_t_option(Sail_t_string()),
							Sail_t_int()
							]),

	'get-prefixes' : Sail_t_tuple([	Sail_t_option(Sail_t_string()), # Error list
									Sail_t_int(),					# Number of prefixes
									Sail_t_int(),					# Rex byte
									Sail_t_int()					# x86 dummy
									])
}


'''
Exclude these files from the translation
'''
exclusions_files = [
	'fp-structures',
]


'''
This variable is a dictionary of the form: {file : [pattern]}.
When translating `file`, if a form beginning with any pattern in the list
`[pattern]` is encountered, then that form will not be translated.  See
description at start of file for more information.
'''
exclude_lisp = {
	# Functions which are used in very few places and would otherwise cause hassle
	'rflags-spec': [
		['defun', 'power-of-2p-measure'],	# Only used in `defn is-power-of-2p`, which we also ignore
		['defn', 'is-power-of-2p'],			# Only used once: in a `defthm` - we ignore `defthm`s
		['defun', 'find-best-fitting-m'],	# Only used once: in a `defthm'
	],

	# Some include-book in structures.lisp and segmentation-structures.lisp
	'structures': [
		['include-book', ACL2String('basic-structs')],
		['std::make-define-config'],
	],
	'segmentation-structures': [
		['include-book', ACL2String('basic-structs')]
	],

	# An include-book in shifts.lisp
	'shifts-spec': [['include-book', ACL2String('centaur/bitops/fast-rotate')]],
	'rotates-spec': [['include-book', ACL2String('centaur/bitops/fast-rotate')]],

	# The rest of the patterns help restrict us to only translating a slice
	# of the model.
	'vex-opcodes-dispatch': [
		['include-book', ACL2String('cpuid')],
		['include-book', ACL2String('std/strings/hexify')]],
	'evex-opcodes-dispatch': [
		['include-book', ACL2String('cpuid')],
		['include-book', ACL2String('std/strings/hexify')]],

	'mov' : [
		['include-book', ACL2String('base')],
		['include-book', ACL2String('centaur/bitops/merge')],
	],
	'shuffle-and-unpack' : [
		['include-book', ACL2String('base')],
		['include-book', ACL2String('centaur/bitops/merge')],
	],

	'decoding-and-spec-utils' : [
		['defsection', 'error-objects'],
		['define', 'rip-guard-okp'],
		['defmacro', 'def-inst'],
	],

	'register-readers-and-writers' : [
		['include-book', 'rflags-spec'],
		['include-book', 'fp-structures'],
		['define', 'reg-indexp'],
		['define', 'mmx'],
		['define', '!mmx'],
		['define', 'mmx-instruction-updates'],
		['define', 'rz256'],
		['define', 'rz512'],
		['define', 'vector-access-type-p'],
		['define', 'wz256'],
		['define', 'wz512'],
		['define', 'zmmi-size'],
		['define', '!zmmi-size'],
		['define', 'unsafe-!undef'],
		['define', 'undef-read-logic'],
		['push-untouchable'],
		['include-book', ACL2String('tools/include-raw')],
		['defttag'],
		['include-raw', ACL2String('register-readers-and-writers-raw.lsp')],
	],

	'top-level-memory': [
		['define', 'gen-read-function'],
		['define', 'gen-write-function'],
	],

}


'''
This variable is a dictionary of the form: {file : [pattern]}.  When
translating `file`, a form is only translated if it begins with a pattern in
the list `[pattern]`.  See description at start of file for more information.
Most of the entries help restrict us to only translating a slice of the model.
'''
only_translate = {
	# The following files are here for their `include-book` forms.
	'other-non-det' : [['include-book', ACL2String('syscalls')],],
	'syscalls' : [['include-book', ACL2String('environment')],],
	'environment' : [['include-book', ACL2String('top-level-memory')]],
	'paging' : [['include-book', ACL2String('physical-memory')]],
	'physical-memory': [['include-book', ACL2String('modes')]],
	'state-field-thms' : [['include-book', ACL2String('abstract-state')]],
	'abstract-state' : [['include-book', ACL2String('concrete-memory')]],
	'concrete-memory' : [['include-book', ACL2String('concrete-state')]],
	'concrete-state' : [['include-book', ACL2String('structures')]],


	# These patterns are specifically to do with taking a slice from x86.lisp downwards, and should be removed
	# to all a full translation
	'x86' : [
		['include-book', ACL2String('instructions/top')],
		['include-book', ACL2String('two-byte-opcodes-dispatch')],
		['include-book', ACL2String('three-byte-opcodes-dispatch')],
		# ['include-book', ACL2String('vex-opcodes-dispatch')],
		# ['include-book', ACL2String('evex-opcodes-dispatch')],
		['define', 'get-prefixes'],
		['define', 'x86-fetch-decode-execute'],
		['make-event'], # Deceptive, but actually the main route into the semantic functions, makes one-byte-opcode-execute
	],
	'top' : [
		['include-book', ACL2String('arith-and-logic')],
		['include-book', ACL2String('bit')],
		['include-book', ACL2String('conditional')],
		['include-book', ACL2String('divide')],
		['include-book', ACL2String('exchange')],
		['include-book', ACL2String('jump-and-loop')],
		['include-book', ACL2String('move')],
		['include-book', ACL2String('multiply')],
		['include-book', ACL2String('push-and-pop')],
		['include-book', ACL2String('rotate-and-shift')],
		['include-book', ACL2String('segmentation')],
		['include-book', ACL2String('signextend')],
		['include-book', ACL2String('string')],
		['include-book', ACL2String('subroutine')],
		['include-book', ACL2String('fp/top')],

		['def-inst', 'x86-hlt'],
		['def-inst', 'x86-cmc/clc/stc/cld/std'],
		['def-inst', 'x86-sahf'],
		['def-inst', 'x86-lahf'],

		# The following forms are not included:
		# include-book syscall
		# x86-rdrand
		# x86-step-unimplemented
		# x86-illegal-instruction
		# x86-general-protection
		# x86-device-not-available
		# show-inst-decoding-and-spec-fn
		# show-inst-decoding-and-spec-ruleset

	],
	'three-byte-opcodes-dispatch' : [['include-book', ACL2String('prefix-modrm-sib-decoding')]],
	'prefix-modrm-sib-decoding' : [
		['include-book', ACL2String('dispatch-macros')],

		['defsection', 'legacy-prefixes-decoding'],
		['define', 'get-one-byte-prefix-array-code'],

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
		['defmacro', 'feature-flag-macro'],
		['defmacro', 'feature-flags-macro'],
	],
	'modes': [
		['include-book', ACL2String('register-readers-and-writers')],
		['define', '64-bit-modep'],
		['define', 'x86-operation-mode'],
	],

	'utilities' : [
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
		# Most FP books are excluded.
	],
	'base' : [
		['include-book', ACL2String('fp-structures')],
	],
	'segmentation' : [
		['include-book', ACL2String('linear-memory')],
		['include-book', ACL2String('segmentation-structures')],
		['define', 'ea-to-la'],
		['define', 'segment-base-and-bounds'],
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
