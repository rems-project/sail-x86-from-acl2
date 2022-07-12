from specialTokens import *
from handwritten_tokens import *

"""
This file collects the translation functions.  Each function below collects
translation functions related to a different type of token.  Namely:

 -  Special tokens: tokens that we translate manually either because they are
 	built in (and thus we must, e.g. `if`) or because it is expedient to do so
 	(e.g. `define`).
 -  Handwritten Token: these tokens link to the handwritten Sail functions in
 	handwritten.sail.
 -  Macros: these tokens are registered as macros and are expanded at
 	translate-time.
 -  Unimplemented: these tokens also link to the handwritten Sail functions in
 	handwritten.sail but indicate that the function has no implementation.

Each function below returns the same type of data structure, a dictionary
mapping a string representing a Lisp token to a function which can translate
that token.  I.e. 

	dict( (Lisp token : str) : (translation function) )

The results of all functions are collated at the end in loadManualEnv().
"""


def specialTokens():
	"""
	In order the list below represents:
	 1. Normal special tokens
	 2. Special tokens which are ignores
	 3. Special tokens which represent error values

	The functions referenced are defined in specialTokens.py.
	"""
	manualDefinitions = {
		'in-package': 		tr_in_package,
		'include-book': 	tr_include_book,
		'local':			tr_local,
		'defsection': 		tr_defsection,
		'define': 			tr_define,
		'defun': 			tr_define,
		'make-event': 		tr_make_event,
		'defmacro': 		tr_defmacro,
		'defabbrev':		tr_defmacro, # TODO: Handle differently?
		'mbe': 				tr_mbe,
		'if': 				tr_if,
		'encapsulate': 		tr_encapsulate,
		'the': 				tr_the,
		'+': 				tr_plus,
		'binary-+': 		tr_plus,
		'*': 				num_op_gen('*', Sail_t_int(), operandType=Sail_t_int(), infix=True),
		'-': 				tr_minus,
		'ash':				tr_ash,
		'=': 				num_op_gen('==', Sail_t_bool(), numOfArgs=2, infix=True),
		'equal': 			num_op_gen('==', Sail_t_bool(), numOfArgs=2, infix=True),
		'eql': 				num_op_gen('==', Sail_t_bool(), numOfArgs=2, infix=True),
		'int=': 			num_op_gen('==', Sail_t_bool(), numOfArgs=2, infix=True),
		'eq': 				num_op_gen('==', Sail_t_bool(), numOfArgs=2, infix=True),
		'/=': 				num_op_gen('!=', Sail_t_bool(), numOfArgs=2, infix=True),
		'<': 				num_op_gen('<', Sail_t_bool(), operandType=Sail_t_int(), numOfArgs=2, infix=True),
		'>': 				num_op_gen('>', Sail_t_bool(), operandType=Sail_t_int(), numOfArgs=2, infix=True),
		'<=': 				num_op_gen('<=', Sail_t_bool(), operandType=Sail_t_int(), numOfArgs=2, infix=True),
		'and': 				num_op_gen('&', Sail_t_bool(), operandType=Sail_t_bool(), infix=True),
		'or': 				num_op_gen('|', Sail_t_bool(), operandType=Sail_t_bool(), infix=True),
		'logior':			bitwise_op_gen('logior'),
		'logand':			bitwise_op_gen('logand'),
		'logxor':			bitwise_op_gen('logxor'),
		'truncate': 		num_op_gen('tdiv_int', Sail_t_int(), operandType=Sail_t_int(), numOfArgs=2, infix=False),
		'trunc': 		tr_trunc,
		'rem': 				num_op_gen('tmod_int', Sail_t_int(), operandType=Sail_t_int(), numOfArgs=2, infix=False),
		'zp': 				tr_zp,
		'part-select': 		tr_part_select,
		'part-install': 	tr_part_install,
		'b*': 				tr_bstar,
		'mv': 				tr_mv,
		'case': 			tr_case,
		'def-inst': 		tr_def_inst,
		'xr': 				tr_xr,
		'cond': 			tr_cond,
		'let': 				tr_bstar,
		'let*':				tr_bstar,
		'list': 			tr_list,
		'mv-let': 			tr_mv_let,
		'mbt': 				tr_mbt,
		'cons': 			tr_cons,
		'defbitstruct': 	tr_defbitstruct,
		'er': 				tr_er,
		'!!ms-fresh': 		tr_ms_fresh,  # `!!ms-fresh` etc. are macros defined in `decoding-and-spec-utils.lisp`
		'!!fault-fresh': 	tr_fault_fresh,  # Again, macro defined in `decoding-and-spec-utils.lisp`
		'!fault':		tr_fault_fresh,  # TODO: More proper fault handling
		'ifix': 			tr_ifix,
		'nfix':				tr_nfix,
		'n-size':			tr_n_size,
		'progn': 			tr_progn,
		'aref1': 			tr_aref1,
		'with-output': 		tr_with_output,
		'member-eq': 		tr_member_eq,
		't':				tr_t,
		'nil':				tr_nil,
		'64-bit-compute-mandatory-prefix-for-two-byte-opcode': 	tr_pe,
		'32-bit-compute-mandatory-prefix-for-two-byte-opcode': 	tr_pe,
                'rb':                           tr_rb,
                'wb':                           tr_wb,

		# The following ACL2 tokens are ignored (we do not translate them).
		#  -  `defthm` and `defthmd` relate to theorem proving in ACL2 and have
		#     no translation in Sail.  They may contains useful typing
		#     information or be translatable in other ways though.
		#  -  `add-macro-alias` is used only four time throughout the codebase,
		#     all in the `rflags-spec` file, and these uses appear extraneous.

		'defthm': 					tr_ignore,
		'defthmd': 					tr_ignore,
		'defrule': 					tr_ignore,
		'add-macro-alias': 			tr_ignore,
		'defthm-signed-byte-p': 	tr_ignore,
		'defthm-unsigned-byte-p': 	tr_ignore,
		'defrulel': 				tr_ignore,
		'in-theory':				tr_ignore,
		'def-ruleset': 				tr_ignore,
		'set-non-linearp': 			tr_ignore,
		'defxdoc': 					tr_ignore,
		'defun-nx': 					tr_ignore,
		'defconst': 					tr_ignore,

		# The following ACL2 tokens are functions which return various errors
		'x86-illegal-instruction': 	tr_fault_fresh,
		'x86-step-unimplemented': 	tr_ms_fresh,
		'x86-general-protection': 	tr_fault_fresh,
		'x86-device-not-available':	tr_fault_fresh,
	}

	# Convert names to uppercase
	name_to_fn_map = {}
	for name in manualDefinitions:
		name_to_fn_map[name.upper()] = manualDefinitions[name]

	# Generate number/bitvector conversions
	numsToGenerate = [1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 16, 17, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 35, 43, 44, 45, 47, 48, 49, 51, 52, 55, 59, 60, 64, 65, 80, 112, 120, 128, 256, 512]
	for i in numsToGenerate:
		n_num = "n{:02d}".format(i)
		i_num = "i{:02d}".format(i)
		n_to_i = n_num + "-to-" + i_num
		n_pred = n_num + "p"
		name_to_fn_map[n_num.upper()] = gen_coercion_to_bits(i, signed=False)
		name_to_fn_map[i_num.upper()] = gen_coercion_to_bits(i, signed=True)
		name_to_fn_map[n_to_i.upper()] = gen_coercion_to_bits(i, signed=True)
		name_to_fn_map[n_pred.upper()] = gen_bits_check(i, signed=True)

	register_accessors = [
		'rip',
		'rflags',
		'msri',
		'seg-visiblei',
		'seg-hidden-attri',
		'seg-hidden-basei',
		'seg-hidden-limiti',
		'ssr-visiblei',
		'ssr-hidden-attri',
		'ssr-hidden-basei',
		'ssr-hidden-limiti',
		'zmmi',
		'ctri',
		'stri',
                'app-view',
                'marking-view',
                'os-info'
	]
	for r in register_accessors:
		name_to_fn_map[r.upper()] = tr_register_read
		name_to_fn_map['!' + r.upper()] = tr_register_write

	return name_to_fn_map


def handwritten():
	"""
	The functions referenced are defined in handwritten_tokens.py and their
	handwritten Sail implementations are in handwritten.sail.  These functions
	mostly represent low-level bitwise operations and register reads/writes.
	"""
	handwrittenDefinitions = {
		'feature-flag'			: feature_flag_fn,
		'feature-flags'			: feature_flags_fn,
		'undef-read-logic'		: undef_read_logic_fn,
		'acl2::bool->bit'		: boolToBit,
		'bool->bit'				: boolToBit,
		'unsigned-byte-p'		: unsigned_byte_p,
		'signed-byte-p'			: signed_byte_p,
		'not'					: not_fn,
		'loghead'				: loghead_fn,
		'logtail'				: logtail_fn,
		'lognot'				: lognot_fn,
		'logcount'				: logcount_fn,
		'logext'				: binary_logext_fn,
		'rgfi'					: rgfi_fn,
		'!rgfi'					: write_rgfi_fn,
		'memi'					: memi_fn,
		'abs'					: abs_fn,
		'floor'					: floor_fn,
		'mod'					: mod_fn,
		'!memi'					: bang_memi_fn,
		'ms'					: ms_fn,
		'fault'					: fault_fn,
		'b-xor'					: b_xor_fn,
		'fast-rotate-left'		: rotate_left_fn,
		'fast-rotate-right'		: rotate_right_fn,
		'merge-2-u64s'			: merge_2_u64s_fn,
		'merge-2-u32s'			: merge_2_u32s_fn,
		'merge-4-u32s'			: merge_4_u32s_fn,
	}

	# Convert names to uppercase and use apply_fn_gen()
	name_to_fn_map = {}
	for (name, funcToApply) in handwrittenDefinitions.items():
		name = name.upper()
		name_to_fn_map[name] = apply_fn_gen(funcToApply, funcToApply.getNumFormals())

	def dependent_fn(func, numOfArgs, coerceActuals=False):
		return {'func': func, 'numOfArgs': numOfArgs, 'coerceActuals': coerceActuals}

	dependentHandwrittenDefs = {
		'logbitp'	: dependent_fn(logbitp_fn, 2, coerceActuals=True),
		'logbit'	: dependent_fn(logbit_fn, 2, coerceActuals=True)
	}
	for (name, fn) in dependentHandwrittenDefs.items():
		name = name.upper()
		name_to_fn_map[name] = apply_dependent_fn_gen(fn['func'], fn['numOfArgs'], coerceActuals=fn['coerceActuals'])

	return name_to_fn_map

def macros():
	"""
	Note that the numOfArgs parameter is actually not used in
	apply_macro_gen()

	A long list of Lisp macros can be found here:
	https://www.cs.utexas.edu/users/moore/acl2/v6-2/ACL2-BUILT-INS.html
	"""
	macroNames = [
		# Name					numOfArgs	useTrans1
		('1-'.upper(), 			1, 			True),
		('1+'.upper(), 			1, 			True),
		('cpl'.upper(), 		1, 			True),  # Buried away as `(defabbrev cpl ...)` in `paging.lisp`
		('ntoi'.upper(),		2, 			True),  # Defined in `utilities.lisp` - seems appropriate to place here given our custom `generateutils.py` file
	]

	# Convert name to uppercase and use apply_macro_gen()
	name_to_fn_map = {}
	for (mn, numOfArgs, useTrans1) in macroNames:
		name_to_fn_map[mn.upper()] = apply_macro_gen(numOfArgs, useTrans1)

	return name_to_fn_map

def unimplemented():
	"""
	Notably evex dispatch is not implemented as it is huge (because of macro
	expansion).
	"""
	unimplementedNames = [
		#	Name					numOfArgs	Type
		('vex-decode-and-execute', 6, Sail_t_fn([Sail_t_int()] * 6, Sail_t_unit(), {'escape'})),
		('evex-decode-and-execute', 6, Sail_t_fn([Sail_t_int()] * 6, Sail_t_unit(), {'escape'})),
		('x86-syscall-app-view', 8, Sail_t_fn([Sail_t_int()] + ([Sail_t_bits(48, True)] * 2) + [Sail_t_bits(52)] + ([Sail_t_bits(8)] * 4), Sail_t_unit(), {'escape'})),
	]

	# And these have not yet been implemented
	name_to_fn_map = {}
	for (name, numOfArgs, typ) in unimplementedNames:
		name_to_fn_map[name.upper()] = apply_fn_gen(
			unimplementedFunctionGen(
				name,
				numOfArgs,
				typ),
			numOfArgs
		)

	return name_to_fn_map

def unimplementedFunctionGen(fnName, numOfArgs, typ=None):
	"""
	Helper function for generating unimplemented translation functions.
	"""
	return SailHandwrittenFn(
		name=f'unimplemented_{fnName}',
		typ=typ)

def loadManualEnv():
	"""
	Merge the dictionaries returned by the functions above to collect all
	the translation functions.
	"""
	return {**specialTokens(),
			**handwritten(),
			**macros(),
			**unimplemented()}
