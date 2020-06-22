from SailASTelems import *
from SailTypes import *
from lex_parse import ACL2Comment
import generateUtils

"""
This file is split into two parts:
1) A list of names and type signatures of hadwritten functions
2) Collecting these definitions together, along with automatically
   generating new ones
"""

###############################################################################
# List of handwritten definitions
###############################################################################

undef_read_logic_fn = SailHandwrittenFn(
	'undef_read_logic',
	Sail_t_fn([Sail_t_int()], Sail_t_tuple([Sail_t_nat(), Sail_t_int()]), {'undef'})
)

boolToBit = SailHandwrittenFn(
				'bool_to_bit',
				Sail_t_fn([Sail_t_bool()], Sail_t_int()))

unsigned_byte_p = SailHandwrittenFn(
					'unsigned_byte_p',
					Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_bool(), {'escape'}))

signed_byte_p = SailHandwrittenFn(
					'signed_byte_p',
					Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_bool(), {'escape'}))

not_fn = SailHandwrittenFn(
			'not_bool',
			Sail_t_fn([Sail_t_bool()], Sail_t_bool()))

loghead_fn = SailHandwrittenFn(
				'loghead',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

logtail_fn = SailHandwrittenFn(
				'logtail',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

logbitp_fn = SailHandwrittenFn(
				'logbitp',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_bool(), {'escape'}))

logbit_fn = SailHandwrittenFn(
	'logbit',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape'})
)

lognot_fn = SailHandwrittenFn(
	'lognot',
	Sail_t_fn([Sail_t_int()], Sail_t_int())
)

logcount_fn = SailHandwrittenFn(
				'logcount',
				Sail_t_fn([Sail_t_int()], Sail_t_int(), {'escape'}))

binary_logand_fn = SailHandwrittenFn(
				'binary_logand',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

binary_logior_fn = SailHandwrittenFn(
				'binary_logior',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

binary_logxor_fn = SailHandwrittenFn(
				'binary_logxor',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

binary_logext_fn = SailHandwrittenFn(
				'binary_logext',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

n_size_fn = SailHandwrittenFn(
				'n_size',
				Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int()))

ash_fn = SailHandwrittenFn(
	'ash',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
)

abs_fn = SailHandwrittenFn(
	'abs',
	Sail_t_fn([Sail_t_int()], Sail_t_int())
)

floor_fn = SailHandwrittenFn(
	'floor2',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
)

mod_fn = SailHandwrittenFn(
	'mod',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
)

# Example from when we bitstructure accessors
# rflagsBits_get_cf_fn = SailHandwrittenFn(
# 				'rflagsBits_get_cf',
# 				Sail_t_fn([Sail_t_int()], Sail_t_int()))

r_rflags_fn = SailHandwrittenFn(
	'r_rflags',
	Sail_t_fn([Sail_t_int()], Sail_t_int(), {'rreg'})
)

write_rflags_fn = SailHandwrittenFn(
	'write_rflags',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'wreg'})
)

r_rip_fn = SailHandwrittenFn(
	'ripi',
	Sail_t_fn([Sail_t_int()], Sail_t_int())
)

write_rip_fn = SailHandwrittenFn(
	'write_rip',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'wreg'})
)

rgfi_fn = SailHandwrittenFn(
						'rgfi',
						Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'}))

write_rgfi_fn = SailHandwrittenFn(
						'write_rgfi',
						Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'}))

msri_fn = SailHandwrittenFn(
	'msri',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

seg_visiblei_fn = SailHandwrittenFn(
	'seg_visiblei',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_seg_visible_fn = SailHandwrittenFn(
	'write_seg_visiblei',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

write_seg_hidden_attri_fn = SailHandwrittenFn(
	'write_seg_hidden_attri',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

write_seg_hidden_basei_fn = SailHandwrittenFn(
	'write_seg_hidden_basei',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

write_seg_hidden_limiti_fn = SailHandwrittenFn(
	'write_seg_hidden_limiti',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

zmmi_fn =  SailHandwrittenFn(
	'zmmi',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_zmmi_fn = SailHandwrittenFn(
	'write_zmmi',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

ctri_fn =  SailHandwrittenFn(
	'ctri',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

stri_fn =  SailHandwrittenFn(
	'stri',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_stri_fn =  SailHandwrittenFn(
	'write_stri',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'wreg'})
)

ssr_visiblei_fn = SailHandwrittenFn(
	'ssr_visiblei',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_ssr_visiblei_fn = SailHandwrittenFn(
	'write_ssr_visiblei',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

ssr_hidden_basei_fn = SailHandwrittenFn(
	'ssr_hidden_basei',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_ssr_hidden_basei_fn = SailHandwrittenFn(
	'write_ssr_hidden_basei',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

ssr_hidden_limiti_fn = SailHandwrittenFn(
	'ssr_hidden_limiti',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_ssr_hidden_limiti_fn = SailHandwrittenFn(
	'write_ssr_hidden_limiti',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

ssr_hidden_attri_fn = SailHandwrittenFn(
	'ssr_hidden_attri',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

write_ssr_hidden_attri_fn = SailHandwrittenFn(
	'write_ssr_hidden_attri',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
)

app_view_fn = SailHandwrittenFn(
	'app_view',
	Sail_t_fn([Sail_t_unit()], Sail_t_bool())
)

ms_fn = SailHandwrittenFn(
	'ms',
	Sail_t_fn([Sail_t_int()], Sail_t_bool())
)

fault_fn = SailHandwrittenFn(
	'fault',
	Sail_t_fn([Sail_t_int()], Sail_t_bool())
)

memi_fn = SailHandwrittenFn(
	'memi',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'rmem', 'rreg'})
)

bang_memi_fn = SailHandwrittenFn(
	'bang_memi',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'eamem', 'wmv', 'rreg'})
)

feature_flags_fn = SailHandwrittenFn(
	'feature_flags',
	Sail_t_fn([Sail_t_list(Sail_t_string()), Sail_t_int()], Sail_t_int())
)

b_xor_fn = SailHandwrittenFn(
	'b_xor',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape'})
)

rotate_left_fn = SailHandwrittenFn(
	'rotate_left',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape'})
)

rotate_right_fn = SailHandwrittenFn(
	'rotate_right',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape'})
)

merge_2_u64s_fn = SailHandwrittenFn(
	'merge_2_u64s',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
)

merge_2_u32s_fn = SailHandwrittenFn(
	'merge_2_u32s',
	Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
)

merge_4_u32s_fn = SailHandwrittenFn(
	'merge_4_u32s',
	Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int())
)

###############################################################################
# Collect them together and generate new ones
###############################################################################
def generateHandwrittenDefinitions():
	# Collect the list in section 1 here
	handwrittenDefinitions = {
		'undef-read-logic'.upper()	: undef_read_logic_fn,
		'acl2::bool->bit'.upper()	: boolToBit,
		'bool->bit'.upper()			: boolToBit,
		'bool->bit'.upper()			: boolToBit,
		'unsigned-byte-p'.upper()	: unsigned_byte_p,
		'signed-byte-p'.upper()		: signed_byte_p,
		'not'.upper()				: not_fn,
		'loghead'.upper()			: loghead_fn,
		'logtail'.upper()			: logtail_fn,
		'logbitp'.upper()			: logbitp_fn,
		'logbit'.upper()			: logbit_fn,
		'lognot'.upper()			: lognot_fn,
		'logcount'.upper()			: logcount_fn,
		'logand'.upper()			: binary_logand_fn,
		'logior'.upper()			: binary_logior_fn,
		'logxor'.upper()			: binary_logxor_fn,
		'logext'.upper()			: binary_logext_fn,
		'n-size'.upper()			: n_size_fn,
		'rflags'.upper()			: r_rflags_fn,
		'!rflags'.upper()			: write_rflags_fn,
		'rip'.upper()				: r_rip_fn,
		'!rip'.upper()				: write_rip_fn,
		'rgfi'.upper()				: rgfi_fn,
		'!rgfi'.upper()				: write_rgfi_fn,
		'msri'.upper()				: msri_fn,
		'seg-visiblei'.upper()		: seg_visiblei_fn,
		'!seg-visiblei'.upper()		: write_seg_visible_fn,
		'!seg-hidden-attri'.upper()	: write_seg_hidden_attri_fn,
		'!seg-hidden-basei'.upper()	: write_seg_hidden_basei_fn,
		'!seg-hidden-limiti'.upper(): write_seg_hidden_limiti_fn,
		'zmmi'.upper()				: zmmi_fn,
		'!zmmi'.upper()				: write_zmmi_fn,
		'ctri'.upper()				: ctri_fn,
		'stri'.upper()				: stri_fn,
		'!stri'.upper()				: write_stri_fn,
		'ssr-visiblei'.upper()		: ssr_visiblei_fn,
		'ssr-hidden-basei'.upper()	: ssr_hidden_basei_fn,
		'ssr-hidden-limiti'.upper()	: ssr_hidden_limiti_fn,
		'ssr-hidden-attri'.upper()	: ssr_hidden_attri_fn,
		'!ssr-visiblei'.upper()		: write_ssr_visiblei_fn,
		'!ssr-hidden-basei'.upper()	: write_ssr_hidden_basei_fn,
		'!ssr-hidden-limiti'.upper(): write_ssr_hidden_limiti_fn,
		'!ssr-hidden-attri'.upper()	: write_ssr_hidden_attri_fn,
		'app-view'.upper()			: app_view_fn,
		'memi'.upper()				: memi_fn,
		'ash'.upper()				: ash_fn,
		'abs'.upper()				: abs_fn,
		'floor'.upper()				: floor_fn,
		'mod'.upper()				: mod_fn,
		'!memi'.upper()				: bang_memi_fn,
		'ms'.upper()				: ms_fn,
		'fault'.upper()				: fault_fn,
		'feature-flags'.upper()		: feature_flags_fn,
		'b-xor'.upper()				: b_xor_fn,
		'fast-rotate-left'.upper()	: rotate_left_fn,
		'fast-rotate-right'.upper()	: rotate_right_fn,
		'merge-2-u64s'.upper()		: merge_2_u64s_fn,
		'merge-2-u32s'.upper()		: merge_2_u32s_fn,
		'merge-4-u32s'.upper()		: merge_4_u32s_fn,

		# Example from when we bitstructure accessors:
		#'rflagsBits->cf'.upper()	:  rflagsBits_get_cf_fn
	}

	# Generate utility functions
	for (name, handwrittenFn) in generateUtils.generate(False):
		handwrittenDefinitions[name] = handwrittenFn

	return handwrittenDefinitions


def unimplementedFunctionGen(fnName, numOfArgs, typ=None):
	return SailHandwrittenFn(
		name=f'unimplemented_{fnName}',
		typ=typ)