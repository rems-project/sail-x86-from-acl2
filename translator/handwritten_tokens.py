from SailASTelems import *
from SailTypes import *

'''
This file links the handwritten Sail functions in handwritten.sail to the
Python framework by specifying their names types and effects.  The definitions
here are collected in config_function_maps.py.

TODO: this could be done automatically.
'''

feature_flag_fn = SailHandwrittenFn(
	'feature_flag_fn',
	Sail_t_fn([Sail_t_string(), Sail_t_int()], Sail_t_int())
)

feature_flags_fn = SailHandwrittenFn(
	'feature_flags_fn',
	Sail_t_fn([Sail_t_list(Sail_t_string()), Sail_t_int()], Sail_t_int())
)

undef_read_logic_fn = SailHandwrittenFn(
	'undef_read_logic',
	Sail_t_fn([Sail_t_int()], Sail_t_tuple([Sail_t_nat(), Sail_t_int()]), {'undef'})
)

boolToBit = SailHandwrittenFn(
				'bool_to_bit',
				Sail_t_fn([Sail_t_bool()], Sail_t_bits(1)))

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

def logbitp_fn(args, env):
	return SailHandwrittenFn('logbitp', Sail_t_fn([Sail_t_int(), args[1].getType()], Sail_t_bool()))


def logbit_fn(args, env):
	return SailHandwrittenFn('logbit', Sail_t_fn([Sail_t_int(), args[1].getType()], Sail_t_bits(1)))


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

def n_size_fn(args, env):
	resultType = Sail_t_bits(args[0].getNum()) if isinstance(args[0], SailNumLit) else Sail_t_int()
	return SailHandwrittenFn('n_size', Sail_t_fn([Sail_t_int(), resultType], resultType))


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
	Sail_t_fn([Sail_t_bits(64), Sail_t_int()], Sail_t_bits(8), {'rmem', 'rreg'})
)

bang_memi_fn = SailHandwrittenFn(
	'bang_memi',
	Sail_t_fn([Sail_t_bits(64), Sail_t_bits(8), Sail_t_int()], Sail_t_int(), {'eamem', 'wmv', 'rreg'})
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
	Sail_t_fn([Sail_t_bits(64), Sail_t_bits(64)], Sail_t_bits(128))
)

merge_2_u32s_fn = SailHandwrittenFn(
	'merge_2_u32s',
	Sail_t_fn([Sail_t_bits(32), Sail_t_bits(32)], Sail_t_bits(64))
)

merge_4_u32s_fn = SailHandwrittenFn(
	'merge_4_u32s',
	Sail_t_fn([Sail_t_bits(32), Sail_t_bits(32), Sail_t_bits(32), Sail_t_bits(32)], Sail_t_bits(128))
)
