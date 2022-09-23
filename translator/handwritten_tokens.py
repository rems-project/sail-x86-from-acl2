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
	Sail_t_fn([Sail_t_string()], Sail_t_int())
)

feature_flags_fn = SailHandwrittenFn(
	'feature_flags_fn',
	Sail_t_fn([Sail_t_list(Sail_t_string())], Sail_t_int())
)

undef_read_logic_fn = SailHandwrittenFn(
	'undef_read_logic',
	Sail_t_fn([], Sail_t_nat(), {'undef'})
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

nfix_fn = SailHandwrittenFn('nfix', Sail_t_fn([Sail_t_int()], Sail_t_nat()))

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
	# Coerce argument to integer if it might be negative, just to be on the safe side...
	# TODO: Add signed variant that returns the MSB if we ask for a bit
	# that is beyond the width of a signed bitvector
	argType = args[1].getType() if isNonnegativeType(args[1].getType()) else Sail_t_int()
	return SailHandwrittenFn('logbitp', Sail_t_fn([Sail_t_int(), argType], Sail_t_bool()))

def logbit_fn(args, env):
	argType = args[1].getType() if isNonnegativeType(args[1].getType()) else Sail_t_int()
	return SailHandwrittenFn('logbit', Sail_t_fn([Sail_t_int(), argType], Sail_t_bits(1)))


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

read_rip_fn = SailHandwrittenFn(
	'read_rip',
	Sail_t_fn([], Sail_t_bits(48, signed=True))
)

write_rip_fn = SailHandwrittenFn(
	'write_rip',
	Sail_t_fn([Sail_t_bits(48, signed=True)], Sail_t_unit())
)

rgfi_fn = SailHandwrittenFn(
						'rgfi',
						Sail_t_fn([Sail_t_int()], Sail_t_bits(64, signed=True), {'escape', 'rreg'}))

write_rgfi_fn = SailHandwrittenFn(
						'write_rgfi',
						Sail_t_fn([Sail_t_int(), Sail_t_bits(64, signed=True)], Sail_t_unit(), {'escape', 'wreg'}))

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
	Sail_t_fn([], Sail_t_bool())
)

ms_fn = SailHandwrittenFn(
	'ms',
	Sail_t_fn([], Sail_t_bool())
)

fault_fn = SailHandwrittenFn(
	'fault',
	Sail_t_fn([], Sail_t_bool())
)

memi_fn = SailHandwrittenFn(
	'memi',
	Sail_t_fn([Sail_t_bits(64)], Sail_t_bits(8), {'rmem', 'rreg'})
)

bang_memi_fn = SailHandwrittenFn(
	'bang_memi',
	Sail_t_fn([Sail_t_bits(64), Sail_t_bits(8)], Sail_t_unit(), {'eamem', 'wmv', 'rreg'})
)

def rb_fn(args, env):
	if isinstance(args[0], SailNumLit):
		nBytes = args[0].getNum()
	elif isinstance(args[0].getType(), Sail_t_member) and args[0].getType().subType == Sail_t_member.INT:
		# Hack: Normally rb is used with constant number of bytes, but
		# in the fall-through case of rml-size, it's used with a
		# variable length.  However, looking at the constraints on the
		# variable, that fall-through case is actually unreachable, so
		# it doesn't really matter...
		nBytes = max(args[0].getType().members)
	else:
		sys.exit(f"Error: unsupported number of bytes {args[0].pp()} in rb")
	valTyp = Sail_t_bits(8 * nBytes)
	retTyp = Sail_t_tuple([Sail_t_option(Sail_t_string()), valTyp])
	# virtual addresses are signed 48-bit values in the ACL2 model
	vaTyp = Sail_t_bits(48, signed=True)
	fnTyp = Sail_t_fn([Sail_t_nat(), vaTyp, Sail_t_string()], retTyp)
	return SailHandwrittenFn('rb', fnTyp)

def wb_fn(args, env):
	if isinstance(args[0], SailNumLit):
		nBytes = args[0].getNum()
	elif isinstance(args[0].getType(), Sail_t_member) and args[0].getType().subType == Sail_t_member.INT:
		# Hack: See rb_fn above...
		nBytes = max(args[0].getType().members)
	else:
		sys.exit(f"Error: non-constant number of bytes {args[0].pp()} in wb")
	valTyp = Sail_t_bits(8 * nBytes)
	retTyp = Sail_t_option(Sail_t_string())
	# virtual addresses are signed 48-bit values in the ACL2 model
	vaTyp = Sail_t_bits(48, signed=True)
	fnTyp = Sail_t_fn([Sail_t_nat(), vaTyp, Sail_t_string(), valTyp], retTyp)
	return SailHandwrittenFn('wb', fnTyp)

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

def bits_list(bits):
	return [Sail_t_bits(n) for n in bits]

ext_one_byte_opcode_execute_fn = SailHandwrittenFn(
	'ext_one_byte_opcode_execute',
	Sail_t_fn([Sail_t_range(0, 4)] + [Sail_t_bits(48, signed=True)] * 2 + bits_list([52, 8, 8, 8, 8]), Sail_t_unit())
)

ext_two_byte_opcode_execute_fn = SailHandwrittenFn(
	'ext_two_byte_opcode_execute',
	Sail_t_fn([Sail_t_range(0, 4)] + [Sail_t_bits(48, signed=True)] * 2 + bits_list([52, 8, 8, 8, 8, 8]), Sail_t_unit())
)

def ext_vex_execute_fn(name):
	ext_name = 'ext_' + utils.sanitiseSymbol(name)
	args = [Sail_t_range(0, 4)] + [Sail_t_bits(48, signed=True)] * 2 + bits_list([52, 8, 24, 8, 8, 8])
	return SailHandwrittenFn(ext_name, Sail_t_fn(args, Sail_t_unit())
)

def ext_evex_execute_fn(name):
	ext_name = 'ext_' + utils.sanitiseSymbol(name)
	args = [Sail_t_range(0, 4)] + [Sail_t_bits(48, signed=True)] * 2 + bits_list([52, 8, 32, 8, 8, 8])
	return SailHandwrittenFn(ext_name, Sail_t_fn(args, Sail_t_unit())
)
