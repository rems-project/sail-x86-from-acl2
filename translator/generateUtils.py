from SailTypes import *
import SailASTelems
from config_files import utilitiesFile

"""
This script creates Sail functions which mimic the operations in
`utilities.lisp` and saves them in `utilitiesFile` defined in
`configuration.py`.

This is don't manually because automatic translation would be tricky for not
much gain.  Not everything is translated as some of it is quite esoteric.
"""

def generate(includeHeader):
	numsToGenerate = [1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 16, 17, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 35, 43, 44, 45, 47, 48, 49, 51, 52, 55, 59, 60, 64, 65, 80, 112, 120, 128, 256, 512]

	namesGenerated = [] # [(name, SailType)]

	'''
	nXY.  Names are, for example `N08`, `N32`, `N512`.  All they do is take use
	the handwritten function `loghead`.
	'''
	code_nXY = []
	for n in numsToGenerate:
		name = "n{:02d}".format(n)
		val = f"val {name} : (int) -> int"
		function = f"function {name} (x) = loghead({n}, x)"
		together = "\n".join([val, function])
		code_nXY.append(together)
		namesGenerated.append((	name.upper(),
								SailASTelems.SailHandwrittenFn(
									name,
									Sail_t_fn([Sail_t_int()], Sail_t_int()))))


	'''
	ntoi.  Names are, for example `N08-to-i08`.  Easiest way to understand this
	function from the `:exec` code snippet in the link below (rather than the 
	':logic' section which uses `logext` and complicates matters).  It takes an
	int, and converts into a signed int of the given size.  The original uses
	sign extension (not applicable here), to the only remaining thing to
	consider is truncation.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____N08-TO-I08?path=3512/28770/10572/30934/30935/381/31043
	'''
	code_ntoi = []
	for n in numsToGenerate:
		n_num = "n{:02d}".format(n)
		i_num = "i{:02d}".format(n)
		code_ntoi.append(\
f"""val {n_num}_to_{i_num} : (int) -> (int) effect {{escape}}
function {n_num}_to_{i_num} (x) = {{
	assert(0 <= x & x <= {2**n - 1});
	if x < {2**(n-1)}
	then x
	else x - {2**n}
}}"""
		)
		namesGenerated.append((	f"{n_num}-to-{i_num}".upper(),
								SailASTelems.SailHandwrittenFn(
									f"{n_num}_to_{i_num}",
									Sail_t_fn([Sail_t_int()], Sail_t_int(), {'escape'}))))

	'''
	iXY.  Names are, for example, `i64`.  The implementation is a simple
	application of the handwritten function `logext`, which sign extends the
	lower order bits of an int.
	'''
	code_iXY = []
	for n in numsToGenerate:
		i_num = "I{:02d}".format(n)
		code_iXY.append(\
f"""val {i_num} : int -> int effect {{escape}}
function {i_num} x = binary_logext ({n}, x)
""")
		namesGenerated.append((i_num.upper(),
							   SailASTelems.SailHandwrittenFn(
								   i_num.upper(),
								   Sail_t_fn([Sail_t_int()], Sail_t_int(), {'escape'}))))

	'''
	Output to file
	'''
	with open(utilitiesFile, 'w') as f:
		f.write("$include <prelude.sail>\n")
		if includeHeader:
			f.write('$include "handwritten2.sail"\n')
		f.write("\n")
		f.write("\n\n".join(code_nXY))
		f.write("\n\n")
		f.write("\n\n".join(code_ntoi))
		f.write("\n\n")
		f.write("\n\n".join(code_iXY))

	# Return
	return namesGenerated

if __name__ == '__main__':
	generate(True)