from lex_parse import NewLine
import utils
import math
import sys
import re

"""
This file is split into two sections:

 1. Utility functions
 2. Classes which represent types in Sail

Of the utility function, `translateType` and `parseGuard`, which translate
type-specs and guards respectively, are of the most note.

Classes representing types must implement the following functions:
 - generalise()			Numeric types are represented as ints.  So, despite
 						types such as `range` being implemented below, they
 						generalise to `int`s via this function.
 - containsUnknown()	Sometimes types cannot be inferred directly.  Thus a
 						pass at the translated AST resolves unknown types.
 						This function returns true if the type contains any
 						types which are unknown.
 - pp()					Pretty print the type.
"""

################################################################################
# Utility functions
################################################################################

class eqSet():
	def __init__(self, init = None):
		"""
		A set where the equality translate is done via __eq__() rather than the
		__hash__() of the builtin Set.  This allows us to reliably store
		Sail types in it without two of the same type being repeated.

		TODO: extend set instead?

		Args:
			- init : list
		"""
		self.set = []

		if init is not None:
			for i in init:
				self.add(i)

	def items(self):
		return self.set

	def add(self, obj):
		if obj not in self.set: # Uses __eq__ not __hash__
			self.set.append(obj)

	def extend(self, objs):
		if type(objs) == eqSet:
			toAdd = objs.set
		elif type(objs) == list:
			toAdd = objs
		else:
			sys.exit(f"Error: unsupported typefor eqSet extend - {type(objs)}")

		for obj in toAdd:
			self.add(obj)

	def union(self, objs):
		self.extend(objs)

	def peek(self):
		return self.set[0]

	def all(self, pred):
		"""
		https://stackoverflow.com/questions/10666163/how-to-check-if-all-elements-of-a-list-matches-a-condition

		Args:
			- pred : SailType -> bool
		"""
		return all(pred(item) for item in self.set)

	def resolve(self):
		"""
		Check if all items in a set are consistent with some overarching type
		"""
		# Check we can resolve to a single overarching type
		genType = self.set[0]
		for t in self.set[1:]:
			genType = mergeTypes(genType, t)
			if genType is None:
				sys.exit(f"Error: can't generalise set - {self.set}")

		# Return this type if it exists
		return genType

	def __str__(self):
		return f"{', '.join([str(i) for i in self.set])}"

	def __len__(self):
		return len(self.set)


def dictAddOrInit(d, k, v):
	"""
	If k in d
	then add v to eqSet d[k]
	else initialise d[k] to eqSet(v)
	"""
	if k in d:
		d[k].add(v)
	else:
		d[k] = eqSet([v])


def translateType(env, typeSymbol, args=None):
	"""
	Translates ACL2 type-specs.  Given an acl2 symbol `typeSymbol`, and
	possible arguments to it, returns a Python class representing the type,
	or None on failure.  Trying to translate an unimplemented type will error
	(unlike parseGuard()).

	See here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____TYPE-SPEC

	Args:
		- typeSymbol : str e.g. `unsigned-byte`
		- args : [ACL2astElem] e.g. `['9']`
	Returns:
		- SailType
	"""
	# Translate any problematic args
	argsSanitised = []
	if args is not None:
		for a in args:
			if utils.convertLiteral(a) is not None or a == '*':
				argsSanitised.append(a)
			else:
				# Hope it's a macro translated to a quoted literal
				newA = env.evalACL2([':trans', a], debracket=True)
				newA = newA[0].getAST()
				argsSanitised.append(newA)
		args = argsSanitised

	# Translate the type
	typeSymbol = typeSymbol.upper()

	if typeSymbol == 'unsigned-byte'.upper():
		return Sail_t_bits(int(args[0])) # Sail_t_range(0, 2 ** int(args[0]) - 1)
	elif typeSymbol == 'signed-byte'.upper():
		return Sail_t_bits(int(args[0]), signed=True)
		# halfBits = int(args[0]) - 1
		# return Sail_t_range(- 2 ** halfBits, 2 ** halfBits - 1)
	elif typeSymbol == 'integer'.upper():
		if args is None:
			return Sail_t_int()
		if args[0] == '0' and args[1] == '*':
			return Sail_t_nat()
		elif '*' in args:
			return Sail_t_int()
		else:
			low = int(args[0])
			high = int(args[1])
			return Sail_t_range(low, high)
	elif typeSymbol == 'member'.upper():
		# Make sure each member is an int or each member is a keyword
		maybeNums = [utils.convertLiteral(a) for a in args]
		if all(isinstance(i, int) for i in maybeNums):
			return Sail_t_member(maybeNums)
		elif all(a.startswith(':') for a in args):
			return Sail_t_member(args)
		else:
			sys.exit("Error: `member` types not homogeneous")
	elif typeSymbol == 'or'.upper():
		# A common pattern is `or t nil` which translates to bool
		if args == ['t'.upper(), 'nil'.upper()]:
			return Sail_t_bool()
		else:
			sys.exit(f"Error: unknown args for type `OR` in `translateType`: {args}")
	else:
		sys.exit(f"Error: translation of type '{typeSymbol}' not implemented")


def parseGuard(guard):
	"""
	Takes a guard ACL2 expression and tries to extract type information from it

	Args:
		- guard : [ACL2astElem]
	Returns:
		- {str : eqSet(SailType)}	Possible types the guard may represent.
	"""
	# `possibilities` is a {str : eqSet(SailType)} mapping names to possible types
	possibilities = {}

	# Filter newlines
	guard = [item for item in guard if type(item) not in [NewLine]]

	# Prepare regexps for checking for number type predicates
	n_pred = re.compile(r"n(\d{2,3})p")
	i_pred = re.compile(r"i(\d{2,3})p")

	# Switch on the first element of guard
	switch = guard[0]
	if switch.lower() == 'and':
		for subGuard in guard[1:]:
			# Check for a specific patterns of encoding range constraints: (natp var) (<= var bound)
			if subGuard[0].lower() == "natp":
				name = subGuard[1]
				for bound in [int(g[2]) for g in guard[1:] if g[0] == "<=" and g[1] == name]:
					if bound >= 0:
						dictAddOrInit(possibilities, name, Sail_t_range(0, bound))
			possibilities = utils.dictExtend(possibilities, parseGuard(subGuard))
	elif switch.lower() == 'case':
		for case in guard[2:]:
			possibilities = utils.dictExtend(possibilities, parseGuard(case[1]))
		# Try to merge types to overarching type
		for (v, typs) in possibilities.items():
			typ = typs.resolve()
			if typ is None:
				del possibilities[v]
			else:
				possibilities[v] = eqSet([typ])
	elif switch.lower() == 'natp':
		if len(guard) != 2: sys.exit(f"Error: guard for natp has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_nat())
	elif switch.lower() == 'integerp':
		if len(guard) != 2: sys.exit(f"Error: guard for integerp has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_int())
	elif switch.lower() == 'unsigned-byte-p' or switch.lower() == 'signed-byte-p':
		if len(guard) != 3: sys.exit(f"Error: guard for unsigned-byte-p has unexpected number of elements: {guard}")
		name = guard[2]
		hiBits = utils.convertLiteral(guard[1])
		signed = (switch.lower() == 'signed-byte-p')
		if hiBits is None or isinstance(hiBits, float) or hiBits <= 0:
			print(f"Warning: none-integer value supplied for unsigned-byte-p argument: {guard}")
		else:
			dictAddOrInit(possibilities, name, Sail_t_bits(hiBits, signed=signed)) # Sail_t_range(0, 2 ^ hiBits - 1))
	elif switch.lower() == 'n01p':
		# TODO: this is for the function `extract-64-bits` - would be better to use fact that we match against a numeric
		# type to infer this type.
		if len(guard) != 2: sys.exit(f"Error: guard for n01p has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_range(0, 1))
	elif n_pred.match(switch.lower()):
		length = int(n_pred.match(switch.lower()).group(1))
		dictAddOrInit(possibilities, guard[1], Sail_t_bits(length))
	elif i_pred.match(switch.lower()):
		length = int(i_pred.match(switch.lower()).group(1))
		dictAddOrInit(possibilities, guard[1], Sail_t_bits(length, signed=True))
	elif switch.lower() == 'booleanp':
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_bool())
	elif switch.lower() == 'symbolp':
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_string())
	else:
		print(f"Warning: guard symbol '{switch}' not implemented")

	return possibilities

def isNumeric(st):
	"""
	Tests if SailType `st` is numeric.

	Args:
		st : SailType
	Returns:
		bool
	"""
	return isinstance(st.generalise(), Sail_t_int)

def isNonnegativeType(t):
	return isinstance(t, Sail_t_nat) or \
			(isinstance(t, Sail_t_range) and t.low >= 0 and t.high >= 0) or \
			(isinstance(t, Sail_t_member) and t.subType == Sail_t_member.INT and all(m >= 0 for m in t.members)) or \
			(isinstance(t, Sail_t_bits) and t.signed == False) or \
			(isinstance(t, Sail_t_bitfield))

def isSignedType(t):
	return not(isNonnegativeType(t))

def isUnitType(t):
	return isinstance(t, Sail_t_unit) or isinstance(t, Sail_t_error)

def isString(st):
	"""
	Tests if SailType `st` is a string.

	Args:
		st: : SailType

	Returns:
		bool
	"""
	return isinstance(st.generalise(), Sail_t_string)


################################################################################
# Types
################################################################################


class SailType:
	"""
	Super class for Sail types.  Note the default containsUnknown() method.
	"""
	def __init__(self):
		self.effects = set([])  # Has type set(Str)
	def __eq__(self, other):
		return type(self) == type(other)
	def __hash__(self):
		return id(self)
	def containsUnknown(self):
		return False
	def pp(self):
		return "unknown"
	def isPrintable(self):
		return True

def ppType(t):
	return t.pp() if isinstance(t, SailType) else "unknown"

def containsUnknownType(t):
	return t.containsUnknown() if isinstance(t, SailType) else True

class Sail_t_unit(SailType):
	"""Represents the Sail unit type"""
	def __init__(self):
		super(Sail_t_unit, self).__init__()

	def generalise(self):
		return Sail_t_unit()

	def pp(self):
		return 'unit'

class Sail_t_bool(SailType):
	"""Represents the base boolean type"""
	def __init__(self):
		super(Sail_t_bool, self).__init__()

	def generalise(self):
		return Sail_t_bool()

	def pp(self):
		return "bool"

class Sail_t_string(SailType):
	"""Represents the base string type"""
	def __int__(self):
		super(Sail_t_string, self).__init__()

	def generalise(self):
		return Sail_t_string()

	def pp(self):
		return "string"

class Sail_t_PlaceholderNil(SailType):
	"""Represents an ACL2 `nil` type before the real type has been resolved"""
	def __init__(self):
		super(Sail_t_PlaceholderNil, self).__init__()

	def generalise(self):
		return Sail_t_PlaceholderNil()

	def pp(self):
		sys.exit("Error: Tried to pp a nil type - these should have all been resolved")

class Sail_t_unknown(SailType):
	"""Represents an type which has yet to be resolved"""
	def __init__(self):
		super(Sail_t_unknown, self).__init__()

	def generalise(self):
		return Sail_t_unknown()

	def containsUnknown(self):
		return True

	def pp(self):
		"""
		If this is actually called then type resolution failed and so will
		trying to type check the output Sail.
		"""
		return "unknown"

	def isPrintable(self):
		return False

def isUnknownType(t):
	return isinstance(t, Sail_t_unknown) or t == None

def isPrintableType(t):
	return isinstance(t, SailType) and t.isPrintable()

class Sail_t_error(SailType):
	"""
	A 'throw' may take on any value.  This class represents that value, but
	it should be resolved at translate-time.
	"""
	def __init__(self):
		super(Sail_t_error, self).__init__()

	def generalise(self):
		return Sail_t_error()




	def pp(self):
		"""
		If this is actually called trying to type check the output Sail will
		fail.
		"""
		print("WARNING: printing Sail_t_error() type")
		return "error"

	def isPrintable(self):
		return False

class Sail_t_int(SailType):
	"""Represents an int type of unspecified size"""
	def __init__(self):
		super(Sail_t_int, self).__init__()

	def generalise(self):
		return Sail_t_int()

	def pp(self):
		return "int"

class Sail_t_bits(SailType):
	"""Represents the bits type"""
	def __init__(self, length, signed=False):
		'''
		Args:
			- length : int > 0 - I think this needs to be strictly bigger than zero
			- signed : bool - whether these bits are to be interpreted as a signed number or not
		'''
		super(Sail_t_bits, self).__init__()
		if type(length) == int and length <= 0:
			raise(ValueError(f"tried to construct a bits type with size le 0.  Size was: {length}"))
		self.length = length
		self.signed = signed

	def __eq__(self, other):
		return type(self) == type(other) and self.length == other.length and self.signed == other.signed
	def __hash__(self):
		return id(self)

	def getLength(self):
		return self.length if isinstance(self.length, int) else None

	def generalise(self):
		return Sail_t_bits(self.length, signed=self.signed)

	def pp(self):
		constr = "sbits" if self.signed else "bits"
		return f"{constr}({self.length})"

	def isPrintable(self):
		return self.length is not None and self.length > 0

class Sail_t_bitfield(SailType):
	"""Represents a bitfield type"""
	def __init__(self, name, length, fields=[]):
		self.name = name
		self.length = length
		self.fields = fields

	def __eq__(self, other):
		return type(self) == type(other) and self.name == other.name and self.length == other.length and self.fields == other.fields
	def __hash__(self):
		return id(self)

	def getName(self):
		return self.name

	def getLength(self):
		return self.length if isinstance(self.length, int) else None

	def getFields(self):
		return self.fields

	def getFieldType(self, field):
		field = [f for f in self.fields if f[0].upper() == field.upper()]
		if field:
			high = field[0][1]
			low = field[0][2]
			return Sail_t_bits(high - low + 1)
		else:
			raise KeyError()

	def generalise(self):
		return Sail_t_bitfield(self.name, self.length)

	def pp(self):
		return utils.sanitiseSymbol(self.name)

class Sail_t_nat(SailType):
	"""Represents the nat type"""
	def __init__(self):
		super(Sail_t_nat, self).__init__()

	def generalise(self):
		return Sail_t_int()

	def pp(self):
		return "nat"

class Sail_t_range(SailType):
	"""Represents the range type"""
	def __init__(self, low, high):
		'''
		Range is __inclusive__ between low and high

		Args:
			- low : int
			- high : int >= low
		'''
		super(Sail_t_range, self).__init__()
		if high < low: sys.exit(f"Error: tried to construct a range type with high < low.  high = {high}, low = {low}")
		self.low = low
		self.high = high

	def getRange(self):
		return self.low, self.high

	def __eq__(self, other):
		return type(self) == type(other) and \
				self.low == other.low and \
				self.high == other.high
	def __hash__(self):
		return id(self)

	def generalise(self):
		return Sail_t_int()

	def pp(self):
		return f"range({self.low}, {self.high})"

class Sail_t_fn(SailType):
	"""
	Represents a function type: 
			lhs -> rhs
	"""
	def __init__(self, lhs : list[SailType], rhs : SailType, effects=set()):
		"""
		Args:
			- lhs : [SailType]
			- rhs : SailType
			- effects : set(Str) - should only be used for handwritten functions
		"""
		super(Sail_t_fn, self).__init__()
		self.lhs = lhs
		self.rhs = rhs
		self.effects = effects

	def __eq__(self, other):
		return type(self) == type(other) and \
				self.lhs == other.lhs and \
				self.rhs == other.rhs and \
				self.effects == other.effects
	def __hash__(self):
		return id(self)

	def getLHS(self):
		return self.lhs

	def getRHS(self):
		return self.rhs

	def getNumFormals(self):
		return len(self.lhs)

	def getEffects(self):
		return self.effects

	def generalise(self):
		return Sail_t_fn(
			lhs=[a.generalise() for a in self.lhs],
			rhs=self.rhs.generalise(),
			effects=self.effects
		)

	def containsUnknown(self):
		return any(containsUnknownType(t) for t in self.lhs) or containsUnknownType(self.rhs)

	def pp(self):
		lhs = Sail_t_tuple(self.lhs) if len(self.lhs) > 0 else Sail_t_unit()
		return f"{lhs.pp()} -> {self.rhs.pp()}"

class Sail_t_member(SailType):
	"""
	Represents a Sail type of form {|...|} e.g. {|8, 16, 32, 64|}.  Supports
	members of type string or int.
	"""
	INT = 0
	STR = 1

	def __init__(self, members):
		'''
		Args:
			- members : [int] | [keyword : str]

		TODO:
			- Maybe use sets instead
			- Make string representation in Sail explicit rather than using Python strings
		'''
		super(Sail_t_member, self).__init__()

		# Set the type of this member to int or string
		if all(isinstance(item, int) for item in members):
			self.subType = Sail_t_member.INT
		elif all(isinstance(item,str) and item.startswith(':') for item in members):
			self.subType = Sail_t_member.STR
		else:
			sys.exit(f"Error: Sail_t_member argument not homogeneous ints or keywords - {members}")

		# Set the members themselves
		self.members = members

	def __eq__(self, other):
		# Test type
		if (type(self)) != type(other):
			return False

		# Test for subtype
		if self.subType != other.subType:
			return False

		if len(self.members) != len(other.members):
			return False

		# Compare the lists
		membersEq = True
		selfMembers = sorted(self.members)
		otherMembers = sorted(other.members)
		for i in range(len(self.members)):
			item = selfMembers[i]
			if isinstance(item, int):
				membersEq = (membersEq and item == otherMembers[i])
			else: # String
				membersEq = (membersEq and item.upper() == otherMembers[i].upper())

		return membersEq

	def __hash__(self):
		return id(self)

	def generalise(self):
		if self.subType == Sail_t_member.INT:
			return Sail_t_int()
		elif self.subType == Sail_t_member.STR:
			return Sail_t_string()
		else:
			sys.exit("Error: type of member not set in generalise()")

	def pp(self):
		if self.subType == Sail_t_member.INT:
			inner = ', '.join([str(item) for item in sorted(self.members)])
			return f"{{|{inner}|}}"
		elif self.subType == Sail_t_member.STR:
			# Don't think we can have member of string so just generalise to string
			return Sail_t_string().pp()
		else:
			sys.exit(f"Error: type of member not set in pp()")


class Sail_t_option(SailType):
	"""
	Represents a Sail maybe type

	TODO: redefine in terms of more general structs
	"""
	def __init__(self, typ):
		"""
		Args:
			- type : SailType - the inner type of the maybe
		"""
		super(Sail_t_option, self).__init__()
		self.typ = typ

	def __eq__(self, other):
		return type(self) == type(other) and self.typ == other.typ

	def __hash__(self):
		return id(self)

	def getTyp(self):
		return self.typ

	def generalise(self):
		return Sail_t_option(self.typ.generalise())

	def containsUnknown(self):
		return self.typ.containsUnknown()

	def pp(self):
		return f"option({self.typ.pp()})"

def isStringOption(t):
	return isinstance(t, Sail_t_option) and isinstance(t.getTyp(), Sail_t_string)

class Sail_t_tuple(SailType):
	"""
	Represents a Sail tuple
	"""
	def __init__(self, subTypes):
		"""
		Args:
			- subTypes : [SailType]
		"""
		super(Sail_t_tuple, self).__init__()
		self.subTypes = subTypes

	def __eq__(self, other):
		if type(self) != type(other):
			return False

		if len(self.subTypes) != len(other.subTypes):
			return False

		for i in range(len(self.subTypes)):
			if self.subTypes[i] != other.subTypes[i]:
				return False

		return True

	def __hash__(self):
		return id(self)

	def getSubTypes(self):
		return self.subTypes


	def generalise(self):
		return Sail_t_tuple([t.generalise() for t in self.subTypes])

	def containsUnknown(self):
		return any(t.containsUnknown() for t in self.subTypes)

	def pp(self):
		return f"({', '.join([item.pp() for item in self.subTypes])})"

	def isPrintable(self):
		return all(t.isPrintable() for t in self.subTypes)

class Sail_t_struct(SailType):
	"""
	Represents a Sail struct
	"""
	def __init__(self, struct):
		"""
		Args:
			struct : SailStruct
		"""
		super(Sail_t_struct, self).__init__()
		self.struct = struct

	def __eq__(self, other):
		if type(self) != type(other):
			return False

		if self.struct != other.struct:
			return False

		return True

	def __hash__(self):
		return id(self)

	def getStruct(self):
		return self.struct

	def generalise(self):
		return Sail_t_struct(self.struct)

	def containsUnknown(self):
		return any(t.containsUnknown() for t in self.struct.getFields().values())

	def pp(self):
		return self.struct.getName()

class Sail_t_vector(SailType):
	"""
	Represents a Sail vector
	"""
	def __init__(self, length, subType):
		"""
		Args:
			length: int
			subType: SailType
		"""
		super(Sail_t_vector, self).__init__()
		self.length = length
		self.subType = subType

	def __eq__(self, other):
		if type(self) != type(other):
			return False

		if self.subType != other.subType or self.length != other.length:
			return False

		return True

	def __hash__(self):
		return id(self)

	def getLength(self):
		return self.length

	def getSubType(self):
		return self.subType

	def generalise(self):
		return Sail_t_vector(self.length, self.subType.generalise())

	def containsUnknown(self):
		return self.subType.containsUnknown()

	def pp(self):
		return f"vector({self.length}, dec, {self.subType.pp()})"

class Sail_t_list(SailType):
	"""
	Represents a Sail list
	"""
	def __init__(self, subType):
		"""
		Args:
			subType: SailType
		"""
		super(Sail_t_list, self).__init__()
		self.subType = subType

	def __eq__(self, other):
		if type(self) != type(other):
			return False

		if self.subType != other.subType:
			return False

		return True

	def __hash__(self):
		return id(self)

	def generalise(self):
		return Sail_t_list(self.subType.generalise())

	def containsUnknown(self):
		return self.subType.containsUnknown()

	def pp(self):
		return f"list({self.subType.pp()})"


def isRangeType(typ):
	return (isinstance(typ, Sail_t_range) or (isinstance(typ, Sail_t_member) and typ.subType == Sail_t_member.INT))

def getRangeOfType(typ):
	if isinstance(typ, Sail_t_range):
		return (typ.low, typ.high)
	elif isinstance(typ, Sail_t_member) and typ.subType == Sail_t_member.INT:
		return (min(typ.members), max(typ.members))
	else:
		return None

def getBitvectorSize(typ):
	if isinstance(typ, Sail_t_bits):
		return typ.getLength()
	elif isinstance(typ, Sail_t_bitfield):
		return typ.getLength()
	elif isRangeType(typ):
		# Determine number of bits required (and make sure that it is at least 1)
		(low, high) = getRangeOfType(typ)
		numBits = math.ceil(math.log2(max(abs(low), abs(high) + 1, 2)))
		return numBits if low >= 0 and high >= 0 else numBits + 1
	else:
		return None

def isBitvectorType(typ):
	return (getBitvectorSize(typ) is not None)

def mergeTypes(t1, t2):
	if t1 == t2:
		return t1
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_member):
		return Sail_t_member(list(set(t1.members + t2.members)))
	elif isRangeType(t1) and isRangeType(t2):
		(l1, h1) = getRangeOfType(t1)
		(l2, h2) = getRangeOfType(t2)
		return Sail_t_range(min(l1, l2), max(h1, h2))
	elif isBitvectorType(t1) and isBitvectorType(t2):
		signed = isSignedType(t1) or isSignedType(t2)
		length1 = getBitvectorSize(t1)
		length2 = getBitvectorSize(t2)
		if isNonnegativeType(t1) and isSignedType(t2):
			# Add a bit to the unsigned type when merging with a
			# signed type, so that the MSB can be a sign bit
			print(f"mergeTypes: Adding a bit to {t1.pp()} when merging with {t2.pp()}")
			length1 = length1 + 1
		if isSignedType(t1) and isNonnegativeType(t2):
			print(f"mergeTypes: Adding a bit to {t2.pp()} when merging with {t1.pp()}")
			length2 = length2 + 2
		length = max(length1, length2)
		if length1 >= length2 and isinstance(t1, Sail_t_bitfield):
			return t1
		elif length1 <= length2 and isinstance(t2, Sail_t_bitfield):
			return t2
		else:
			return Sail_t_bits(length, signed=signed) if length > 0 else None
	elif (isNumeric(t1) and isNumeric(t2)) \
	     or (isNumeric(t1) and isBitvectorType(t2)) \
	     or (isBitvectorType(t1) and isNumeric(t2)):
		return Sail_t_int()
	elif isinstance(t1, Sail_t_tuple) and isinstance(t2, Sail_t_tuple):
		ts1 = t1.getSubTypes()
		ts2 = t2.getSubTypes()
		if len(ts1) != len(ts2):
			return None
		merged = []
		for i in range(len(ts1)):
			t = mergeTypes(ts1[i], ts2[i])
			if t is None:
				return None
			merged = merged + [t]
		return Sail_t_tuple(merged)
	elif isinstance(t2, Sail_t_unknown) or isinstance(t2, Sail_t_error):
		return t1
	elif isinstance(t1, Sail_t_unknown) or isinstance(t1, Sail_t_error):
		return t2
	elif isinstance(t1.generalise(), Sail_t_string) and isinstance(t2.generalise(), Sail_t_string):
		return Sail_t_string()
	else:
		return None

def isSubType(t1, t2):
	if t1 == t2:
		return True
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_member) and t1.subType == t2.subType:
		for i in t1.members:
			if i not in t2.members:
				return False
		return True
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_range) and t1.subType == Sail_t_member.INT:
		return t2.low <= min(t1.members) and max(t1.members) <= t2.high
	elif isinstance(t1, Sail_t_range) and isinstance(t2, Sail_t_member) and t2.subType == Sail_t_member.INT:
		return isSubType(Sail_t_member(list(range(t1.low, t1.high))), t2)
	elif isinstance(t1, Sail_t_range) and isinstance(t2, Sail_t_range):
		return t2.low <= t1.low and t1.high <= t2.high
	elif isRangeType(t1) and isinstance(t2, Sail_t_nat):
		(low, high) = getRangeOfType(t1)
		return low >= 0 and high >= 0
	elif isNumeric(t1) and isinstance(t2, Sail_t_int):
		return True
	elif isString(t1) and isString(t2):
		return True
	elif isinstance(t1, Sail_t_option) and isinstance(t2, Sail_t_option):
		return isSubType(t1.getTyp(), t2.getTyp())
	elif isinstance(t1, Sail_t_tuple) and isinstance(t2, Sail_t_tuple) and len(t1.getSubTypes()) == len(t2.getSubTypes()):
		return all([isSubType(t1.getSubTypes()[i], t2.getSubTypes()[i]) for i in range(len(t1.getSubTypes()))])
	else:
		return False

def intersectTypes(t1, t2):
	if t1 == t2:
		return t1
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_member) and t1.subType == t2.subType:
		members = [i for i in t1.members if i in t2.members]
		return Sail_t_member(members) if members != [] else None
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_range) and t1.subType == Sail_t_member.INT:
		members = [i for i in t1.members if t2.low <= i and i <= t2.high]
		return Sail_t_member(members) if members != [] else None
	elif isinstance(t2, Sail_t_member) and isinstance(t1, Sail_t_range) and t2.subType == Sail_t_member.INT:
		members = [i for i in t2.members if t1.low <= i and i <= t1.high]
		return Sail_t_member(members) if members != [] else None
	elif isinstance(t1, Sail_t_member) and isinstance(t2, Sail_t_nat) and t1.subType == Sail_t_member.INT:
		members = [i for i in t1.members if i >= 0]
		return Sail_t_member(members) if members != [] else None
	elif isinstance(t2, Sail_t_member) and isinstance(t1, Sail_t_nat) and t2.subType == Sail_t_member.INT:
		members = [i for i in t2.members if i >= 0]
		return Sail_t_member(members) if members != [] else None
	elif isinstance(t1, Sail_t_range) and isinstance(t2, Sail_t_range):
		low = max(t1.low, t2.low)
		high = min(t1.low, t2.low)
		return Sail_t_range(low, high) if low <= high else None
	elif isRangeType(t1) and isinstance(t2, Sail_t_nat):
		(low, high) = getRangeOfType(t1)
		return Sail_t_range(max(0, low), max(0, high)) if low >= 0 or high >= 0 else None
	elif isRangeType(t2) and isinstance(t1, Sail_t_nat):
		(low, high) = getRangeOfType(t2)
		return Sail_t_range(max(0, low), max(0, high)) if low >= 0 or high >= 0 else None
	elif isNumeric(t1) and isinstance(t2, Sail_t_int):
		return t1
	elif isNumeric(t2) and isinstance(t1, Sail_t_int):
		return t2
	elif isinstance(t1, Sail_t_option) and isinstance(t2, Sail_t_option):
		t = intersectTypes(t1.getTyp(), t2.getTyp())
		return Sail_t_option(t) if t is not None else None
	elif isinstance(t1, Sail_t_tuple) and isinstance(t2, Sail_t_tuple) and len(t1.getSubTypes()) == len(t2.getSubTypes()):
		typs = []
		for (i, t1) in enumerate(t1.getSubTypes()):
			t = intersectTypes(t1, t2.getSubTypes()[i])
			if t is None:
				return None
			else:
				typs.append(t)
		return Sail_t_tuple(typs)
	else:
		return None
