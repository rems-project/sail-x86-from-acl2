from lex_parse import NewLine
import transform

import sys

"""
Contains the classes which represent types in Sail, and some utility functions

Types must implement certain functions.  Namely:
 - generalise()
 - containsUnknown()
 - pp()
"""

# =============================================================================
# Utility functions
# =============================================================================

class eqSet():
	def __init__(self, init = None):
		'''
		TODO: extend set instead?

		Args:
			- init : list
		'''
		self.set = []

		if init != None:
			for i in init:
				self.add(i)

	def add(self, obj):
		if obj not in self.set: # Uses __eq__ not __hash__ (I hope)
			self.set.append(obj)

	def extend(self, objs):
		if type(objs) == eqSet:
			toAdd = objs.set
		elif type(objs) == list:
			toAdd = objs

		for obj in toAdd:
			self.add(obj)

	def peek(self):
		return self.set[0]

	def all(self, pred):
		'''
		https://stackoverflow.com/questions/10666163/how-to-check-if-all-elements-of-a-list-matches-a-condition
		
		Args:
			- pred : SailType -> bool
		'''
		return all(pred(item) for item in self.set)

	def resolve(self):
		'''
		Check if all items in a set are consistent with some overarching type
		'''
		# Check we can resolve to a single overarchign type
		genType = self.set[0].generalise()
		for t in self.set[1:]:
			if t.generalise() != genType:
				sys.exit(f"Error: can't generalise set - {self.set}")

		# Return this type if it exists
		return genType

	def __str__(self):
		return f"{', '.join([str(i) for i in self.set])}"

	def __len__(self):
		return len(self.set)

def dictExtend(orig, additions):
	'''
	For each element in additions, if the key is not in orig, adds the
	(key, value) pair.  If key is in orig, adds to the Set in orig accordingly

	Args:
		- orig : {'a : Set('b)}
		- additions: {'a : Set('b)}
	Returns:
		- {'a : Set('b)}
	'''
	for (k, v) in additions.items():
		if k in orig:
			orig[k].union(v)
		else:
			orig[k] = v

	return orig

def dictAddOrInit(d, k, v):
	'''
	If k in d
	then add v to set d[k]
	else initialise d[k] to set(v)
	'''
	if k in d:
		d[k].add(v)
	else:
		d[k] = eqSet([v])

def translateType(env, typeSymbol, args=None):
	'''
	Given an acl2 symbol `typeSymbol`, and optional arguments to it, returns
	a Python class representing the type, or None on failure.

	See here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____TYPE-SPEC

	Args:
		- typeSymbol : str e.g. `unsigned-byte`
		- args : [ACL2astElem] e.g. `['9']`
	Returns:
		- SailType
	'''
	# Translate any problematic args
	argsSanatised = []
	if args != None:
		for a in args:
			if transform.convertLiteral(a) != None or a == '*':
				argsSanatised.append(a)
			else:
				# Hope it's a macro translated to a quoted literal
				# TODO: go via env.lookup instead
				newA = env.evalACL2([':trans', a], debracket=True)
				newA = newA[0].getAST()
				argsSanatised.append(newA)
		args = argsSanatised

	# Tranlate the type
	typeSymbol = typeSymbol.upper()

	if typeSymbol == 'unsigned-byte'.upper():
		return ( Sail_t_range(0, 2**int(args[0]) - 1) )
	elif typeSymbol == 'signed-byte'.upper():
		halfBits = int(args[0]) - 1
		return ( Sail_t_range(-2**(halfBits), 2**halfBits - 1) )
	elif typeSymbol == 'integer'.upper():
		if args == None:
			return ( Sail_t_int() )
		if args[0] == '0' and args[1] == '*':
			return ( Sail_t_nat() )
		elif '*' in args:
			return ( Sail_t_int() )
		else:
			low = int(args[0])
			high = int(args[1])
			return ( Sail_t_range(low, high) )
	elif typeSymbol == 'member'.upper():
		# Make sure each member is an int or each member is a keyword
		maybeNums = [transform.convertLiteral(a) for a in args]
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
	'''
	Takes a guard ACL2 expression and tries to extract type information from it
	Args:
		- guard : [ACL2astElem]
	Returns:
		- {str : eqSet(SailType)}
	'''
	# `possibilities` is a {str : eqSet(SailType)} mapping names to possible types
	possibilities = {}

	# Filter newlines
	guard = [item for item in guard if type(item) not in [NewLine]]

	# Switch on the first element of guard
	switch = guard[0]
	if switch.lower() == 'and':
		for subGuard in guard[1:]:
			possibilities = dictExtend(possibilities, parseGuard(subGuard))
	elif switch.lower() == 'natp':
		if len(guard) != 2: sys.exit(f"Error: guard for natp has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_nat())
	elif switch.lower() == 'integerp':
		if len(guard) != 2: sys.exit(f"Error: guard for integerp has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_int())
	elif switch.lower() == 'unsigned-byte-p':
		if len(guard) != 3: sys.exit(f"Error: guard for unsigned-byte-p has unexpected number of elements: {guard}")
		name = guard[2]
		hiBits = transform.convertLiteral(guard[1])
		if hiBits == None or isinstance(hiBits, float) or hiBits < 0:
			print(f"Warning: none-integer value supplied for unsigned-byte-p argument: {guard}")
		else:
			dictAddOrInit(possibilities, name, Sail_t_range(0, 2 ^ hiBits - 1))
	elif switch.lower() == 'n01p':
		# TODO: this is for the function `extract-64-bits` - would be better to use fact that we match against a numeric
		# type to infer this type.
		if len(guard) != 2: sys.exit(f"Error: guard for n01p has unexpected number of elements: {guard}")
		name = guard[1]
		dictAddOrInit(possibilities, name, Sail_t_range(0, 1))
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
	'''
	Args:
		- st : SailType
	Returns:
		- bool
	'''
	return isinstance(st.generalise(), Sail_t_int)

def isString(st):
	return isinstance(st.generalise(), Sail_t_string)

def isSetPred(sts, p):
	'''
	Args:
		sts: eqSet(SailType)
		p: SailType -> bool

	Returns: bool

	TODO: do we really need this function?
	'''
	return sts.all(p)

# =============================================================================
# Types
# =============================================================================
class SailType():
	"""
	Stub class representing Sail types
	"""
	def __init__(self):
		self.effects = set([])  # Has type set(Str)
	def __eq__(self, other):
		return type(self) == type(other)
	def __hash__(self):
		return id(self)
	def containsUnknown(self):
		return False

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
		return "unknown"
		# sys.exit("Error: Tried to pp an unknwon type - these should have all been resolved")

class Sail_t_error(SailType):
	"""
	A 'throw' may take on any value.  This class represents that value, but it should be resolved at translate-time.
	"""
	def __init__(self):
		super(Sail_t_error, self).__init__()

	def generalise(self):
		return Sail_t_error()

	def __eq__(self, other):
		if issubclass(type(other), SailType):
			return True

		return False

	def __hash__(self):
		return id(self)

	def pp(self):
		print("WARNING: printing Sail_t_error() type")
		return "error"

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
	def __init__(self, length):
		'''
		Args:
			- length : int > 0 - I think this needs to be strictly bigger than zero
		'''
		super(Sail_t_bits, self).__init__()
		if length <= 0: sys.exit(f"Error: tried to construct a bits type with size le 0.  Size was: {length}")
		self.length = length

	def __eq__(self, other):
		return type(self) == type(other) and self.length == other.length
	def __hash__(self):
		return id(self)

	def generalise(self):
		return Sail_t_bits(self.length)

	def pp(self):
		return f"bits({self.length})"

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
		return (self.low, self.high)

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
	def __init__(self, lhs : [SailType], rhs : SailType, effects=set()):
		'''
		Args:
			- lhs : [SailType]
			- rhs : SailType
			- effects : set(Str) - should only be used for handwritten functions
		'''
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
		return any(t.containsUnknown() for t in self.lhs) or self.rhs.containsUnknown()

	def pp(self):
		return f"({', '.join([item.pp() for item in self.lhs])}) -> {self.rhs.pp()}"

class Sail_t_member(SailType):
	"""
	Represents a Sail type of form {|...|} e.g. {|8, 16, 32, 64|}
	"""
	INT = 0
	STR = 1

	def __init__(self, members):
		'''
		Args:
			- members : [int] | [keyword : str]

		TODO: maybe use sets instead
		TODO: make string representation in Sail explicit rather than using Python strings
		'''
		super(Sail_t_member, self).__init__()

		# Set the type of this member to int or string
		if all(isinstance(item, int) for item in members):
			self.subType = Sail_t_member.INT
		elif all(isinstance(item,str) and item.startswith(':') for item in members):
			self.subType = Sail_t_member.STR
		else:
			sys.exit(f"Error: Sail_t_member argument not homogeneous ints or keywords - {members}")

		# Set the memebers themselves
		self.members = members

	def __eq__(self, other):
		# Test type
		if (type(self)) != type(other):
			return False

		# Test for subtype
		if self.subType != other.subType:
			return False

		# Compare the lists
		membersEq = True
		for (i, item) in self.members:
			if isinstance(item, int):
				membersEq = (membersEq and item == other.members[i])
			else: # String
				membersEq = (membersEq and item.upper() == other.members[i].upper())

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
			inner = ', '.join([item for item in self.members])
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
		'''
		Args:
			- type : SailType - the inner type of the maybe
		'''
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

class Sail_t_tuple(SailType):
	"""
	Represents a Sail tuple
	"""
	def __init__(self, subTypes):
		'''
		Args:
			- subTypes : [SailType]
		'''
		super(Sail_t_tuple, self).__init__()
		self.subTypes = subTypes

	def __eq__(self, other):
		if type(self) != type(other):
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
		'''
		Args:
			length: int
			subType: SailType
		'''
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
		'''
		Args:
			subType: SailType
		'''
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