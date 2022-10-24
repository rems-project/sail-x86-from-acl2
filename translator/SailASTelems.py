from lex_parse import ACL2Comment
from SailTypes import *
import utils

import sys
import os

"""
This file is split into two parts:

 1. Classes defining Sail AST elements.
 2. Helper and utility function which uses these classes.

The following Sail syntax elements are represented:

 -  SailFn					Function definitions.
 -  SailHandwrittenFn		Represents functions whose behaviour is implemented
 							manually.
 - 	SailLet					A `let <var> = <expr> in <body>` expression.
 -  SailPlaceholderNil		Lisp `nil` can translate to this before its actual
 							value is resolved.
 -  SailBoundVar			A bound variable for use in functions and `let`
 							bindings.
 -  SailApp					Function application.
 -  SailNumLit				A number literal.
 -  SailBoolLit				A boolean literal.
 -  SailStringLit			A string literal.
 -  SailUnderscoreLit		An underscore for use in pattern matching.
 -  Sail if					An if expression.
 -  SailInclude				An `$include`.
 -  SailTuple				A tuple.
 -  SailMatch				A match expression.
 -  SailStruct				Represents the definition of a struct type
 -  SailStructLit			Represents a struct being initialised with values
 -  SailStructProject		Represents a value beint extracted from a struct
 -  SailVectorLit			A vector literal
 -  SailVectorProject		Represents indexing into a vector
 -  SailListLit				A list literal

Each AST element should implement the following methods:
 - pp() 		Pretty printing function which returns a string (preferably
 				without a newline at the end).
 - getType() 	Returns the type (types definted in SailTypes.py) 
 - getEffects	Take a context ([str]) and returns set(tr) representing the
 				effects.

Most AST elements should also implement:
 - getChildrenByPred	Given a predicate function p, will return itself if
 						`p(self)` as well as the result of the recursive call
 						on its children. 								
"""


################################################################################
# Classes
################################################################################


class SailASTelem():
	"""
	Superclass for Sail AST elements.
	"""
	def __init__(self):
		pass

	def getChildrenByPred(self, p):
		if p(self):
			return {self} # A set
		else:
			return set()

class SailFn(SailASTelem):
	"""Sail function definition"""
	def __init__(self, name=None, formals=None, body=None):
		"""
		It is not necessary to set any value at initialisation.  This is
		because we want to create this object before the body is translated in
		order to allow recursion to take place.

		Args:
			- name : str
			- formals : [SailBoundVar]
			- body : [SailAstElem]
		"""
		super().__init__()
		self.setName(name)
		self.formals = formals
		self.setBody(body)

		self.forceRHSType = None

	### Custom methods ###
	def setName(self, name):
		if name is None:
			self.name = None
		else:
			self.name = utils.sanitiseSymbol(name)

	def getName(self):
		return self.name

	def getFormals(self):
		return self.formals

	def setFormals(self, formals):
		self.formals = formals

	def getNumFormals(self):
		return len(self.formals)

	def setBody(self, body):
		if body is not None and len(body) != 1:
			sys.exit(f"Length of function body not 1: {body}")
		self.body = body

	def getBody(self):
		return self.body

	def setForceRHSType(self, typ):
		self.forceRHSType = typ

	def setKeyDefaults(self, keyDefaults):
		self.keyDefaults = keyDefaults

	### Required methods ###
	def getType(self):
		if self.forceRHSType is None:
			return Sail_t_fn(
				lhs=[f.getType() for f in self.formals],
				rhs=self.body[0].getType()
			)
		else:
			return Sail_t_fn(
				lhs=[f.getType() for f in self.formals],
				rhs=self.forceRHSType
			)

	def getEffects(self, ctx):
		ctx.append(self.name)
		toReturn = self.body[0].getEffects(ctx)
		ctx.pop()

		return toReturn

	def getChildrenByPred(self, p):
		formalsSet = utils.unionListOfSets(f.getChildrenByPred(p) for f in self.formals)
		bodySet = self.body[0].getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(formalsSet, bodySet, selfSet)

	def pp(self):
		sanitisedName = utils.sanitiseSymbol(self.name)

		# Only construct an effects string if there are any effects
		# Print effects in order to help with version control
		if len(self.getEffects([])) != 0:
			effectsInner = ", ".join(sorted(self.getEffects([])))
			effectsString = f" effect {{{effectsInner}}}"
		else:
			effectsString = ""

		# Combine
		typeSig = f"val {sanitisedName} : {self.getType().pp()}{effectsString}"
		header = f"function {sanitisedName} ({', '.join([utils.sanitiseSymbol(item.getName(), includeFnNames=True) for item in self.formals])}) ="
		body = "\n".join([elem.pp() for elem in self.body])

		return "\n".join([typeSig, header, body]) + "\n"

class SailHandwrittenFn(SailASTelem):
	"""
	Some functions we will write by hand because there is little
	point in trying to translate them.  Basic and utility functions
	fall in this category, e.g. common arithmetic ops like addition.

	These handwritten functions are resident in another file, but objects
	of this type represent which functions are available to us, and their
	type.
	"""
	def __init__(self, name, typ=None, infix=False):
		"""
		Args:
			- name : str
			- typ : Sail_t_fn
		"""
		super().__init__()
		self.name = utils.sanitiseSymbol(name, lower=False, infix=infix)
		self.typ  = typ

	### Custom methods ###
	def getName(self):
		return self.name

	def getNumFormals(self):
		return self.typ.getNumFormals()

	def setType(self, t):
		self.typ = t

	### Required methods ###
	def getType(self):
		return self.typ

	def getEffects(self, ctx):
		return self.typ.getEffects()

	def pp(self):
		sys.exit("Error: handwritten sail functions are already defined and so their pp() function should not be called")


class SailPatchedDefinition(SailASTelem):
	"""
	Some functions we will write by hand because there is little
	point in trying to translate them.  Basic and utility functions
	fall in this category, e.g. common arithmetic ops like addition.

	These handwritten functions are resident in another file, but objects
	of this type represent which functions are available to us, and their
	type.
	"""
	def __init__(self, name, typ, bodyText):
		"""
		Args:
			- name : str
			- typ : Sail_t_fn
		"""
		super().__init__()
		self.name = utils.sanitiseSymbol(name)
		self.typ = typ
		self.bodyText = bodyText

	### Custom methods ###
	def getName(self):
		return self.name

	def getNumFormals(self):
		return self.typ.getNumFormals()

	def setType(self, t):
		self.typ = t

	### Required methods ###
	def getType(self):
		return self.typ

	def getEffects(self, ctx):
		return self.typ.getEffects()

	def pp(self):
		return self.bodyText


class SailLet(SailASTelem):
	"""Let expression in Sail"""
	def __init__(self, varName, expr, body):
		"""
		For: `let varName = expr in body`
		Or:  `let (x, y) = expr in body`

		Args:
			- varName : SailBoundVar | SailTuple
			- expr : [SailAstElem]
			- body : [SailAstElem]
		"""
		super().__init__()
		if len(expr) != 1: sys.exit(f"Error: expr length not 1 in `let` getEffects - {expr}")
		if len(body) != 1: sys.exit(f"Error: body length not 1 in `let` getEffects - {body}")

		self.varName = varName
		self.expr = expr
		self.body = body

		self.resolveNilCB = None

	### Custom Methods ###
	def getVarName(self):
		return self.varName

	def getExpr(self):
		return self.expr

	def getBody(self):
		return self.body

	def resolveTypes(self, file, justPrint=False):
		return
		#file.write(f"Let Resolve Types - {self.varName.pp()}\n")
		#file.write(f"Binding: {self.varName.getType().pp()}\n")
		#file.write(f"Expression: {self.expr[0].getType().pp()}\n\n")

		#sys.exit("Implement resolving of types of SailLet")

	### Required methods ###
	def getType(self):
		# Filter and check length
		bodyFiltered = self.body
		if len(bodyFiltered) != 1: sys.exit(f"Error: incorrect body length in `let` - {bodyFiltered}")

		return bodyFiltered[0].getType()

	def getEffects(self, ctx):
		exprEffects = self.expr[0].getEffects(ctx).copy()
		bodyEffects = self.body[0].getEffects(ctx).copy()

		totalEffects = exprEffects.union(bodyEffects)

		return totalEffects

	def getChildrenByPred(self, p):
		if p(self.varName):
			varSet = set(self.varName)
		else:
			varSet = set()

		exprSet = self.expr[0].getChildrenByPred(p)
		bodySet = self.body[0].getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(varSet, exprSet, bodySet, selfSet)

	def pp(self):
		exprTyp = self.expr[0].getType()
		pp_typ = f" : {ppType(exprTyp)}" if isPrintableType(exprTyp) else ""
		pp_var = self.varName.pp() + pp_typ
		if isinstance(self.expr[0], SailMatch):
			# SailMatch prints a type annotation by default, but we
			# already have one on the LHS
			pp_expr = f"({self.expr[0].pp(withAnnotation=False)})"
		else:
			pp_expr = f"({self.expr[0].pp()})"
		if isMultilineExpr(self.expr[0]):
			# Wrap (potential) multi-line expressions in a block
			pp_expr = "{ " + pp_expr + " }"
		pp_body = self.body[0].pp()
		
		return f"let {pp_var} = {pp_expr} in\n{pp_body}"


class SailBlock(SailASTelem):
	"""Block in Sail"""
	def __init__(self, exprs):
		"""
		For: `let varName = expr in body`
		Or:  `let (x, y) = expr in body`

		Args:
			- varName : SailBoundVar | SailTuple
			- expr : [SailAstElem]
			- body : [SailAstElem]
		"""
		super().__init__()

		if any(not isUnitType(e.getType()) for e in exprs[:-1]):
			sys.exit("Error: Non-unit expression in block")

		self.exprs = exprs

	### Custom Methods ###
	def getExprs(self):
		return self.exprs

	### Required methods ###
	def getType(self):
		return self.exprs[-1].getType()

	def getEffects(self, ctx):
		return set.union(*[e.getEffects(ctx).copy() for e in self.exprs])

	def getChildrenByPred(self, p):
		exprSet = set.union(*[e.getChildrenByPred(p) for e in self.exprs])
		selfSet = super().getChildrenByPred(p)

		return set.union(exprSet, selfSet)

	def pp(self):
		exprs_pp = [e.pp() for e in self.exprs]
		return '{ ' + ';\n'.join(exprs_pp) + '\n}'


class SailAssign(SailASTelem):
	"""Assignment expression in Sail"""
	def __init__(self, lhs, rhs):
		"""
		For: `let varName = expr in body`
		Or:  `let (x, y) = expr in body`

		Args:
			- lhs : SailASTelem
			- rhs : SailAstElem
		"""
		super().__init__()

		self.lhs = lhs
		self.rhs = rhs

	### Required methods ###
	def getType(self):
		return Sail_t_unit()

	def getEffects(self, ctx):
		lhsEffects = self.lhs.getEffects(ctx).copy()
		rhsEffects = self.rhs.getEffects(ctx).copy()

		totalEffects = lhsEffects.union(rhsEffects)

		return totalEffects

	def getChildrenByPred(self, p):
		lhsSet = self.lhs.getChildrenByPred(p)
		rhsSet = self.rhs.getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(lhsSet, rhsSet, selfSet)

	def pp(self):
		return f"{self.lhs.pp()} = {self.rhs.pp()}"


class SailPlaceholderNil(SailASTelem):
	"""Represents a `nil` in the ACL2 code which hasn't yet been resolved in the Sail code"""
	# Class variables
	DEFAULT = 1
	BOOL = 2
	UNKNOWN = 3
	nilTypes = (DEFAULT, BOOL, UNKNOWN)

	def __init__(self, nilType=1):
		super().__init__()
		if nilType not in SailPlaceholderNil.nilTypes: sys.exit("Error: nilType not valid")

		self.resolveNilCB = None
		self.nilType = nilType

	def resolve(self, resolvedType):
		if isinstance(resolvedType, Sail_t_option) and isinstance(resolvedType.getTyp(), Sail_t_string):
			self.nilType = SailPlaceholderNil.DEFAULT
		elif isinstance(resolvedType, Sail_t_bool):
			self.nilType = SailPlaceholderNil.BOOL
		else:
			self.nilType = SailPlaceholderNil.UNKNOWN
			# print(f"Warning: tried setting SailPlaceholderNil to invalid type - {resolvedType}, {resolvedType.pp()} - will keep as default")

	### Required methods ###
	def getType(self):
		if self.nilType == SailPlaceholderNil.DEFAULT:
			return Sail_t_option(Sail_t_string())
		elif self.nilType == SailPlaceholderNil.BOOL:
			return Sail_t_bool()
		elif self.nilType == SailPlaceholderNil.UNKNOWN:
			return Sail_t_unknown()
		else:
			return Sail_t_PlaceholderNil()

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		if self.nilType == SailPlaceholderNil.DEFAULT:
			# TODO: use nonHelper() or similar instead of a raw string
			return 'None() : option(string)'
		elif self.nilType == SailPlaceholderNil.BOOL:
			# TODO: use SailBoolLit instead of a raw string
			return 'false'
		elif self.nilType == SailPlaceholderNil.UNKNOWN:
			return 'None()'
		else:
			sys.exit("Error: Tried to pp unresolved nil")


class SailBoundVar(SailASTelem):
	"""Represents a bound variable (e.g. from formal parameters of a function
	of from b*/let bindings"""
	def __init__(self, binding, typ=None, sanitise=True):
		"""
		Args:
			- binding : str
			- typ : SailType
		"""
		super().__init__()
		self.binding = binding
		self.setType(typ)
		self.sanitise = sanitise

	def getName(self):
		return self.binding

	### Custom methods ###
	def setType(self, typ):
		"""
		Args:
			- typ : SailType
		"""
		self.typ = typ

		if self.binding.lower() == 'x86':
			if typ is None or isinstance(typ, Sail_t_unknown):
				self.typ = Sail_t_unit()
			# else the type is being set explicitly, which we allow.

	### Required methods ###
	def getType(self):
		if self.typ is not None:
			return self.typ
		else:
			return Sail_t_unknown()
			#raise ValueError(f"Error: type of Sail `boundVar` is None - {self.binding}")

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		# TODO: remove printing the type for as many cases as possible
		if utils.sanitiseSymbol(self.binding) in ['les_lds_distinguishing_byte', 'max_offset']:
			return f"{utils.sanitiseSymbol(self.binding, includeFnNames=True)} : {self.getType().pp()}"
		elif self.sanitise:
			return utils.sanitiseSymbol(self.binding, includeFnNames=True)
		else:
			return self.binding


class SailApp(SailASTelem):
	"""Represents function application in sail"""
	def __init__(self, fn, actuals, infix=False, retType=None):
		"""
		Args:
			- fn : SailFn | SailHandwrittenFn
			- actuals : [SailAstElems]
			- infix : Bool
			- retTyp : SailTyp (to override the return type)
		"""
		super().__init__()
		self.fn = fn
		self.actuals = actuals
		self.infix = infix
		self.retType = retType

	### Custom methods ###
	def getFn(self):
		return self.fn

	def getActuals(self):
		return self.actuals

	def isInfix(self):
		return self.infix

	def resolveTypes(self, file, justPrint = False):
		"""
		Used by the type resolution algorithm to work out missing types.
		"""
		formalTypes = self.fn.getType().getLHS()
		actualTypes = [a.getType() for a in self.actuals]

		file.write(f"App Resolve Types - {self.fn.getName()}\n")
		file.write(f"Formals: {[f.pp() for f in formalTypes]}\n")
		file.write(f"Actuals: {[at.pp() for at in actualTypes]}\n\n")

		if not justPrint:
			# Resolve unknown types of variables from the formals
			# of the function, if we can
			for i in range(len(self.actuals)):
				fType = formalTypes[i]
				aType = actualTypes[i]
				if isUnknownType(aType) and \
						not isUnknownType(fType) and \
						isinstance(self.actuals[i], SailBoundVar):
					self.actuals[i].setType(fType)

	def resolve(self, resolvedType):
		"""
		Used for resolving type of `throw`.

		Args:
			resolvedType: SailType

		Returns:
			None
		"""
		if self.fn.getName().lower() != 'throw':
			print(f"Warning, try to resolve type of application to function {self.fn.getName()}")
		else:
			self.fn.setType(Sail_t_fn([], resolvedType, {'escape'}))

	### Required methods ###
	def getType(self):
		if self.retType is not None:
			return self.retType
		elif self.fn.getType() is None:
			sys.exit(f"Error: function {self.fn.getName()} has not RHS type - you may need to specify it yourself")
		else:
			return self.fn.getType().getRHS()

	def setType(self, typ):
		self.retType = typ

	def getEffects(self, ctx):
		# Check if function has any associated effects
		if self.fn.getName() in ctx:
			fnEffects = set([])
		else:
			fnEffects = self.fn.getEffects(ctx).copy()

		# Check if the evaluation of actuals causes any effects
		actualsEffects = [a.getEffects(ctx) for a in self.actuals]

		return fnEffects.union(*actualsEffects)

	def getChildrenByPred(self, p):
		# We ignore functions in the expectation they add themselves, but do want to take into account
		# handwritten functions here
		if isinstance(self.fn, SailHandwrittenFn):
			fnSet = self.fn.getChildrenByPred(p)
		else:
			fnSet = set()

		actualsSet = utils.unionListOfSets([a.getChildrenByPred(p) for a in self.actuals])

		selfSet = super().getChildrenByPred(p)

		return set.union(fnSet, actualsSet, selfSet)

	def pp(self):
		fnName = self.fn.getName()

		# If we're not infix, print normally, otherwise check we're well formed and print infix
		if not self.infix:
			ppString = f"{fnName}({', '.join([elem.pp() for elem in self.actuals])})"
		elif isinstance(self.fn, SailFn):
			sys.exit("Error: can't pretty print SailFn as infix - must be SailHandwrittenFn")
		elif len(self.actuals) != 2:
			sys.exit("Error: can't pretty print infix function which doesn't have exactly 2 actuals")
		else:
			ppString = f"({self.actuals[0].pp()}) {fnName} ({self.actuals[1].pp()})"

		# Hack - provide a type annotation in this specific case.
		if self.fn.getName() == '==' and \
				isinstance(self.actuals[0], SailApp) and \
				utils.sanitiseSymbol(self.actuals[0].getFn().getName()) in ['vex3_byte1_get_m_mmmm']:

			return f"({ppString}) : {self.getType().generalise().pp()}"
		else:
			return ppString


class SailNumLit(SailASTelem):
	"""A number literal"""
	def __init__(self, num):
		"""
		Args:
			- num : int | float
		"""
		super().__init__()
		if type(num) not in [int, float]:
			print(f"Error: {num}")
			raise
		self.num = num

	### Custom methods ###
	def getNum(self):
		return self.num

	### Required methods ###
	def getType(self):
		return Sail_t_member([self.num])

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return str(self.num)


class SailBitsLit(SailASTelem):
	"""A bitvector literal"""
	def __init__(self, width, value, signed):
		"""
		Args:
			- width : int
			- value : int
		"""
		super().__init__()
		if type(width) != int or type(value) != int:
			print(f"Error: SailBitsLit({width}, {value})")
			raise
		self.width = width
		self.value = value
		self.signed = signed

	### Custom methods ###
	def getValue(self):
		return self.value

	### Required methods ###
	def getType(self):
		return Sail_t_bits(self.width, signed=self.signed)

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		# Convert to two's complement, if necessary
		value = self.value if self.value >= 0 else ((2 ** self.width) + self.value)
		if self.width % 4 == 0:
			width = int(self.width / 4)
			numUnderscores = int((width - 1) / 4)
			formatStr = f"0x{{:0{width + numUnderscores}_x}}"
		else:
			width = self.width
			numUnderscores = int((width - 1) / 4)
			formatStr = f"0b{{:0{width + numUnderscores}_b}}"
		return formatStr.format(value)


class SailBoolLit(SailASTelem):
	"""A boolean literal"""
	def __init__(self, b):
		"""
		Args:
			b: bool
		"""
		super().__init__()
		self.b = b

	### Custom methods ###
	def getBool(self):
		return self.b

	### Required methods ###
	def getType(self):
		return Sail_t_bool()

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return 'true' if self.b else 'false'


class SailStringLit(SailASTelem):
	"""A string literal"""
	def __init__(self, s):
		"""
		Args:
			s: str
		"""
		super().__init__()
		self.s = s

	def getString(self):
		return self.s

	### Required methods ###
	def getType(self):
		return Sail_t_string()

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return f'"{self.s}"'


class SailUnitLit(SailASTelem):
	"""Unit"""
	def __init__(self):
		super().__init__()

	### Required methods ###
	def getType(self):
		return Sail_t_unit()

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return "()"


class SailUnderScoreLit(SailASTelem):
	"""An underscore"""
	def __init__(self):
		super().__init__()

	### Required methods ###
	def getType(self):
		sys.exit("Can't get type of Sail underscore")

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return "_"


class SailIf(SailASTelem):
	"""An `if` statement"""
	def __init__(self, ifTerm, thenTerm, elseTerm):
		"""
		Args:
			- ifTerm: [SailAstElems]
			- thenTerm: [SailAstElems]
			- elseTerm: [SailAstElems]
		"""
		# Check length
		super().__init__()
		if len(ifTerm)   != 1: sys.exit(f"Error: ifTerm length should be 1: {ifTerm}")
		if len(thenTerm) != 1: sys.exit(f"Error: thenTerm length should be 1: {thenTerm}")
		if len(elseTerm) != 1: sys.exit(f"Error: elseTerm length should be 1: {elseTerm}")

		# Convert the if term to the correct type
		self.ifTerm = convertToBool(ifTerm)

		# Hack: If have string as `then` and nil as `else` then make the
		# string a Some(string) - for `create-dispatch-for-opcodes`
		if isinstance(thenTerm[0], SailStringLit) and isinstance(elseTerm[0], SailPlaceholderNil):
			thenTerm = [someHelper(thenTerm[0])]

		#### nil resolution #####
		resolveNils([thenTerm[0], elseTerm[0]])

		# Set the then and else terms
		self.thenTerm = thenTerm
		self.elseTerm = elseTerm

		self.type = None

		self.coerceBranches()

	def getCondition(self):
		return self.ifTerm[0]

	def getThenTerm(self):
		return self.thenTerm[0]

	def getElseTerm(self):
		return self.elseTerm[0]

	def getType(self):
		if self.type is None:
			# Extract types and check they're the same
			thenType = self.thenTerm[0].getType()
			elseType = self.elseTerm[0].getType()
			if thenType != elseType:
				merged = mergeTypes(thenType, elseType)
				if merged is None:
					print(f"Error: then type not compatible with else type.  then type: {ppType(thenType)}; else type: {ppType(elseType)}")
					print(f"then term: {self.thenTerm[0].pp()}")
					print(f"else term: {self.elseTerm[0].pp()}")
					sys.exit()
				self.type = merged
			else:
				self.type = thenType

		return self.type

	def setType(self, typ):
		self.type = typ
		self.coerceBranches()

	def coerceBranches(self):
		typ = self.getType()
		thenTerm = coerceExpr(self.thenTerm[0], typ)
		if thenTerm is None:
			print(f"Error: cannot coerce then-term {self.thenTerm[0].pp()} to {typ.pp()} (else-term {self.elseTerm[0].pp()}")
			sys.exit()
		self.thenTerm[0] = thenTerm
		elseTerm = coerceExpr(self.elseTerm[0], typ)
		if elseTerm is None:
			print(f"Error: cannot coerce else-term {self.elseTerm[0].pp()} to {typ.pp()} (then-term {self.thenTerm[0].pp()}")
			sys.exit()
		self.elseTerm[0] = elseTerm

	def getEffects(self, ctx):
		# Extract effects from each term and combine
		ifEffects = self.ifTerm[0].getEffects(ctx).copy()
		thenEffects = self.thenTerm[0].getEffects(ctx).copy()
		elseEffects = self.elseTerm[0].getEffects(ctx).copy()

		return ifEffects.union(thenEffects, elseEffects)

	def getChildrenByPred(self, p):
		ifSet = self.ifTerm[0].getChildrenByPred(p)
		thenSet = self.thenTerm[0].getChildrenByPred(p)
		elseSet = self.elseTerm[0].getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(ifSet, thenSet, elseSet, selfSet)

	def resolveTypes(self, file, justPrint = False):
		# If the condition is a variable with an unknown type, assume it is a Boolean
		if isinstance(self.ifTerm[0], SailBoundVar) and isUnknownType(self.ifTerm[0].getType()):
			self.ifTerm[0].setType(Sail_t_bool())

	def pp(self):

		ifPP = self.ifTerm[0].pp()
		thenPP = self.thenTerm[0].pp()
		elsePP = self.elseTerm[0].pp()
		return f"if {ifPP}\nthen {thenPP}\nelse {elsePP}"


class SailInclude(SailASTelem):
	"""Represents an `$include` expression"""
	def __init__(self, sailAST, path, file, includeHeaders, env):
		"""
		Args:
			- sailAST : [SailASTelem]
			- path : str
			- file : str - should not include an extenstion
			- includeheaders : bool
			- env : env
		"""
		super().__init__()
		self.sailAST = sailAST
		self.path = path
		self.file = utils.sanitiseSymbol(file)
		self.includeHeaders = includeHeaders
		self.savedOut = False
		self.env = env

	### Required methods ###
	def getType(self):
		sys.exit(f"Error: should not try to get type of `SailInclude`")

	def getChildrenByPred(self, p):
		# We probably still want to calculate sailASTset even if self.saveOut is true as we may be analysing
		# a file which hasn't had this included already.
		sailASTset = utils.unionListOfSets(i.getChildrenByPred(p) for i in self.sailAST if not isinstance(i, ACL2Comment))

		selfSet = super().getChildrenByPred(p)

		return set.union(sailASTset, selfSet)

	def pp(self):
		if not self.savedOut:
			# Save out the file
			self.savedOut = True
			saveSail(self.sailAST, self.path, self.file, self.env, self.includeHeaders)

			return f'$include "{self.file}.sail"'
		else:
			return ""


class SailTuple(SailASTelem):
	"""Represents a SailTuple"""
	def __init__(self, subItems):
		"""
		Args:
			- subItems : [SailASTelem]
		"""
		super().__init__()
		self.subItems = subItems

		self.resolveNilCB = None

	### Custom methods ###
	def getItems(self):
		return self.subItems

	### Required methods ###
	def getType(self):
		return Sail_t_tuple([item.getType() for item in self.subItems])

	def getEffects(self, ctx):
		effects = [e.getEffects(ctx) for e in self.subItems]
		if len(effects) == 0:
			return set([])
		else:
			return set.union(*effects)

	def getChildrenByPred(self, p):
		itemsSet = utils.unionListOfSets(i.getChildrenByPred(p) for i in self.subItems)

		selfSet = super().getChildrenByPred(p)

		return set.union(itemsSet, selfSet)

	def pp(self, withAnnotations=False):
		itemStrings = []
		for e in self.subItems:
			if withAnnotations and isPrintableType(e.getType()):
				pp = f"{e.pp()} : {e.getType().pp()}"
			else:
				pp = f"{e.pp()}"
			itemStrings.append(pp)
		return f"({', '.join([i for i in itemStrings])})"


class SailMatch(SailASTelem):
	"""Represents a Sail match expression"""
	def __init__(self, var, matches, forceType=None):
		"""
		Args:
			- var : SailBoundVar
			- matches : [(pattern : SailNumLit?|SailUnderscoreLit, expr : SailASTelem)]
			- forceType : None | SailType
		"""
		super().__init__()
		self.var = var
		self.matches = matches
		self.type = forceType

		# Convert the patterns to bool as required
		if isinstance(self.var.getType(), Sail_t_option):
			self.matches = [(someHelper(p), e) if not isinstance(p, SailUnderScoreLit) else (p,e,) for (p, e) in self.matches]

		self.coerceMatches()

		# Resolve nils for the expressions
		exprs = [e for (_, e) in self.matches]
		resolveNils(exprs)

	### Custom methods ###
	def getMatches(self):
		return self.matches

	def setType(self, typ):
		self.type = typ
		self.coerceMatches()

	### Required methods ###
	def getType(self):
		if self.type is None:
			# Get the type of each expression and hope they're the same
			typesWork, expr_t = checkTypesMatch([expr for (_, expr) in self.matches])

			if not typesWork:
				print(f"Error: types of expression in match statement not the same")
				print(self.var)
				for (p, e) in self.matches:
					print(f"\t{p.pp()}, {e.getType().pp()}")
				for (p, e) in self.matches:
					print(f"\t{p.pp()}, {e.pp()}")
				sys.exit()

			self.type = expr_t

		return self.type

	def coerceMatches(self):
		typ = self.getType()
		for i in range(len(self.matches)):
			(p, e) = self.matches[i]
			ce = coerceExpr(e, typ)
			if ce is None:
				print(f"Error: cannot coerce ({e.pp()} : {e.getType().pp()}) to type {typ.pp()}")
				sys.exit()
			self.matches[i] = (p, ce)

	def getEffects(self, ctx):
		effects = [e.getEffects(ctx) for (_, e) in self.matches]
		if len(effects) == 0:
			return set([])
		else:
			return set.union(*effects)

	def getChildrenByPred(self, p):
		varSet = self.var.getChildrenByPred(p)
		matchesSet = utils.unionListOfSets(set.union(pat.getChildrenByPred(p), e.getChildrenByPred(p)) for (pat,e) in self.matches)

		selfSet = super().getChildrenByPred(p)

		return set.union(varSet, matchesSet, selfSet)

	def pp(self, withAnnotation=True):
		# Partial constant folding.  If the variable is actually a string,
		# then we can just use the appropriate expression directly.  This is
		# common when flgi, !flgi and !flgi-undefined have been macro expanded.
		if isinstance(self.var, SailStringLit):
			matches = [e for (p, e) in self.matches if isinstance(p, SailStringLit) and p.getString() == self.var.getString()]
			if len(matches) != 1: sys.exit("Incorrect number of possible expressions when constant folding a match")
			return matches[0].pp()
		
		# If constant folding is not applicable, continue as normal
		headerLine = f"match {self.var.pp()} {{"
		matchLines = []
		for (pat, expr) in self.matches:
			expr_pp = expr.pp()
			if isMultilineExpr(expr):
				# Wrap (potential) multi-line expressions in a block
				expr_pp = "{ " + expr_pp + " }"
			matchLines.append(f"{pat.pp()} => {expr_pp}")
		matchLines = ",\n".join(matchLines)
		together = f"{headerLine}\n{matchLines}\n}}"

		# Add a type annotation
		annotated = f"({together}) : {ppType(self.getType())}"

		return annotated if withAnnotation else together


class SailStruct(SailASTelem):
	"""Represents the definition of Sail struct"""
	def __init__(self, name, fields, defaults=None):
		"""

		Args:
			name: str
			fields: {(name:str) : SailType}
			defaults: [(name:str, SailASTelem)] - compatible with SailStructLit below

		TODO: replace `name` and `fields` with a single list of boundvars
		"""
		super().__init__()
		self.name = utils.sanitiseSymbol(name)
		self.fields = fields
		self.defaults = defaults

	### Custom methods ###
	def getName(self):
		return self.name

	def getFields(self):
		return self.fields

	def getTypeOfName(self, name):
		return self.fields[name]

	def getDefaults(self):
		return self.defaults

	### Required methods ###
	def getType(self):
		print("Should not try to get type of SailStruct")
		raise

	def getEffects(self, ctx):
		sys.exit("Should not try to get effect of SailStruct")

	def getChildrenByPred(self, p):
		exprsSet = utils.unionListOfSets(e.getChildrenByPred(p) for (_, e) in self.defaults)

		selfSet = super().getChildrenByPred(p)

		return set.union(exprsSet, selfSet)

	def pp(self):
		names_types = []
		for (n, t) in self.fields.items():
			names_types.append(f"{utils.sanitiseSymbol(n)} : {t.pp()}")
		names_types_str = ",\n\t".join(names_types)
		structDefInner = f"{{\n\t{names_types_str}\n}}"
		structDef = f"struct {self.name} = {structDefInner}"

		return structDef


class SailStructLit(SailASTelem):
	"""Represents a literal struct"""
	def __init__(self, struct, exprs):
		"""
		Args:
			struct: SailStruct
			exprs: [(name:str, SailASTelem)]
		"""
		# Check the types of the expressions match the expected types
		super().__init__()
		for (n, e) in exprs:
			if e.getType().generalise() != struct.getTypeOfName(n).generalise():
				print(f"Field name: {n}\nGiven expression: {e.pp()}")
				print(f"Given expression type: {e.getType().pp()}\nExpected expression type: {struct.getTypeOfName(n).pp()}")
				sys.exit("Error: subfield type in a struct literal does not match definition type")

		# Set object fields
		self.struct = struct
		self.exprs = exprs

	### Required methods ###
	def getType(self):
		return Sail_t_struct(self.struct)

	def getEffects(self, ctx):
		effects = [e.getEffects(ctx) for (_, e) in self.exprs]
		if len(effects) == 0:
			return set([])
		else:
			return set.union(*effects)

	def getChildrenByPred(self, p):
		# Ignore struct in the expectation that it has been included when the struct was defined
		# Do include the actuals though.  Sure, they may have been been included as the defaults, but they also
		# may not have been.
		exprSet = utils.unionListOfSets(e.getChildrenByPred(p) for (_, e) in self.exprs)

		selfSet = super().getChildrenByPred(p)

		return set.union(exprSet, selfSet)

	def pp(self):
		names_exprs = []
		for (n, e) in self.exprs:
			names_exprs.append(f"{utils.sanitiseSymbol(n)} = {e.pp()}")

		names_exprs = ", ".join(names_exprs)
		structLitInner = f"{names_exprs}"
		structLit = f"struct {{{structLitInner}}}"

		return structLit


class SailStructProject(SailASTelem):
	"""Extract a field from a struct"""
	def __init__(self, sailASTelem, fieldName: str):
		"""
		Args:
			sailASTelem: sailASTelem - must have type Sail_t_struct
			fieldName: str
		"""
		super().__init__()
		if type(sailASTelem.getType()) not in [Sail_t_struct, Sail_t_bitfield]:
			sys.exit("Error: type of Sail AST element to project from is not a struct")

		self.sailASTelem = sailASTelem
		self.fieldName = fieldName


	### Required methods ###
	def getType(self):
		if isinstance(self.sailASTelem.getType(), Sail_t_bitfield) and self.fieldName == 'bits':
			return Sail_t_bits(self.sailASTelem.getType().getLength())
		elif isinstance(self.sailASTelem.getType(), Sail_t_struct):
			return self.sailASTelem.getType().getStruct().getTypeOfName(self.fieldName)
		else:
			raise ValueError()

	def getEffects(self, ctx):
		return set([])

	def getChildrenByPred(self, p):
		elemsSet = self.sailASTelem.getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(elemsSet, selfSet)

	def pp(self):
		return f"({self.sailASTelem.pp()}).{utils.sanitiseSymbol(self.fieldName)}"


class SailVectorLit(SailASTelem):
	"""A Sail vector literal"""
	def __init__(self, items):
		"""
		Args:
			items: [SailASTelem]
		"""
		super().__init__()
		self.items = items

	### Required methods ###
	def getType(self):
		# Check subitems have the same type here
		typesWork, expr_t = checkTypesMatch(self.items)
		if not expr_t:
			print("Error: types of SailVectorLit not compatible")
			print(f"{self.items}")
			sys.exit()

		return Sail_t_vector(len(self.items), expr_t)

	def getEffects(self, ctx):
		effects = [e.getEffects(ctx) for e in self.items]
		if len(effects) == 0:
			return set([])
		else:
			return set.union(*effects)

	def getChildrenByPred(self, p):
		itemsSet = utils.unionListOfSets(i.getChildrenByPred(p) for i in self.items)

		selfSet = super().getChildrenByPred(p)

		return set.union(itemsSet, selfSet)

	def pp(self):
		# We use `reversed` here because our default order is `dec`
		return f"[{', '.join(item.pp() for item in reversed(self.items))}] : {self.getType().pp()}"


class SailVectorProject(SailASTelem):
	"""A Sail vector projection"""

	def __init__(self, vector, index, coerceIndex=True):
		"""
		Args:
			vector: SailASTelem : Sail_t_vector
			index: SailASTelem : Sail_t_int
		"""
		super().__init__()
		indexType = Sail_t_range(0, vector.getType().getLength() - 1)
		self.vector = vector
		self.index = coerceExpr(index, indexType) if coerceIndex else index

	### Required methods ###
	def getType(self):
		return self.vector.getType().getSubType()

	def getEffects(self, ctx):
		vectorEffects = self.vector.getEffects(ctx)
		indexEffects = self.index.getEffects(ctx)

		return set.union(vectorEffects, indexEffects, {'escape'})

	def getChildrenByPred(self, p):
		vectorSet = self.vector.getChildrenByPred(p)
		indexSet = self.index.getChildrenByPred(p)

		selfSet = super().getChildrenByPred(p)

		return set.union(vectorSet, indexSet, selfSet)

	def pp(self):
		# Add paranthesis, unless the vector is just a variable
		vector_pp = '(' + self.vector.pp() + ')' if not isinstance(self.vector, SailBoundVar) else self.vector.pp()
		return f"{vector_pp}[{self.index.pp()}]"

	### Nil resolution ###
	def setCallBacks(self):
		sys.exit("Finish implementing nil resolution functions for SailVectorProject!")


class SailBitsSubrange(SailASTelem):
	"""A Sail bitvector subrange"""

	def __init__(self, bits, high, low, coerceIndices=True):
		"""
		Args:
			bits: SailASTelem : Sail_t_bits
			high, low: int
		"""
		super().__init__()
		# nBits = bits.getType().getLength()
		# indexType = Sail_t_range(0, nBits - 1) if nBits is not None else None
		self.bits = bits
		self.high = high # coerceExpr(high, indexType) if coerceIndex and indexType is not None else high
		self.low = low # coerceExpr(low, indexType) if coerceIndex and indexType is not None else low

	### Required methods ###
	def getType(self):
		return Sail_t_bits(self.high - self.low + 1)

	def getEffects(self, ctx):
		return self.bits.getEffects(ctx)

	def getChildrenByPred(self, p):
		return self.bits.getChildrenByPred(p)

	def pp(self):
		# Add paranthesis, unless the vector is just a variable
		vector_pp = '(' + self.bits.pp() + ')' if not isinstance(self.bits, SailBoundVar) else self.bits.pp()
		return f"{vector_pp}[{self.high} .. {self.low}]"


class SailListLit(SailASTelem):
	"""A Sail list literal"""
	def __init__(self, members):
		"""
		Args:
			members: [SailASTelem]
		"""
		super().__init__()
		check, _ = checkTypesMatch(members)
		if not check:
			sys.exit(f"Error: types of list members don't match in init() - {members}")

		self.members = members

	### Required methods ###
	def getType(self):
		check, subtype = checkTypesMatch(self.members)
		if not check:
			sys.exit(f"Error: types of list members don't match in getType() - {self.members}")

		return Sail_t_list(subtype)

	def getEffects(self, ctx):
		return utils.unionListOfSets([m.getEffects(ctx) for m in self.members])

	def getChildrenByPred(self, p):
		memberSet = utils.unionListOfSets([m.getChildrenByPred(p) for m in self.members])

		selfSet = super().getChildrenByPred(p)

		return set.union(memberSet, selfSet)

	def pp(self):
		return f"[|{', '.join([m.pp() for m in self.members])}|]"


class SailBitfield(SailASTelem):
	"""A Sail bitfield definition"""

	def __init__(self, typeName, length, fields):
		"""
		Args:
			typeName: string
			nBits: int
			fields: list[tuple[string, int, int]] - list of fields with name and high and low indices
		"""
		super().__init__()
		self.name = typeName
		self.length = length
		self.fields = fields

	### Required methods ###
	def getType(self):
		return Sail_t_bitfield(self.name, self.length, fields=self.fields)

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		decl = f"bitfield {utils.sanitiseSymbol(self.name)} : bits({self.length})"
		fields_pp = ',\n'.join([f"  {utils.sanitiseSymbol(f[0])} : {f[1]} .. {f[2]}" for f in self.fields])
		return decl + " = {\n" + fields_pp + "\n}"


class SailBitfieldAccess(SailASTelem):
	"""A Sail bitfield definition"""

	def __init__(self, exp, field):
		"""
		Args:
			exp: SailASTelem - a bitfield expression
			field: string - the field to be accessed
		"""
		super().__init__()
		if isinstance(exp.getType(), Sail_t_bitfield):
			self.exp = exp
			self.field = field
			self.length = exp.getType().getFieldType(field).getLength()
		else:
			raise ValueError()

	### Required methods ###
	def getType(self):
		return Sail_t_bits(self.length)

	def getEffects(self, ctx):
		return self.exp.getEffects(ctx)

	def getChildrenByPred(self, p):
		expSet = self.exp.getChildrenByPred(p)
		selfSet = super().getChildrenByPred(p)
		return set.union(expSet, selfSet)

	def pp(self):
		return f"({self.exp.pp()})[{utils.sanitiseSymbol(self.field)}]"


class SailBitfieldUpdate(SailASTelem):
	"""A Sail bitfield definition"""

	def __init__(self, exp, field, value):
		"""
		Args:
			exp: SailASTelem - a bitfield expression
			field: string - the field to be accessed
		"""
		super().__init__()
		if isinstance(exp.getType(), Sail_t_bitfield):
			self.exp = exp
			self.field = field
			self.value = value
		else:
			raise ValueError()

	### Required methods ###
	def getType(self):
		return self.exp.getType()

	def getEffects(self, ctx):
		return set.union(self.exp.getEffects(ctx), self.value.getEffects(ctx))

	def getChildrenByPred(self, p):
		expSet = self.exp.getChildrenByPred(p)
		valueSet = self.value.getChildrenByPred(p)
		selfSet = super().getChildrenByPred(p)
		return set.union(expSet, valueSet, selfSet)

	def pp(self):
		return f"[({self.exp.pp()}) with {utils.sanitiseSymbol(self.field)} = ({self.value.pp()})]"


################################################################################
# Helper and utility functions
################################################################################


def getType(sail):
	return sail.getType() if isinstance(sail, SailASTelem) else Sail_t_unknown()

def createStructWithDefault(name, fields):
	"""
	Given a desired name and map of field name to default value, creates a
	Sail struct.

	Args:
		name: str
		fields: {str : SailASTelem}

	Returns:
		SailStruct
	"""
	# Map names to types
	names_types = {}
	for n in fields:
		names_types[n] = fields[n].getType().generalise()

	# Create the struct
	struct = SailStruct(name, names_types, list(fields.items()))

	# Return
	return struct

def someHelper(item):
	"""
	Given a Sail item, wraps it in a Some().

	Args:
		item: SailASTelem

	Returns:
		SailHandwrittenFn implementing Some()
	"""
	return SailApp(
		fn = SailHandwrittenFn(
			name = 'Some',
			typ = Sail_t_fn([item.getType()], Sail_t_option(item.getType()))),
		actuals = [item])

def isSome(item):
	return isinstance(item, SailApp) and isinstance(item.getFn(), SailHandwrittenFn)\
			and item.getFn().getName() == 'Some' and isinstance(item.getFn().getType().getRHS(), Sail_t_option)

def noneHelper(typ):
	"""
	Given a Sail type, creates a None() of that type.

	Args:
		typ: SailType

	Returns:
		SailHandwrittenFn implementing None()
	"""
	return SailApp(
		fn = SailHandwrittenFn(
			name = 'None',
			typ = Sail_t_fn([], Sail_t_option(typ))),
		actuals = [])

def isNone(item):
	return isinstance(item, SailApp) and isinstance(item.getFn(), SailHandwrittenFn)\
			and item.getFn().getName() == 'None' and isinstance(item.getFn().getType().getRHS(), Sail_t_option)

def isMultilineExpr(expr):
	return isinstance(expr, SailLet) or \
			isinstance(expr, SailIf) or \
			(isinstance(expr, SailBlock) and len(expr.getExprs()) > 1) or \
			(isinstance(expr, SailMatch) and not isinstance(expr.var, SailStringLit))

def checkTypesMatch(items):
	"""
	Given a list of Sail AST elements, returns either (False, None) indicating
	their types don't match or (True, SailType) with the generalised SailType
	if they do.

	Args:
		items: [SailASTelem]

	Returns:
		(bool, SailType)
	"""
	expr_t = items[0].getType()
	for expr in items[1:]:
		if isinstance(expr, SailPlaceholderNil) and (isinstance(expr_t, Sail_t_bool) or isinstance(expr_t, Sail_t_option)):
			continue
		new_expr_t = mergeTypes(expr_t, expr.getType())
		if new_expr_t is None:
			if coerceExpr(expr, expr_t) is None:
				return False, None
			else:
				continue
		else:
			expr_t = new_expr_t

	return True, expr_t

def convertToBool(sailAST):
	"""
	Conditional expression don't always use a bool as their predicate.  For
	example, they sometimes use an option type, in which case it must we
	wrapped in an is_some().  This function converts items to bools if it is
	possible for the given item.

	Args:
		sailAST: [SailAST]

	Returns:
		[SailAST]
	"""
	# Get the type of the term
	try:
		ifType = sailAST[0].getType()
	except ValueError:
		# If we can't get the type, assume it's a bool
		ifType = Sail_t_bool()

	# Modify the term term based on its type
	if isinstance(ifType, (Sail_t_bool, Sail_t_unknown)):
		toReturn = sailAST
	elif isinstance(ifType, Sail_t_option):
		toReturn = [SailApp(
			fn=SailHandwrittenFn(
				name='is_some',
				typ=Sail_t_fn([], Sail_t_bool())  # Sort of
			),
			actuals=sailAST
		)]
	else:
		sys.exit(f"Error: don't know how to handle term of type {ifType} in `convertToBool`")

	return toReturn


def resolveNils(exprs):
	"""
	If any of the exprs is a nil, attempts to resolve it using the other
	exprs.  If any of the exprs is a tuple, attempts to resolve nils within it
	using the other exprs.

	Args:
		exprs: [SailASTelem]

	Returns:
		None

	Side effects:
		SailPlaceholderNil objects are resolved.
	"""

	def resolveNilsInner(otherTypes, theNil):
		"""
		Resolve `theNil` based on the types found in `otherTypes`.

		`otherTypes` is a list of types representing the other branches.
		theNil is the `nil` or `throw` whose type we are setting.  There could
		be more nils/throws in the other branches, so we must first filter them
		out of otherTypes.

		Args:
			otherTypes: [SailType]
			theNil: SailPlaceholderNil | SaillApp (with a 'throw')

		Returns:
			None

		Side effects:
			`theNil` is resolved
		"""
		# Filter out nils from the other types
		otherTypes = [typ for typ in otherTypes if not (isinstance(typ, Sail_t_PlaceholderNil) or isinstance(typ, Sail_t_error))]

		# Check all the other types are the same
		if any([t != otherTypes[0] for t in otherTypes]):
			print("Warning: types don't match")
			return

		# If we've removed averything in otherTypes, can't do much!
		if not otherTypes:
			return

		# Resolve
		theNil.resolve(otherTypes[0])

	#### Onto the actual body of the function
	# Iterate through the expressions we're given
	for (i, e) in enumerate(exprs):
		# Get a list of all expressions but this one
		otherExprs = exprs[:i] + exprs[i + 1:]

		# If the expression is a nil or a `throw`, resolve directly against the other expressions' types
		if isinstance(e, SailPlaceholderNil) or (isinstance(e, SailApp) and e.getFn().getName().lower() == 'throw'):
			# Get the types of the other terms and filter out nils
			otherTypes = [oe.getType() for oe in otherExprs]
			resolveNilsInner(otherTypes, e)

		# If the expression is a tuple, go through it looking for nils to resolve and resolve against the types of the
		# expressions at the correct index in their tuple if types
		elif isinstance(e, SailTuple):
			for (j, sub_e) in enumerate(e.getItems()):
				if isinstance(sub_e, SailPlaceholderNil):
					otherTypes = [oe.getType().getSubTypes()[j] for oe in otherExprs if isinstance(oe.getType(), Sail_t_tuple)]
					resolveNilsInner(otherTypes, sub_e)

		# Otherwise we don't know what to do so don't do anything
		else:
			pass

def saveSail(SailAST, path, name, env, includeHeaders):
	"""
	Given a translated Sail AST, saves it to file.

	Args:
		- SailAST : [SailASTelems]
		- path : str - path to output file
		- name : str - name of file to save, without extension
		- includeHeaders : bool - whether to include various `$include`s
	"""
	print(f"Pretty printing and saving to path: {path}; name: {name}")
	# Get the pretty printed version
	pp = "\n".join([elem.pp() for elem in SailAST])

	# Also output into a file
	with open(os.path.join(path, f"{utils.sanitiseSymbol(name)}.sail"), 'w') as f:
		if includeHeaders:
			f.write("$ifndef _DEFAULT_DEC\n")
			f.write("\tdefault Order dec\n")
			f.write("$endif\n\n")

		f.write("$include <prelude.sail>\n")
		f.write("$include <string.sail>\n") # TODO: only include this when we need

		if name in env.auxiliaryInclude:
			f.write('$include "auxiliary.sail"\n')

		if includeHeaders:
			f.write('$include "handwritten.sail"\n')

		f.write(pp)

		# A trailing new line is needed for, for example, file only containing `$include`s.
		f.write("\n")

		print(f"Successfully saved file path: {path}; name: {name}")

def coerceExpr(expr, typ, exact=True):
	try:
		etyp = expr.getType()
	except:
		print(f"coerceExpr: Cannot get type of {expr.pp()}")
		return None
	if etyp is None or isinstance(etyp, Sail_t_unknown):
		print(f"coerceExpr: Cannot coerce expression {expr.pp()} with unknown type")
		return None
	elif isSubType(etyp, typ):
		return expr
	elif isNumeric(etyp) and not isNonnegativeType(etyp) and isinstance(typ, Sail_t_nat):
		fntyp = Sail_t_fn([Sail_t_int()], typ)
		return SailApp(SailHandwrittenFn("nat_of_int", fntyp), [expr])
	elif isNumeric(etyp) and isRangeType(typ):
		(low, high) = getRangeOfType(typ)
		# Preserve existing constraints on the expression type, e.g. if
		# we have a set-constrained integer
		retTyp = intersectTypes(etyp, typ)
		fntyp = Sail_t_fn([Sail_t_int(), Sail_t_int(), etyp], retTyp)
		args = [SailNumLit(low), SailNumLit(high), expr]
		return SailApp(SailHandwrittenFn("check_range", fntyp), args)
	elif isinstance(expr, SailNumLit) and isinstance(typ, Sail_t_bits) and typ.getLength() is not None:
		return SailBitsLit(typ.getLength(), expr.getNum(), typ.signed)
	elif isinstance(expr, SailBitsLit) and isinstance(typ, Sail_t_bits) and typ.getLength() is not None:
		return SailBitsLit(typ.getLength(), expr.getValue(), typ.signed)
	elif isNumeric(etyp) and isinstance(typ, Sail_t_bits) and typ.length is not None:
		fntyp = Sail_t_fn([Sail_t_int(), Sail_t_int()], typ)
		lengthArg = typ.length if isinstance(typ.length, SailASTelem) else SailNumLit(typ.getLength())
		args = [expr, lengthArg]
		return SailApp(SailHandwrittenFn("bits_of_int", fntyp), args)
	elif isinstance(etyp, Sail_t_bits) and isNumeric(typ):
		if etyp.getLength() is None:
			castTyp = Sail_t_nat()
		else:
			if etyp.signed:
				castTyp = Sail_t_range(-(2 ** (etyp.getLength() - 1)), (2 ** (etyp.getLength() - 1)) - 1)
			else:
				castTyp = Sail_t_range(0, (2 ** etyp.getLength()) - 1)
		fn = 'signed' if etyp.signed else 'unsigned'
		innerTyp = typ if isSubType(castTyp, typ) else castTyp
		innerExpr = SailApp(SailHandwrittenFn(fn, Sail_t_fn([etyp], innerTyp)), [expr])
		return coerceExpr(innerExpr, typ, exact)
	elif isinstance(etyp, Sail_t_bits) and isinstance(typ, Sail_t_bits) and typ.length is not None:
		if etyp.getLength() is not None and typ.getLength() is not None:
			if typ.getLength() < etyp.getLength():
				fn = 'truncate'
			elif typ.getLength() == etyp.getLength() and isinstance(expr, SailBoundVar):
				# If we have a variable with the right
				# bitvector length, we don't need a cast, but
				# return a fresh copy with the new type
				# (including signedness)
				return SailBoundVar(expr.getName(), typ=typ)
			elif typ.getLength() == etyp.getLength() and isinstance(expr, SailApp):
				# Similarly for function calls returning a
				# bitvector of the right length;  no cast
				# needed, but remember signedness
				return SailApp(expr.getFn(), expr.getActuals(), infix=expr.isInfix(), retType=typ)
			else:
				# Zero-extend unsigned bitvectors, sign-extend signed bitvectors
				# What about coercing signed to unsigned
				# bitvectors?  It looks like those coercions
				# occuring in the ACL2 source do
				# sign-extension, usually with explicit
				# comments to that effect (e.g. the
				# sign-extension of `imm` in
				# `x86-add/adc/sub/sbb/or/and/xor/cmp-test-rAX-I`),
				# so we sign-extend in that case.
				fn = 'sail_sign_extend' if etyp.signed else 'sail_zero_extend'
			fntyp = Sail_t_fn([etyp, Sail_t_int()], typ)
			args = [expr, SailNumLit(typ.getLength())]
		else:
			fn = 'sail_mask_signed' if etyp.signed else 'sail_mask'
			fntyp = Sail_t_fn([Sail_t_int(), etyp], typ)
			lengthArg = typ.length if isinstance(typ.length, SailASTelem) else SailNumLit(typ.getLength())
			args = [lengthArg, expr]
		if not(typ.signed) and etyp.signed and fn != 'truncate':
			print(f"coerceExpr: Sign-extending {expr.pp()} to unsigned {typ.pp()}")
		return SailApp(SailHandwrittenFn(fn, fntyp), args)
	elif isinstance(etyp, Sail_t_bitfield): # and isinstance(typ, Sail_t_bits):
		merged = mergeTypes(etyp, typ)
		retTyp = typ if exact or merged is None else merged
		if retTyp == etyp:
			return expr
		else:
			return coerceExpr(SailStructProject(expr, "bits"), retTyp, exact)
	elif isinstance(typ, Sail_t_bitfield):
		expr_bits = coerceExpr(expr, Sail_t_bits(typ.getLength()))
		if expr_bits is None:
			print(f"coerceExpr: Cannot coerce {expr.pp()} to bitfield type {typ.getName()}")
			return None
		else:
			fname = f"Mk_{utils.sanitiseSymbol(typ.getName())}"
			ftyp = Sail_t_fn([Sail_t_bits(typ.getLength())], typ)
			return SailApp(SailHandwrittenFn(fname, ftyp), [expr_bits])
	elif isinstance(expr, SailTuple) and isinstance(typ, Sail_t_tuple):
		elems = []
		for i in range(len(expr.getItems())):
			elem = coerceExpr(expr.getItems()[i], typ.getSubTypes()[i], exact)
			if elem is None:
				return None
			elems = elems + [elem]
		return SailTuple(elems)
	elif isinstance(etyp, Sail_t_tuple) and isinstance(typ, Sail_t_tuple):
		elems = SailTuple([SailBoundVar("elem" + str(i), typ=etyp.getSubTypes()[i]) for i in range(len(etyp.getSubTypes()))])
		body = coerceExpr(elems, typ, exact)
		return SailLet(elems, [expr], [body]) if body is not None else None
	elif isinstance(expr, SailPlaceholderNil) and isinstance(typ, Sail_t_bool):
		return SailBoolLit(False)
	elif isinstance(etyp, Sail_t_option) and isinstance(typ, Sail_t_bool):
		fntyp = Sail_t_fn([etyp], typ)
		return SailApp(fn=SailHandwrittenFn(name='is_some', typ=fntyp), actuals=[expr])
	elif isSome(expr) and isinstance(typ, Sail_t_option):
		item = coerceExpr(expr.getActuals()[0], typ.getTyp(), exact)
		return someHelper(item) if item is not None else None
	elif isNone(expr) and isinstance(typ, Sail_t_option):
		return noneHelper(typ.getTyp())
	elif isinstance(expr, SailIf) or isinstance(expr, SailMatch):
		expr.setType(typ)
		return expr
	elif isinstance(expr, SailApp) and expr.getFn().getName().lower() == 'throw':
		return expr
	else:
		print(f"coerceExpr: Cannot coerce {expr.pp()} to {typ.pp()}")
		return None

def coerceExprs(exprs, typs):
	coercedExprs = []
	for (i, e) in enumerate(exprs):
		new = coerceExpr(e, typs[i])
		if new is None:
			print(f"Error: cannot coerce ({e.pp()} : {e.getType().pp()}) to type {typs[i].pp()}")
			sys.exit()
		coercedExprs.append(new)
	return coercedExprs
