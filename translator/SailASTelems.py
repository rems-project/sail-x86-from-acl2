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
		if len(self.getEffects([])) != 0:
			effectsInner = ", ".join(self.getEffects([]))
			effectsString = f" effect {{{effectsInner}}}"
		else:
			effectsString = ""

		# Combine
		typeSig = f"val {sanitisedName} : {self.getType().generalise().pp()}{effectsString}"
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
	def __init__(self, name, typ=None):
		"""
		Args:
			- name : str
			- typ : Sail_t_fn
		"""
		super().__init__()
		self.name = utils.sanitiseSymbol(name, lower=False)
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
		return self.typ.generalise()

	def getEffects(self, ctx):
		return self.typ.getEffects()

	def pp(self):
		sys.exit("Error: handwritten sail functions are already defined and so their pp() function should not be called")


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

		# Hack: force type of `check-alignment?` and `inst-ac?` to false - nil
		# resolution.
		if isinstance(varName, SailBoundVar) and varName.getName().lower() in ['check-alignment?', 'inst-ac?'] and isinstance(self.expr[0], SailPlaceholderNil):
			self.varName.setType(Sail_t_bool())
			self.expr = [SailBoolLit(False)]

		self.resolveNilCB = None

	### Custom Methods ###
	def getVarName(self):
		return self.varName

	def getExpr(self):
		return self.expr

	def resolveTypes(self, file, justPrint=False):
		file.write(f"Let Resolve Types - {self.varName.pp()}\n")
		file.write(f"Binding: {self.varName.getType().pp()}\n")
		file.write(f"Expression: {self.expr[0].getType().pp()}\n\n")

		sys.exit("Implement resolving of types of SailLet")

	### Required methods ###
	def getType(self):
		# Filter and check length
		bodyFiltered = manualCode.filterAST(self.body, comments=True)
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
		# Force a type annotation of the expression to avoid type variable
		# escape errors.
		pp_var = self.varName.pp()
		pp_expr = f"({self.expr[0].pp()}) : {self.expr[0].getType().generalise().pp()}"
		pp_body = self.body[0].pp()
		
		return f"let {pp_var} = {pp_expr} in\n{pp_body}"


class SailPlaceholderNil(SailASTelem):
	"""Represents a `nil` in the ACL2 code which hasn't yet been resolved in the Sail code"""
	# Class variables
	DEFAULT = 1
	BOOL = 2
	nilTypes = (DEFAULT, BOOL)

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
			print(f"Warning: tried setting SailPlaceholderNil to invalid type - {resolvedType}, {resolvedType.pp()} - will keep as default")

	### Required methods ###
	def getType(self):
		if self.nilType == SailPlaceholderNil.DEFAULT:
			return Sail_t_option(Sail_t_string())
		elif self.nilType == SailPlaceholderNil.BOOL:
			return Sail_t_bool()
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
		else:
			sys.exit("Error: Tried to pp unresolved nil")


class SailBoundVar(SailASTelem):
	"""Represents a bound variable (e.g. from formal parameters of a function
	of from b*/let bindings"""
	def __init__(self, binding, typ=None):
		"""
		Args:
			- binding : str
			- typ : SailType
		"""
		super().__init__()
		self.binding = binding
		self.setType(typ)

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
				self.typ = Sail_t_int()
			# else the type is being set explicitly, which we allow.

	### Required methods ###
	def getType(self):
		if self.typ is not None:
			return self.typ
		else:
			raise ValueError(f"Error: type of Sail `boundVar` is None - {self.binding}")

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		# TODO: remove printing the type for as many cases as possible
		if utils.sanitiseSymbol(self.binding) in ['les_lds_distinguishing_byte', 'max_offset']:
			return f"{utils.sanitiseSymbol(self.binding, includeFnNames=True)} : {self.getType().pp()}"
		else:
			return utils.sanitiseSymbol(self.binding, includeFnNames=True)


class SailApp(SailASTelem):
	"""Represents function application in sail"""
	def __init__(self, fn, actuals, infix=False):
		"""
		Args:
			- fn : SailFn | SailHandwrittenFn
			- actuals : [SailAstElems]
			- infix : Bool
		"""
		super().__init__()
		self.fn = fn
		self.actuals = actuals
		self.infix = infix

	### Custom methods ###
	def getFn(self):
		return self.fn

	def getActuals(self):
		return self.actuals

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
			# Go through terms resolving types if necessary
			for i in range(len(self.actuals)):
				fType = formalTypes[i]
				aType = actualTypes[i]
				# If neither are unknown, we need not do anything so continue:
				if not (isinstance(fType, Sail_t_unknown) or isinstance(aType, Sail_t_unknown)):
					continue
				# If both are unknown, we can't do anything so continue:
				if isinstance(fType, Sail_t_unknown) and isinstance(aType, Sail_t_unknown):
					continue
				# Otherwise, resolve the unknown var.  Note:
				# - Only a SailBoundVar can have raw type of Sail_t_unknown, so we shoul be safe using its setType() method
				# - self.fn must be a SailFn (not a SailHandwrittenFn) by this point.
				unknownTerm = self.actuals[i] if isinstance(aType, Sail_t_unknown) else self.fn.getFormals()[i]
				knownType = formalTypes[i] if isinstance(aType, Sail_t_unknown) else actualTypes[i]
				unknownTerm.setType(knownType)

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
		if self.fn.getType() is None:
			sys.exit(f"Error: function {self.fn.getName()} has not RHS type - you may need to specify it yourself")
		return self.fn.getType().getRHS()

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
		return Sail_t_int()

	def getEffects(self, ctx):
		return set([])

	def pp(self):
		return str(self.num)


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

		self.resolveNilCB = None

	### Required methods ###
	def getType(self):
		# Extract types and check they're the same
		thenType = self.thenTerm[0].getType()
		elseType = self.elseTerm[0].getType()
		if thenType != elseType:
			if isNumeric(thenType) and isNumeric(elseType):
				return Sail_t_int()
			elif isinstance(thenType, Sail_t_tuple) and isinstance(elseType, Sail_t_tuple):
				if thenType != elseType:
					print("Error: then tuple not compatible with else tuple")
					print(f"\tThen tuple/type: {self.thenTerm[0].pp()} : {thenType.pp()}")
					print(f"\tElse tuple/type: {self.elseTerm[0].pp()} : {elseType.pp()}")
					sys.exit()
				else:
					return thenType
			else:
				print(f"Error: then type not compatible with else type.  then type: {thenType.pp()}; else type: {elseType.pp()}")
				print(f"if term: {self.ifTerm[0].pp()}; then term: {self.thenTerm[0].pp()}, else term: {self.elseTerm[0].pp()}")
				sys.exit()
		else:
			if isinstance(thenType, Sail_t_error) and isinstance(elseType, Sail_t_error):
				sys.exit("Error: Both branches have `any` type")
			elif isinstance(thenType, Sail_t_error):
				return elseType
			elif isinstance(elseType, Sail_t_error):
				return thenType
			else:
				return thenType

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
		# Cast items which have a numeric type to int)
		return Sail_t_tuple([item.getType() if not isNumeric(item.getType()) else Sail_t_int() for item in self.subItems])

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

	def pp(self):
		itemStrings = []
		for i in self.subItems:
			if isinstance(i, SailNumLit):
				itemStrings.append(f"{i.pp()} : {i.getType().pp()}")
			else:
				itemStrings.append(i.pp())
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
		self.setForceType(forceType)

		# Convert the patterns to bool as required
		if isinstance(self.var.getType(), Sail_t_option):
			self.matches = [(someHelper(p), e) if not isinstance(p, SailUnderScoreLit) else (p,e,) for (p, e) in self.matches]

		# Resolve nils for the expressions
		exprs = [e for (_, e) in self.matches]
		resolveNils(exprs)

		self.resolveNilCB = None

	### Custom methods ###
	def getMatches(self):
		return self.matches

	def setForceType(self, ft):
		self.forceType = ft

	### Required methods ###
	def getType(self):
		if self.forceType is None:
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

			return expr_t
		else:
			return self.forceType

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

	def pp(self):
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
			matchLines.append(f"{pat.pp()} => {expr.pp()}")
		matchLines = ",\n".join(matchLines)
		together = f"{headerLine}\n{matchLines}\n}}"

		# Add a type annotation
		withAnnotation = f"({together}) : {self.getType().pp()}"

		return withAnnotation


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
			if e.getType() != struct.getTypeOfName(n):
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
		if type(sailASTelem.getType()) != Sail_t_struct:
			sys.exit("Error: type of Sail AST element to project from is not a struct")

		self.sailASTelem = sailASTelem
		self.fieldName = fieldName

		self.struct = self.sailASTelem.getType().getStruct()

	### Required methods ###
	def getType(self):
		return self.struct.getTypeOfName(self.fieldName)

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

	def __init__(self, vector, index):
		"""
		Args:
			vector: SailASTelem : Sail_t_vector
			index: SailASTelem : Sail_t_int
		"""
		super().__init__()
		self.vector = vector
		self.index = index

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
		return\
f"""let vectorIndex = {self.index.pp()} in {{
	assert(0 <= vectorIndex & vectorIndex <= {self.vector.getType().getLength() - 1});
	({self.vector.pp()})[vectorIndex]
}}"""

	### Nil resolution ###
	def setCallBacks(self):
		sys.exit("Finish implementing nil resolution functions for SailVectorProject!")


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


################################################################################
# Helper and utility functions
################################################################################


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
		names_types[n] = fields[n].getType()

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
			typ = Sail_t_fn([typ], Sail_t_option(typ))),
		actuals = [])


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
	expr_t = items[0].getType().generalise()
	for expr in items[1:]:
		if expr.getType().generalise() != expr_t:
			return False, None

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
			otherTypes = [oe.getType().generalise() for oe in otherExprs]
			resolveNilsInner(otherTypes, e)

		# If the expression is a tuple, go through it looking for nils to resolve and resolve against the types of the
		# expressions at the correct index in their tuple if types
		elif isinstance(e, SailTuple):
			for (j, sub_e) in enumerate(e.getItems()):
				if isinstance(sub_e, SailPlaceholderNil):
					otherTypes = [oe.getType().generalise().getSubTypes()[j] for oe in otherExprs]
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
			f.write('$include "handwritten2.sail"\n')
			f.write('$include "utils.sail"\n\n')

		f.write(pp)

		# A trailing new line is needed for, for example, file only containing `$include`s.
		f.write("\n")

		print(f"Successfully saved file path: {path}; name: {name}")