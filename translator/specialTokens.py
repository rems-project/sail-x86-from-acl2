import transform
from lex_parse import ACL2String, ACL2quote, ACL2qq, CodeTopLevel, pp_acl2, lexLispString, parseACL2
from SailASTelems import *
from SailTypes import parseGuard, Sail_t_fn, translateType, eqSet
import config_files
import config_patterns

import os
import sys

'''
Most functions here implement the translation of 'special tokens'.  The first
element of each Lisp list indicates how it should be translated and how the
tail elements should be interpreted.  For example, consider the following
Lisp form:

	(if t (+ 1 5) (- 6 2))

the first element indicates we should translate this the the Sail code:

	if X then Y else Z
	
where X, Y and Z are the recursively translated expressions for `t`,
`(+ 1 5)` and `(- 6 2)` respectively.  I.e.

	X := `true`
	Y := `(1 + 5)`
	Z := `(6 - 2)`.
	
Some special tokens are built in to Lisp/ACL2 and so must be translated such
(`if` is a good example).  Others are expedient to translate like this.

Each function below has the following type:

	(ACL2ast : [ACL2astElem], env : Env)
	-> (Sailast' : [SailastElem],
		env' : Env,
		consumed : int)
		
In other words, it takes an ACL2 AST and the current environment and returns
the translated Sail AST, the updated environment and the number of tokens
consumed.  ACL2astElem represents the classes defined in lex_parse.py.

The functions are collected in `config_function_maps.loadManualEnv()`.  It is
expected that most functions will call the environment in order to translate
their subexpressions.

The rest of this file is split into two parts:
 1. Helper functions
 2. Special token functions and other helper functions
'''


def _printLine():
	print("-"*80)


################################################################################
# Helper functions
################################################################################
def _hasKeyword(ACL2ast, keyword):
	"""
	Tests if the ACL2 AST contains the keyword parameter `keyword`.
	Automatic case converstion.

	Args:
		ACL2ast: [ACL2astElem]
		keyword: str

	Returns:
		bool
	"""
	for elem in ACL2ast:
		if isinstance(elem, str) and elem.upper() == keyword.upper():
			return True

	return False

def _getValueOfKeyword(ACL2ast, keyword, errorOnMoreThanOne=False):
	"""
	Some ACL2 terms have extra information in the form of keywords: symbols
	which begin with a colon, e.g. ':guard'.  The value associated with each
	keyword is the next term along.  This function extracts the values for the
	given keywords (values plural in case the keyword appears more than once).

	Args:
		- ACL2ast : [ACL2astElem]
		- keyword : str - including the colon e.g. ':guard' not 'guard'
		- errorOnMoreThanOne : bool - throw exception if a keyword is used more
									  than once
	Returns:
		- [ [ACL2astElem] | ACL2astElem ]
	"""
	# Iterates through the top level of ACL2ast finding instances of the
	# keyword
	toReturn = []
	for i in range(len(ACL2ast)):
		if isinstance(ACL2ast[i], str) and  ACL2ast[i].upper() == keyword.upper():
			toAppend = ACL2ast[i+1]
			if type(toAppend) in [ACL2Comment, NewLine]:
				sys.exit(f"Error: incorrect type of keyword value '{toAppend}' in {ACL2ast}")
			toReturn.append(toAppend)

	if errorOnMoreThanOne and len(toReturn) > 1:
		sys.exit(f"Error: more than one instance of keyword {keyword} found in {ACL2ast}")

	return toReturn

def _extractAllKeywords(ACL2ast, failOnRedef):
	"""
	Searches through the top level of the given ACL2 AST for keywords and
	extracts the value of each.  Returns a dictionary of keywords and their
	values as well as the original AST with keywords stripped out.
	Keywords are converted to uppercase.

	Args:
		- ACL2ast : [ACL2astElem]
		- failOnRedef : bool - throw exception if a keyword is used more than once

	Returns:
		(
			{ (keyword : str) : (value : [[ACL2astElems] | ACL2astElem]) },
			[ACL2astElems] - without keywords
		)
	"""
	i = 0
	keywords = {} # {(keyword : str) : (value : [[ACL2astElems] | ACL2astElem])}
	remainder = []
	while i < len(ACL2ast):
		item = ACL2ast[i]
		if isinstance(item, str):
			item = item.upper()
			if isinstance(item, str) and item.startswith(':'):
				if failOnRedef and item in keywords: sys.exit(f"Error: keyword `{item}` used twice - {ACL2ast}")
				if item not in keywords: keywords[item] = []
				keywords[item].append(ACL2ast[i+1])
				inc = 2
			else:
				remainder.append(item)
				inc = 1
		else:
			remainder.append(item)
			inc = 1
		i += inc

	return keywords, remainder

def _filterExtract(ACL2ast, numOfElems, subTypes, subLengths, debugName, comments=False):
	"""
	Remove newlines (and optionally comments) from ACL2ast then perform checks.
	Checks are:
	 -  Whether the filtered AST is the correct length
	 -  The types of sub items of the filtered AST
	 -  The size of sub items of the filtered AST

	Args:
		- ACL2ast 	 : [ACL2astElem]

		- numOfElems : int | None			Expected number of items in
											ACL2ast list.

		- subTypes 	 : [[type] | [None] ]	Permitted types of each of the
											numOfElems sub items.

		- subLengths : [[int] | [None]]		Permitted lengths of each of the.
											numOfElems sub items which are
											lists.

		- debugName  : str					Name to print in error messages.

		- comments 	 : bool					Whether to filter comments from
											the top level or not.
	Returns:
		- tuple ( ACL2astElem )
	"""
	# Filter out newlines and comments
	ACL2astFiltered = filterAST(ACL2ast, comments=comments)

	if numOfElems is None:
		numOfElems = len(ACL2astFiltered)		
	else:
		# Check overall length
		if len(ACL2astFiltered) != numOfElems:
			sys.exit(f"Error in {debugName}: '{debugName}' expected {numOfElems} elements, got {len(ACL2astFiltered)} in:\n{ACL2astFiltered}")

		# Check parameters the other parameters to this function
		if len(subLengths) != numOfElems - 1: sys.exit(f"Error: Expected {numOfElems - 1} items in lengths formal in {debugName}, got {subLengths}")
		if len(subTypes) != numOfElems - 1: sys.exit(f"Error: Expected {numOfElems - 1} items in types formal in {debugName}, got {subTypes}")

	# Check types (and if necessary lengths) of sub items
	for (item, length, types) in zip(ACL2astFiltered[1:], subLengths, subTypes):
		if types is not None and type(item) not in types: sys.exit(f"Error: {item} not in permitted types {types} whilst decoding {debugName}")
		if length is not None and isinstance(item, list) and len(item) != length: sys.exit(f"Error: {item} not of length {length} whilst decoding {debugName}")

	# Return
	return tuple(ACL2astFiltered[1:])

def filterAST(ACL2ast, comments=False):
	"""
	Filter out NewLines and optionally comments from the top level of a
	Sail AST
	"""
	if isinstance(ACL2ast, str):
		return ACL2ast

	if comments:
		return [item for item in ACL2ast if type(item) not in [NewLine, ACL2Comment]]
	else:
		return [item for item in ACL2ast if type(item) not in [NewLine]]

################################################################################
# Special token functions and other helper functions
################################################################################


def tr_in_package(ACL2ast, env):
	"""
	We can simply ignore `in-package` forms as they are ACL2 'events' and have
	no translation.
	"""
	return [None], env, len(ACL2ast)


def tr_include_book(ACL2ast, env):
	"""
	Roughly, `include-book` forms translate as `$include` expressions in Sail.
	This function triggers the translation of the file being included if it
	hasn't been translated already.

	Caveats include:
	 -  Handling `:dir` keywords which specify the location of the book
	 -  The Sail files are not translated hierarchically, so sometimes name
	 	collisions must be avoided.
	 -  Skipping certain files defined in configuration.py.

	TODO:
	 -  Handle `:dir :utils` in a more reliable way
	 -  Handle name collisions propery by having the translated Sail match the
	   	ACL2 hierarchy.
	"""
	_printLine()

	# If a ':dir' keyword is specified, use that, otherwise just use the specified path/name
	keywordDirs = _getValueOfKeyword(ACL2ast, ':dir', errorOnMoreThanOne=True)
	if keywordDirs == []:
		# Extract path/name of file to include
		thisPath, thisFile = os.path.split(ACL2ast[1].getString())
	else:
		keywordDir = keywordDirs[0]
		if keywordDir == ':utils':
			thisPath = '../utils'
			thisFile = ACL2ast[1].getString()
		else:
			sys.exit(f"Error: unknown :dir - {keywordDir}")

	# Hack to avoid collision of the two `segmentation` and `top` files.
	thisFileRename = thisFile
	if thisFile.lower() == 'segmentation' and env.getFile() == 'top':
		thisFileRename = 'segmentationInst'
	if thisFile.lower() == 'top' and env.getFile() == 'top':
		thisFileRename = 'topFP'

	# The file may already have been translated
	isIncluded, ast = env.isIncluded(thisFileRename)
	if isIncluded:
		print(f"Returning from `include` of {thisFileRename} to file: {env.getFile()} because already translated.")
		return [ast], env, len(ACL2ast)

	# Skip certain files
	if thisFileRename not in config_patterns.exclusions_files:
		# Push the path and file to the env, perform the translation, then pop.
		env.pushPath(thisPath)
		env.pushFile(thisFileRename)

		print(f"Generating Sail for included file: {thisFileRename}")
		sailAST, env2, consumed = transform.transformACL2FiletoSail(f'{thisFile}.lisp', env)

		env2.popPath()
		env2.popFile()

		# Add a Sail `include` for the file translated above
		toReturn = SailInclude(sailAST,
							   config_files.outputFolder,
							   thisFileRename,
							   includeHeaders=False,
							   env=env)
		env.addToIncluded(thisFileRename, toReturn)
		print(f"Returning from `include` of {thisFileRename} to file: {env2.getFile()} because finished translating")
		return [toReturn], env, len(ACL2ast)
	else:
		print(f"Skipping `include`: {thisFileRename}")
		return [None], env, len(ACL2ast)

def tr_local(ACL2ast, env):
	"""
	Local events are only processed when checking admissibility in ACL2
	`:logic` mode and are not exported by the encapsulation or book.
	We can ignore local events in the translation.

	If we do need to use local events in the future, we can check the
	current context by using

		ctx = env.peekContext2()

	Examples of values `ctx` can take:
	 -  'include-book'
	 -  'defsection'
	 -  'encapsulate'

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____LOCAL
	"""
	return [None], env, len(ACL2ast)


def tr_defsection(ACL2ast, env):
	"""
	Technically, `defsection` is used to introduce a new scope for `local`
	events.  They are used more frequently for documentation.
	In particular, we translate the following keyword parameters as comments
	in Sail (before translating the body as per normal):

	 -  `:short`
	 -  `:long`

	We ignore the following keyword parameters:

	 -  ':name`
	 -  `:parents`
	 -  `autodoc`
	 -  `extension`

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____DEFSECTION
	"""
	_printLine()

	# Extract keyword arguments and add those representing comments to the AST.
	SailAST = []
	keywords, remainder = _extractAllKeywords(ACL2ast, failOnRedef=False)
	for kw in [':short', ':long']:
		if kw.upper() in keywords:
			SailAST.extend([ACL2Comment(comment.getString()) for comment in keywords[kw.upper()]])

	# Remove the first two items ('defsection name') from the AST with
	# keywords removed
	newTopLevel = remainder[2:]

	# Translate the body of the `defsection`
	if len(newTopLevel) == 0:
		return [None], env, len(ACL2ast)
	(SailTopLevel, env, _) = transform.transformACL2asttoSail(CodeTopLevel(newTopLevel), env)
	SailAST.extend(SailTopLevel)

	return SailAST, env, len(ACL2ast)

def _parseNormalFormal(f, env):
	"""
	Parses a `Formal` as per the syntax for extended formals found here:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS

	Args:
		f : a formal
		env : Env

	Returns:
		(name : str,
		types : eqSet(SailType) | None)
	"""
	# Formal arguments are often represented by symbols, but sometimes a list is provided which gives extra type
	# information.
	if isinstance(f, str):
		return f, None
	elif isinstance(f, list) and len(f) == 1:
		return f[0], None
	elif isinstance(f, list):
		# Extract the formal name
		if not isinstance(f[0], str): sys.exit("Error: first element of formal parameter list is not a symbol")
		name = f[0]

		# Extract type information
		if _hasKeyword(f, ':type'):
			# Use ':type' keyword if it exists
			possibleTypes = _getValueOfKeyword(f, ':type')
			types = eqSet()
			for pt in possibleTypes:
				# The type spec may may be a list or a string
				if isinstance(pt, list):
					types.add(translateType(env, pt[0], pt[1:]))
				elif isinstance(pt, str):
					types.add(translateType(env, pt))
				else:
					sys.exit(f"Error: invalid type spec - {pt}")
		else:
			# Otherwise assume we have an 'inline guard'
			if len(f) != 2: print(f"Warning: you may need to implement more to parse this formal: {f}")
			guardWord = f[1]
			name = f[0]
			if isinstance(guardWord, str):
				types = parseGuard([guardWord, name]).get(name, None)
			elif isinstance(guardWord, list):
				types = parseGuard(guardWord).get(name, None)
			else:
				sys.exit(f"Error: invalid guard - {guardWord}")

		# Return
		return name, types
	else:
		sys.exit("Error: item in formals not a symbol or list")

def _parseKeywordFormal(f, env):
	"""
	Parses an `OptFormal` as per the syntax for extended formals found here:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS

	Args:
		f: a formal
		env: Env

	Returns:
		(name : str,
		types : eqSet(SailType) | None,
		default: SailASTelem,
		env)
	"""
	if isinstance(f, str): 						# I.e. just the name
		name, typ = _parseNormalFormal(f, env)
		default = SailPlaceholderNil()
	elif isinstance(f, list) and len(f) == 1: 	# I.e. (varname Item*) where Item can be xdoc, guard, :key val
		name, typ = _parseNormalFormal(f, env)
		default = SailPlaceholderNil()
	elif isinstance(f, list) and len(f) == 2: 	# I.e. (Formal, 'val) - a default value is given
		if isinstance(f[1], ACL2quote): toTranslate = f[1].getAST()
		else: toTranslate = f[1]
		name, typ = _parseNormalFormal(f[0], env)
		if typ is not None and isinstance(typ.resolve(), Sail_t_bool):
			env.setCurrentType(SailPlaceholderNil.BOOL)
		default, env, _ = transform.transformACL2asttoSail([toTranslate], env)
		env.clearCurrentType()
		default = default[0]
	else:
		sys.exit(f"Error: malformed keyword formal - {f}")

	return name, typ, default, env

def _parseFormals(fnFormals, mode, env):
	"""
	Parses extended formals as per a partial implementation of the syntax
	found here:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS

	Args:
		fnFormals: [str | [ACL2astElem]]
		mode : str - 'normal', 'optional', 'key'
		env: as above

	Returns:
		(names : [str],
		types : {str: eqSet(SailType},
		keyDefaults : {str : SailASTelem},
		env)
	"""
	### Base case: no formals left to parse
	if fnFormals == []:
		return [], {}, {}, env

	### Recursive cases
	# Extract structure from list of formals
	head = fnFormals[0]
	tail = fnFormals[1:]

	# Start of keyword/optional
	if isinstance(head, str):
		if head.lower() == '&optional':
			return _parseFormals(tail, 'optional', env)
		if head.lower() == '&key':
			return _parseFormals(tail, 'key', env)

	# Parse a normal formal
	if mode == 'normal':
		name, types = _parseNormalFormal(head, env)
		tailNames, tailTypes, tailKeyDefaults, env = _parseFormals(tail, 'normal', env)
		if name in tailNames: sys.exit(f"Error: name '{name}' already parsed, mode='normal'")
		if types != None:
			tailTypes[name] = types
		return [name] + tailNames, tailTypes, tailKeyDefaults, env

	# Parse an optional formal
	elif mode == 'optional':
		sys.exit("Error: &optional arguments not yet implemented")

	# Parse a keyword formal
	elif mode == 'key':
		name, types, keyDefault, env = _parseKeywordFormal(head, env)
		tailNames, tailTypes, tailKeyDefaults, env = _parseFormals(tail, 'key', env)
		if name in tailNames: sys.exit(f"Error: name '{name}' already parsed, mode='key'")
		tailKeyDefaults[name] = keyDefault
		return tailNames, tailTypes, tailKeyDefaults, env

	# Otherwise error
	else:
		sys.exit(f"Error: unrecognised formal parsing mode {mode}")


def tr_define(ACL2ast, env):
	"""
	Function definitions (via `define` or `defun`) are one of the more
	complex pieces of ACL2 code.  At a high level, this function
	performs the following actions:

	 -  Splits the `define` into constituent parts (name, formals, body)
	 -  Ignores functions whose bodies are quotes or QQs in the hope they
	 	will be expanded later.
	 -  Parse formals and extracts type info using above helper functions
	 	_parseFormals(), _parseKeywordFormal() and _parseNormalFormal().
	 -	Handles keyword arguments by creating the relevant structs.
	 -  Ensures correct binding/unbinding order of names and types of
	 	parameters in order to allow recursion.

	Sail functions require a type annotation so, when translating, the types
	of formal parameters and the return type must be inferred.  The latter is
	done as a class method in SailFn and performs normal type inference over
	the Sail AST.  We can gain information for the former in three places:

	 1. Function and parameter guards.  Function guards aren't used currently,
		parameter guards are used in _parseFormals()
	 2. `:type` annotations are used in _parseFormals().
	 3. We can gain information about parameter types by examining where they
	 	are used.  This is implemented as a post-translation pass of the AST
	 	elsewhere.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____DEFINE
	"""
	_printLine()

	# Setup dummy return values,
	SailAST = []
	thisSailFn = SailFn()

	# Extract and filter out extended options, adding comments to the return
	# AST but ignoring `:parents`, `:inline`, `no-function` etc.  Handle typing
	# using `:guard` later.
	extendedOpts, ACL2astRemainder = _extractAllKeywords(ACL2ast, failOnRedef=False)

	for kw in [':short', ':long']:
		if kw.upper() in extendedOpts:
			SailAST.extend([ACL2Comment(comment.getString()) for comment in extendedOpts[kw.upper()]])

	guardTypes = {}
	if ':guard'.upper() in extendedOpts:
		for guard in extendedOpts[':guard'.upper()]:
			newTypes = parseGuard(guard)
			guardTypes = utils.dictExtend(guardTypes, newTypes)

	# Split the rest into pre- and post-///
	# We use the content before ///, but ignore the content after
	if '///' in ACL2astRemainder:
		slashesIndex = ACL2astRemainder.index('///')
		preSlashes = ACL2astRemainder[:slashesIndex]
	else:
		preSlashes = ACL2astRemainder

	# Split pre-/// into component parts
	# assert preSlashes[0] == 'define'
	fnName = preSlashes[1]
	fnFormals = preSlashes[2]
	fnRest = preSlashes[3:-1]
	fnBody = preSlashes[-1]

	print(f"Name: {fnName}")
	# print(f"Formals: {fnFormals}")
	# print(f"Rest: {fnRest}")
	# print(f"Body: {fnBody}")

	env.setDefineSlot(fnName)

	# If the function body is a quote or quasi quote, do not proceed - we hope
	# it will be used in a useful way later
	if type(fnBody) in [ACL2quote, ACL2qq]:
		print(f"WARNING: body of function {fnName} is a quote/QQ, ignoring it.")
		env.setDefineSlot("")
		return [None], env, len(ACL2ast)

	# Set the type if we've specified it manually
	if fnName.lower() in config_patterns.forced_return_types:
		thisSailFn.setForceRHSType(config_patterns.forced_return_types[fnName.lower()])

	# Check for forced argument types
	if fnName.lower() in config_patterns.forced_argument_types:
		fnFormalsForced = config_patterns.forced_argument_types[fnName.lower()]
	else:
		fnFormalsForced = {}

	# Perform rudimentary checks
	if not isinstance(fnName, str): sys.exit("Error: function name not a symbol")
	if not isinstance(fnFormals, list): sys.exit("Error: function formals not a list")
	for f in fnFormals:
		if type(f) not in [str, list]: sys.exit(f"Error: item in formals not a symbol or list, found: {type(f)}")
	if not isinstance(fnBody, list): sys.exit("Error: function body not a list")

	# === Parse and extract type information from the formals.
	# 		(fnFormalsFiltered : [str],
	# 		fnFormalsTyped : {str: eqSet(SailType},
	# 		keyDefaults : {str : SailASTelem},
	fnFormalsFiltered, fnFormalsTyped, keyDefaults, env = _parseFormals(fnFormals, 'normal', env)
	numNonKeywordFormals = len(fnFormalsFiltered)

	# Merge type information from guards
	fnFormalsTyped = utils.dictExtend(fnFormalsTyped, guardTypes)
	for (f, typs) in fnFormalsTyped.items():
		if isinstance(typs, eqSet):
			# Try to find the most specific type out of the available options
			typ = typs.resolve()
			for t in typs.items():
				if isSubType(t, typ):
					typ = t
			fnFormalsTyped[f] = typ

	# Add the formal parameter bindings to the stack with their types if we have them
	fnFormalsBVs = []
	for f in fnFormalsFiltered:
		if f.lower() in fnFormalsForced:
			bv = env.pushToBindings([f], [fnFormalsForced[f.lower()]])
		elif f in fnFormalsTyped:
			# generalisedType = fnFormalsTyped[f].resolve()
			# bv = env.pushToBindings([f], [generalisedType])
			bv = env.pushToBindings([f], [fnFormalsTyped[f]])
		else:
			bv = env.pushToBindings([f], [Sail_t_unknown()])
		fnFormalsBVs.append(bv[0])

	# Deal with keyword arguments by creating a struct and registering it in
	# the relevant places
	struct = None
	keywordsToPop = []
	if len(keyDefaults) != 0:
		struct = createStructWithDefault(f"Struct_{fnName}", keyDefaults)
		SailAST.append(struct)

		keywordType = Sail_t_struct(struct)
		keywordBV = env.pushToBindings(tokens=['keywords'], types=[keywordType])
		keywordBV = keywordBV[0]
		fnFormalsFiltered.append('keywords')
		fnFormalsTyped['keywords'] = eqSet([keywordType])
		fnFormalsBVs.append(keywordBV)
		keywordsToPop = []
		for (kd, sailKd) in keyDefaults.items():
			env.pushToBindings(tokens=[kd], customSail=[SailStructProject(keywordBV, kd)])
			keywordsToPop.append(kd)

	# Add the name and formals to the dummy SailFn and register in the auto
	# environment in order to allow recursion
	thisSailFn.setName(fnName)
	thisSailFn.setFormals(fnFormalsBVs)
	env.addToAuto(fnName, apply_fn_gen(thisSailFn, numNonKeywordFormals, struct))

	# === Translate the body
	# Evaluate the body - no events should take place but use the returned environment anyway
	(SailItem, env, _) = transform.transformACL2asttoSail(fnBody, env)

	# Amend the SailFn object to contain the body definition and add to the AST
	thisSailFn.setBody(SailItem)
	SailAST.append(thisSailFn)

	# === Tidy up
	# Pop the formal parameter bindings
	env.popWithCheck(keywordsToPop)
	env.popWithCheck(fnFormalsFiltered)

	# Return early if no Sail ast was produced
	if SailItem == [None]:
		env.setDefineSlot("")
		return [None], env, len(ACL2ast)

	# Return
	env.setDefineSlot("")
	return SailAST, env, len(ACL2ast)

def tr_make_event(ACL2ast, env):
	"""
	From here: http://www.cs.utexas.edu/users/moore/acl2/v6-3/MAKE-EVENT.html

			'The expression (make-event form) replaces itself with the result
			of evaluating form, say, ev, as though one had submitted ev
			instead of the make-event call.'

	We thus translate `form` with the caveat that it should be passed through
	a running instance of ACL2 first to remove quotations.
	"""
	_printLine()

	# Extract `form` and send to ACL2 server for evaluation before translating
	# the result
	form = ACL2ast[1]
	newAST = env.evalACL2(form)
	(SailAST, env, _) = transform.transformACL2asttoSail(newAST[0], env)

	# Return
	return SailAST, env, len(ACL2ast)

def tr_defmacro(ACL2ast, env):
	"""
	When a macro is used we query the ACL2 server to find its expansion.
	A `defmacro`, however, is the definition of a macro, and so we need to
	register that it exists.  This is so, when we encountered it later, we
	know how to handle it.  Thus, we do not need to examine the macro body
	here.

	General Form:
	(defmacro name macro-args doc-string dcl ... dcl body)

	Fortunately, we only concern ourselves with the first two items.
	"""
	_printLine()

	# Extract the name and perform rudimentary check
	macroName = ACL2ast[1]
	if not isinstance(macroName, str): sys.exit("Error: macro name not a symbol")

	# Originally `apply_macro_gen` checked it received the expected number of
	# arguments.  It does not any more, hence why we pass `None`.
	env.addToAuto(macroName, apply_macro_gen(numOfArgs=None))

	# Return
	return [None], env, len(ACL2ast)


def tr_mbe(ACL2ast, env):
	"""
	`mbe` forms allow different forms to be used for theorem proving (the
	`:logic` branch) vs. execution (the `:exec` branch).  Both branches must
	be proved to be equal so we can chose which to used.  The branch to
	translate is set in configuration.py.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____MBE
	"""
	_printLine()

	# Check we have the expected number of elements in the mbe
	if len(ACL2ast) != 5:
		print(f"Error: unexpected number of items in `mbe` ast: {len(ACL2ast)} items")
		print(f"AST:\n{ACL2ast}")
		sys.exit(1)

	# Extract elements
	logicCode = _getValueOfKeyword(ACL2ast, ':logic')
	execCode  = _getValueOfKeyword(ACL2ast, ':exec')

	# Perform rudimentary checks
	if len(logicCode) != 1: sys.exit(f'Error: wrong number of :logic keywords in: {ACL2ast}')
	if len(execCode)  != 1: sys.exit(f'Error: wrong number of :exec keywords in: {ACL2ast}')
	if type(logicCode[0]) not in [list, str]: sys.exit(f"Error: Unexpected MBE :logic code type, expected list, got: {type(logicCode[0])}")
	if type(execCode[0]) not in [list, str]: sys.exit(f"Error: Unexpected MBE :exec code type, expected list or string, got: {type(execCode[0])}")

	# Select which branch
	if config_files.mbe_branch == ':logic':
		(SailAST, env, consumed) = transform.transformACL2asttoSail(logicCode[0], env)
	elif config_files.mbe_branch == ':exec':
		(SailAST, env, consumed) = transform.transformACL2asttoSail(execCode[0], env)
	else:
		sys.exit(f"Error: unexpected mbe_switch value {config_files.mbe_branch}")

	# Return
	return SailAST, env, len(ACL2ast)

def tr_ignore(ACL2ast, env):
	"""
	There are some forms we can ignore wholesale.  E.g. `defthm`.  See
	`loadManualEnv` for a complete list and descriptions.
	"""
	return [None], env, len(ACL2ast)

def tr_if(ACL2ast, env):
	"""
	Translates an `(if <cond-form> <then-form> <else-form>)` form.

	Various optimisations are performed here:
	 -  When testing for `app-view` only the then branch is translated to avoid
	 	handling system level view.
	 -  In `(if _ t nil)` force `t` and `nil` to be booleans.

	TODO:
		For the `app-view` hack, translate the else branch as a translation
		error rather than simply ignoring it.
	"""
	_printLine()

	# Check we have the expected number of elements
	if len(ACL2ast) != 4:
		sys.exit(f"Error: unexpected number of items in `if` ast: {len(ACL2ast)} items")

	# Extract elements
	ifTerm   = ACL2ast[1]
	thenTerm = ACL2ast[2]
	elseTerm = ACL2ast[3]

	# Perform rudimentary checks
	validTypes = [str, list, ACL2quote]
	if type(ifTerm) not in validTypes: sys.exit(f"Error: unexpected `if` term type: {type(ifTerm)}")
	if type(thenTerm) not in validTypes: sys.exit(f"Error: unexpected `then` term type: {type(thenTerm)}")
	if type(elseTerm) not in validTypes: sys.exit(f"Error: unexpected `else` term type: {type(elseTerm)}")

	# Translate subterms.  Hacks:
	# - Avoid handling system-view for now.
	# - If we have just `if _ t nil` then force t and nil to both be bools (don't just return the predicate as it
	#	might not be a bool.
	if (isinstance(ifTerm, list) and ifTerm[0].lower() == 'app-view') or\
			(isinstance(ifTerm, list) and isinstance(ifTerm[0], SailBoolLit) and
			ifTerm[0].getBool()):

		(thenTermSail, env, _) = transform.transformACL2asttoSail(thenTerm, env)
		toReturn = thenTermSail
	elif isinstance(thenTerm, str) and thenTerm.lower() == 't' and \
			isinstance(elseTerm, str) and elseTerm.lower() == 'nil':

		(ifTermSail, env, _) = transform.transformACL2asttoSail(ifTerm, env)
		toReturn = [SailIf(ifTermSail, [SailBoolLit(True)], [SailBoolLit(False)])]
	else:
		(ifTermSail, env, _) = transform.transformACL2asttoSail(ifTerm, env)
		(thenTermSail, env, _) = transform.transformACL2asttoSail(thenTerm, env)
		(elseTermSail, env, _) = transform.transformACL2asttoSail(elseTerm, env)
		toReturn = [SailIf(ifTermSail, thenTermSail, elseTermSail)]

	# Construct the Sail ast `if` element and return
	return toReturn, env, len(ACL2ast)

def tr_encapsulate(ACL2ast, env):
	"""
	Encapsulates come in two forms, indicated by the first symbol after
	`encapsulate`:
	1) First symbol is nil: a 'trivial' encapsulate
	2) Second symbol is non-nul: a 'non-trivial' encapsulate

	We translate both using the same method but the second case should be
	handled manually.  There are only two instances of the second case
	(in 'register-readers-and-writers.lisp' and 'cpuid.lisp)', which
	the former relates to undefined behaviour.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____ENCAPSULATE
	"""
	_printLine()

	# Filter and extract elements
	filtered = _filterExtract(ACL2ast, None, [[list]], [None], f"`encapsulate`", comments=True)
	trivialp = filtered[0]
	body = filtered[1:]

	# Perform rudimentary checks
	if trivialp != []: print("WARNING: non-trivial encapsulate not implemented, treating as trivial")

	# For each further element, just translate it - `tr_local` handles whether it should be included or not
	toReturn = []
	for acl2Term in body:
		(sailTerm, env, consumed) = transform.transformACL2asttoSail(acl2Term, env)
		if consumed != len(acl2Term): print(f"WARNING: not all of an encapsulate event was translated.  Consumed = {consumed}; length = {len(acl2Term)}; term: {acl2Term}")
		toReturn.extend(sailTerm)

	# Return
	return toReturn, env, len(ACL2ast)


def tr_zp(ACL2ast, env):
	"""
	Test for 0.  Translate as `x == 0` in Sail.  In reality `(zp x)` returns
	`t` if `x` is 0 or is not a natural number.  The translation is thus an
	approximation.
	"""
	_printLine()

	# Filter and extract elements then translate operand.
	(operand, ) = _filterExtract(ACL2ast, 2, [[list, str]], [None], "`zp`")
	(operandSail, env, _) = transform.transformACL2asttoSail([operand], env)

	# Return
	return ([SailApp(
				SailHandwrittenFn('==', Sail_t_fn([], Sail_t_bool())),
				[operandSail[0], SailNumLit(0)],
				infix = True)],
			env, len(ACL2ast))

def num_op_gen(op, resultType, operandType=None, numOfArgs=None, infix=True):
	"""
	Generates a function which conforms to the spec described at the start of
	this file and which can be applied to an ACL2 AST to generate a simple
	operation such as `+` or `<`.

	Args:
		- op : str - e.g. '+'
		- resultType : SailType - e.g. Sail_t_int()
		- numOfArgs : int | None
		- infix : bool
	"""
	def tr_num_op(ACL2ast, env):
		# Extract the arguments and perform rudimentary checks
		args = ACL2ast[1:]
		if not all(type(item) in [list, str, ACL2quote] for item in args): sys.exit(f"Error: type of num op argument not permitted: {ACL2ast}")
		if numOfArgs is not None and len(args) != numOfArgs: sys.exit(f"Error: incorrect number of args to num op {ACL2ast}")

		# Translate the arguments and get their types
		argsSail = []
		typesSail = []
		for i in args:
			# Translate
			(aSail, env, _) = transform.transformACL2asttoSail(i, env)
			argsSail.append(aSail)

			# Get type
			try:
				typesSail.append(aSail[0].getType())
			except:
				print(f"Warning: could not infer type for argument in num op")
				typesSail.append(Sail_t_int())

		# Check we have at least 2 elements for the num op
		if len(argsSail) < 2: sys.exit(f"Error: not enough arguments for num op {ACL2ast}")

		nonlocal operandType
		if operandType is None:
			if len(argsSail) > 2:
				operandType = resultType
			else:
				operandType = argsSail[0][0].getType()
				for arg in argsSail[:-1]:
					operandType = mergeTypes(operandType, arg[0].getType())
					if operandType is None:
						sys.exit(f"Error: cannot merge types in num op {ALC2ast}")

		for (i, arg) in enumerate(argsSail):
			new = coerceExpr(arg[0], operandType)
			if new is None:
				sys.exit(f"Error: cannot coerce operand ({arg[0].pp()} : {arg[0].getType().pp()}) to {operandType.pp()}")
			argsSail[i] = [new]

		# Construct the base AST and remove those elements from the args/types lists
		fnType = Sail_t_fn([operandType, operandType], resultType)
		currentAST = [SailApp(
						fn = SailHandwrittenFn(op, typ = fnType),
						actuals = argsSail[-2] + argsSail[-1],
						infix = infix)]

		argsSail = argsSail[:-2]
		typesSail = typesSail[:-2]

		# Construct the rest of the tree
		while len(argsSail) > 0:
			currentAST = [SailApp(
							fn = SailHandwrittenFn(op, typ = fnType),
							actuals = argsSail[-1] + currentAST,
							infix = infix)]
			argsSail = argsSail[:-1]
			typesSail = typesSail[:-1]


		# Return
		return currentAST, env, len(ACL2ast)

	return tr_num_op

def bitwise_op_gen(operation):
	def funcToApply(args, env):
		typ0 = args[0].getType()
		typ1 = args[1].getType()
		retType = mergeTypes(typ0, typ1)
		if not isinstance(retType, Sail_t_bits):
			retType = Sail_t_int()
		return SailHandwrittenFn(operation, typ=Sail_t_fn([retType, retType], retType))
	return apply_dependent_fn_gen(funcToApply, 2)

def _the_helper(theType, sailTerm):
	"""
	Helper for `tr_the`.  See documentation in `tr_the` for details on `the`.
	The correct wrapper function from handwritten.sail is chosen based on the
	`theType`.

	Args:
		- theType : SailType
		- sailTerm : [SailASTelem] | SailASTelem
	Returns:
		- [SailAstElem]
	"""
	if not config_files.translate_the:
		return sailTerm

	# The sail term should be a symbol or single valued list
	if type(sailTerm) not in [str, list]: sys.exit(f"Error: `the` term not a string or list - {sailTerm}")
	if isinstance(sailTerm, list) and len(sailTerm) != 1: sys.exit(f"Error: `the` list not length - {sailTerm}")

	try:
		termType = sailTerm[0].getType()
	except:
		termType = Sail_t_unknown()

	if termType == theType:
		retTerm = sailTerm[0]
	# Select the correct Sail `the`
	elif isinstance(theType, Sail_t_int):
		retTerm = coerceExpr(sailTerm[0], theType)
	elif isinstance(theType, Sail_t_nat):
		fnType = Sail_t_fn([Sail_t_int()], theType, {'escape'})
		retTerm = SailApp(fn = SailHandwrittenFn(name="the_nat", typ=fnType), actuals = sailTerm)
	elif isinstance(theType, Sail_t_range):
		(low, high) = theType.getRange()
		# Preserve existing constraints on the Sail term, e.g. don't
		# cast {|1, 2, 4|} to range(1, 4), which would lose information
		resultType = intersectTypes(termType, theType)
		if resultType is None:
			print(f"Warning: Failed to determine result type of `the_range({low}, {high}, {sailTerm[0].pp()})`")
			resultType = theType
		fnType = Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], resultType, {'escape'})
		fn = SailHandwrittenFn(name="the_range", typ=fnType)
		retTerm = SailApp(fn=fn, actuals=[SailNumLit(low), SailNumLit(high), sailTerm[0]])
	elif isinstance(theType, Sail_t_bits) and theType.length is not None:
		fnName = "the_sbits" if theType.signed else "the_bits"
		fnType = Sail_t_fn([Sail_t_int(), termType], theType, {'escape'})
		fn = SailHandwrittenFn(name=fnName, typ=fnType)
		retTerm = SailApp(fn=fn, actuals=[SailNumLit(theType.length), sailTerm[0]])
	else:
		sys.exit(f"Error: unexpected type spec in `the` - {theType} in {sailTerm}")

	if retTerm is None:
		sys.exit(f"Error: failed to coerce {sailTerm[0].pp()} to {theType.pp()} in `the`")

	return [retTerm]


def tr_the(ACL2ast, env):
	"""
	In ACL2, `(the <type-spec> <form>)` indicates that `form` has type
	`type-spec`.  In ACL2 this is proved statically, it is translated as a
	dynamic type check in Sail.  Also ee `the_int`, `the_nat` and `the_range`
	in handwritten.sail.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____THE
	"""
	(typeSpec, rest) = _filterExtract(ACL2ast, 3, [[list], [list, str]], [None, None], "`the`")

	# Decode typespec
	theType = translateType(env, typeSpec[0], typeSpec[1:])

	# Decode the rest
	(sailTerm, env, _) = transform.transformACL2asttoSail(rest, env)

	# Get the return ast from the helper function
	retTerm = _the_helper(theType, sailTerm)

	# Encapsulate and return	
	return retTerm, env, len(ACL2ast)

def _parts_helper(lowAST, hiAST, widAST, env):
	"""
	Used for both tr_part_select and tr_part_install.

	Implements translation of logic found here:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____PART-SELECT

	Valid keyword combinations are

		`:low` and `:high`
		`:low` and `:width`

	Note that the `:high` and `:width` combination is not valid.  This function
	translates these keyword options and returns a common interface by
	specifying low and width (which plays well with get_slice_int()).  Thus,
	if `:low` and `:high` are both specified, we need to translate a
	subtraction.

	Args:
		lowAST: [ACL2astElem]
		hiAST: [ACL2astElem]
		widAST: [ACL2astElem]
		env: As above

	Returns:
		( sizeASTsail	: SailNumLit,
		 size			: Int | None,
		 lowASTsail		: [SailASTelem],
		 env			: Env
		)
	"""
	# If :high and :width defined, fail
	if hiAST != [] and widAST != []:
		sys.exit(f"Error: can't use :high and :width in `part-select`.")

	# Otherwise check the other options
	if len(lowAST) == 1 and len(hiAST) == 1:
		# Translate
		(lowASTsail, env, _) = transform.transformACL2asttoSail(lowAST[0], env) # lowASTsail is expected to be a list after the `if` so don't unpack yet
		(hiASTsail, env, _) = transform.transformACL2asttoSail(hiAST[0], env)

		# Create size literal
		if not isinstance(hiASTsail[0], SailNumLit) or not isinstance(lowASTsail[0], SailNumLit):
			size = None

			fnTyp = Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())
			subASTsail = SailApp(fn=SailHandwrittenFn('-', typ=fnTyp), actuals=[hiASTsail[0], lowASTsail[0]], infix=True)
			sizeASTsail = SailApp(fn=SailHandwrittenFn('+', typ=fnTyp), actuals=[subASTsail, SailNumLit(1)], infix=True)
		else:
			size = hiASTsail[0].getNum() - lowASTsail[0].getNum() + 1
			sizeASTsail = SailNumLit(size)

	elif len(lowAST) == 1 and len(widAST) == 1:
		# Translate
		(lowASTsail, env, _) = transform.transformACL2asttoSail(lowAST[0], env)
		(widASTsail, env, _) = transform.transformACL2asttoSail(widAST[0], env)

		# Create size AST
		sizeASTsail = widASTsail[0]

		# Create size literal
		if not isinstance(sizeASTsail, SailNumLit):
			size = None
		else:
			size = sizeASTsail.getNum()
	else:
		sys.exit(f"Error: incorrect keywords in this `_part_helper`")

	return sizeASTsail, size, lowASTsail, env

def tr_part_select(ACL2ast, env):
	"""
	`part-select` selects a portion of bits from an integer that represents a
	bitvector.  Translate as a call to get_slice_int().

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-SELECT
	"""
	# Get keyword values
	lowAST = _getValueOfKeyword(ACL2ast, ':low')
	hiAST  = _getValueOfKeyword(ACL2ast, ':high')
	widAST = _getValueOfKeyword(ACL2ast, ':width')

	# Convert keywords to a size and start index
	(sizeASTsail, size, lowASTsail, env) = _parts_helper(lowAST, hiAST, widAST, env)

	# Translate the target of the part_select
	(target, _, _, _, _) = _filterExtract(ACL2ast, 6, [None] * 5, [None] * 5, "`part_select`")
	(targetSail, env, _) = transform.transformACL2asttoSail(target, env)

	retType = Sail_t_bits(size)
	targetType = targetSail[0].getType()

	# Create the slice function application
	if isinstance(targetType, Sail_t_bits):
		sailTerm = SailApp(fn = SailHandwrittenFn(
								name = 'slice',
								typ = Sail_t_fn([targetType, Sail_t_int(), Sail_t_int()], retType)),
						actuals = [targetSail[0], lowASTsail[0], sizeASTsail])
	else:
		sailTerm = SailApp(fn = SailHandwrittenFn(
								name = 'get_slice_int',
								typ = Sail_t_fn([Sail_t_int(), targetType, Sail_t_int()], retType)),
						actuals = [sizeASTsail, targetSail[0], lowASTsail[0]])

	return [sailTerm], env, len(ACL2ast)

def tr_part_install(ACL2ast, env):
	"""
	`part-install` sets a portion of bits of an integer to some value.
	Translate as a call to the handwritten function `changeBits` which calls
	set_slice_int() and get_slice_int() under the hood.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-INSTALL
	"""
	# Get keyword values
	lowAST = _getValueOfKeyword(ACL2ast, ':low')
	hiAST = _getValueOfKeyword(ACL2ast, ':high')
	widAST = _getValueOfKeyword(ACL2ast, ':width')

	# Convert keywords to a size and start index
	(sizeASTsail, width, lowASTsail, env) = _parts_helper(lowAST, hiAST, widAST, env)

	# Translate the target and value of the part_install
	(val, x, _, _, _, _) = _filterExtract(ACL2ast, 7, [None] * 6, [None] * 6, "`part_install`")
	(valSail, env, _) = transform.transformACL2asttoSail(val, env)
	(xSail, env, _) = transform.transformACL2asttoSail(x, env)

	srcType = xSail[0].getType()
	low = lowASTsail[0].getNum() if isinstance(lowASTsail[0], SailNumLit) else None

	if isinstance(srcType, Sail_t_bits) and width is not None:
		cvalSail = coerceExpr(valSail[0], Sail_t_bits(width))
		if cvalSail is None:
			sys.exit(f"Error: failed to coerce {valSail[0].pp()} to bits({width}) in part_install expression")
		valSail = [cvalSail]
		if srcType.length is None: print("Warning: srcType.length is None!")
		if low is None: print("Warning: low is None!")
		if srcType.length is not None and low is not None and srcType.length < low + width:
			cxSail = coerceExpr(xSail[0], Sail_t_bits(low + width))
			if cxSail is None:
				sys.exit(f"Error: failed to coerce {xSail[0].pp()} to bits({low + width}) in part_install expression")
			xSail = [cxSail]
	else:
		valSail = [coerceExpr(valSail[0], Sail_t_int())]
		xSail = [coerceExpr(xSail[0], Sail_t_int())]
		if valSail[0] is None or xSail[0] is None:
			sys.exit(f"Error: failed to coerce operands to integer in part_install expression")

	# Create the setting function application
	sailTerm = SailApp(
		fn=SailHandwrittenFn(
			name='changeSlice',
			typ=Sail_t_fn([xSail[0].getType(), Sail_t_int(), Sail_t_int(), valSail[0].getType()], xSail[0].getType())),
		actuals=[xSail[0], lowASTsail[0], sizeASTsail, valSail[0]])

	return [sailTerm], env, len(ACL2ast)

def _bstar_helper(bindersRemaining, results, env):
	"""
	Along with `define`, `b*` forms are another class of complex Lisp
	expressions.

	General form is:

		(b* <list-of-bindings> . <list-of-result-forms)

	This function translates the list-of-bindings (`bindersRemaining`) and,
	as a base case `results`.  A binder has the form:

		(<binder-form> [<expression>])

	The binder-form tells us how to translate each binding.  Implemented
	forms are:

	 -  ((mv a b ...) <expr>)
	 -  (x <expr>) - let*-like binding
	 -  (- <expr>) - no binding (implementation does not evaluate `body`)
	 -  ((the <type-spec> x) <expr>)
	 -  ((when x) <expr>)
	 -  ((if x) <expr>)
	 -  ((unless x) <expr>)
	 -  Ignoring of variable `&`
	 -  Handling of variables starting with '?'

	When implementing a new binder, it should register any names it binds,
	in order, by appending them to the `boundNames` list - the names in this
	list are then de-registered from the environment when the stack unwinds
	and the function returns.

	Args:
		bindersRemaining : [[ACL2astElem] | ACL2astElem] - assume filtered
		results : [ACL2astElem] | ACL2astElem
		env : Env
	Returns:
		- ([ACL2astElem], env')
	"""
	# ===== Base case: no binders remaining, translate the result ===== #
	if len(bindersRemaining) == 0:
		(resultsSail, env, _) = transform.transformACL2asttoSail(results, env)
		return resultsSail, env

	# ===== Helper function to handle leading `!` and `?!` from names ===== #
	def sanitiseBstarName(name):
		# See 'Side Effects and Ignoring Variables' section here:
		# http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____B_A2
		if name == '-':
			# TODO: really ought to fix this as there may be side effects
			print(f"Warning: '-' binder for b* not implemented - {bindersRemaining}")
		if name == '&':
			# TODO: we really want to use SailUnderScoreLit somehow, but this will do for now
			name = '_'
		if name.startswith('?!'):
			sys.exit(f"Error: bstar name starting with `?!` encountered - {name}")
		if name.startswith('?'):
			# There is at least one instance of a binder starting with '?' which is used
			# in the subsequent body (`?flg0` in `effective-address-computations` in
			# `decoding-and-spec-utils.lisp`).
			return name[1:]
		return name

	# ===== Recursive case: match on the first binding ===== #
	# Extract the head element
	binding = bindersRemaining[0]

	# Extract the first element binder
	b = binding[0]

	# Set up a list of boundNames to be de-registered when we return on
	# the way back up the recursive call
	boundNames = [] # [str]

	# b may be a symbol (e.g. '-') or a list - let*
	if isinstance(b, str):
		b = sanitiseBstarName(b)

		# Translate the body first, find its type, then register name with env and boundNames
		forcedType = config_patterns.forced_variable_types.get(b.lower())
		if isinstance(forcedType, Sail_t_bool):
			env.setCurrentType(SailPlaceholderNil.BOOL)
		(exprSail, env, _) = transform.transformACL2asttoSail(binding[1], env)
		env.clearCurrentType()
		if len(exprSail) != 1: sys.exit(f"Error: body length not 1 in `let*` in `b*` - {exprSail}")
		exprType = exprSail[0].getType() if forcedType is None else forcedType
		bv = env.pushToBindings([b], [exprType])
		boundNames.append(b)

		# Recurse on rest of list
		(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)

		# Create sail term
		toReturn = SailLet(
						bv[0],
						exprSail,
						recursedSail
					)

	# b must be a list here.
	elif isinstance(b, list):
		# Test its first value to see what kind of binder it is.
		bindType = b[0]

		# `mv` binder
		if bindType.lower() == 'mv':
			# Translate the body before pushing new bindings as the old bindings may be used in the body
			body = binding[1]
			(bodySail, env, _) = transform.transformACL2asttoSail(body, env)
			bodyTypes = bodySail[0].getType().getSubTypes() if isinstance(bodySail[0].getType(), Sail_t_tuple) else []

			# Perform the bindings
			afters = [] # [(name:str, the_expr : ACL2astElem)]
			boundVars = []
			# Each name we bind may either be a raw symbol (easy) or a list (e.g. `the` - harder)
			for i in range(len(b[1:])):
				ident = b[i + 1]
				if type(ident) == str:
					name = sanitiseBstarName(ident)
					typ = [bodyTypes[i]] if i < len(bodyTypes) else None
					boundVars.extend(env.pushToBindings([name], typ))
					boundNames.append(name)
				elif type(ident) == list and ident[0].lower() == 'the':
					# This is a bit of a hack as we can have more general patterns in an mv b* binder
					name = sanitiseBstarName(ident[2])
					ident[2] = name # tr_the needs to be able to look up the sanitised symbol
					typ = [bodyTypes[i]] if i < len(bodyTypes) else None
					boundVars.extend(env.pushToBindings([name], typ))
					boundNames.append(name)
					(sailThe, env, _) = tr_the(ident, env)
					afters.append((name, sailThe[0]))
				else:
					sys.exit(f"Unknown type of mv binder - {ident}")

			# Get type information from the function call
			bodyType = bodySail[0].getType().getSubTypes() if isinstance(bodySail[0].getType(), Sail_t_tuple) else []
			for (i, t) in enumerate(bodyType):
				try:
					boundVars[i].getType()
				except:
					# TODO: make exception more specific
					boundVars[i].setType(t)

			# Sometimes there is structure within the mv (e.g. a `the`
			# expression).  We need to insert such type checks between this
			# binding and the next binding (or the result form).  This
			# function help us do that.
			def afters_helper(afters, env):
				# Failure case - no items
				if afters == []: sys.exit("Error: no items in afters")

				# Base case - single item
				if len(afters) == 1:
					(name, sailThe) = afters[0]
					bv = env.pushToBindings([name], [sailThe.getType()])
					bv = bv[0]
					(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)
					returnTerm = [SailLet(
						varName=bv,
						expr=[sailThe],
						body=recursedSail
					)]

					return returnTerm, [name], env

				# Recursive case
				else:
					(name, sailThe) = afters[0]
					bv = env.pushToBindings([name], [sailThe.getType()])
					bv = bv[0]
					tail = afters[1:]
					(recursedTerm, recursedNames, env) = afters_helper(tail, env)

					returnTerm = [SailLet(
						varName=bv,
						expr=[sailThe],
						body=recursedTerm
					)]

					return returnTerm, [name] + recursedNames, env

			if config_files.translate_the and afters != []:
				(recursedSail, recursedNames, env) = afters_helper(afters, env)
				boundNames.extend(recursedNames)
			else:
				# Recurse on rest of the list
				(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)

			# Create Sail term
			toReturn = SailLet(
				varName=SailTuple(subItems=boundVars),
				expr=bodySail,
				body=recursedSail,
			)

		# `the` binder
		elif bindType.lower() == 'the':
			# Extract typeSpec and name
			(typeSpec, name) = _filterExtract(b, 3, [[list], [str]], [None, None], "`the`")
			name = sanitiseBstarName(name)

			# Translate the body first (using the old environment, in case we are about to re-bind an existing variable)
			body = filterAST(binding[1])
			(bodySail, env, _) = transform.transformACL2asttoSail(body, env)

			# Encapsulate in correct `the`
			theType = translateType(env, typeSpec[0], typeSpec[1:])
			bodySail = _the_helper(theType, bodySail)

			# Determine the actual type (which, in particular in
			# the case of the_range, might be inferred to be more
			# precise, e.g. {|1, 2, 4|} instead of range(1, 4)
			actualType = bodySail[0].getType()

			# Register name with env and boundNames,
			bv = env.pushToBindings([name], [actualType])
			boundNames.append(name)

			# Recurse on the rest of the list
			(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)

			# Create Sail term
			toReturn = SailLet(
							varName = bv[0],
							expr = bodySail,
							body = recursedSail
						)
		# `when`, `if` and `unless` binders
		elif bindType.lower() in ['when', 'if', 'unless']:
			# Extract and translate the conditional expression
			cond, = _filterExtract(b, 2, [[list, str]], [None], "`when`")
			condSail, env, _ = transform.transformACL2asttoSail(cond, env)

			# Negate the condition if we are an 'unless'
			if bindType.lower() == 'unless':
				condSail = SailApp(
					fn=SailHandwrittenFn(
						name='not_bool',
						typ=Sail_t_fn([Sail_t_bool()], Sail_t_bool())),
					actuals=condSail
				)
				condSail = [condSail]

			# Translate the thing to return if we exit early.  No extra binding takes place
			bodySail, env, _ = transform.transformACL2asttoSail(binding[1], env)

			# Recurse on the rest of the list
			recusedSail, env = _bstar_helper(bindersRemaining[1:], results, env)

			# Create Sail term
			# E.g. `if [cond] then [body] else [recursed]`
			toReturn = SailIf(
				ifTerm=condSail,
				thenTerm=bodySail,
				elseTerm=recusedSail)
		# Something else
		else:
			sys.exit(f"Error : Unrecognised binder in b* - {b} in \n{bindersRemaining}")

	# b not a symbol or list: error
	else:
		sys.exit(f"Error: b* binder not a symbol or list - {bindersRemaining}")


	# De-registered names bound in this call from env
	env.popWithCheck(boundNames)

	# Return
	return [toReturn], env

def tr_bstar(ACL2ast, env):
	"""
	We could use the macro expander to expand the b* macro, but this gives us
	some really low-level garbage which, when translated to Sail, would likely
	be pretty horrible.

	Most of the work in translating b* is done in _bstar_helper - this function
	is just a wrapper.  The result is a series of nested `let` bindings with
	the translated Sail form at the bottom.

	See here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____B_A2
	"""
	_printLine()

	# Filter and extract
	(binders, results) = _filterExtract(ACL2ast, 3, [[list], None], [None, None], '`b*`')

	# If result is a quote, we don't want to deal with it
	if type(results) in [ACL2quote, ACL2qq]:
		return [None], env, len(ACL2ast)

	# Use helper
	(toReturn, env) = _bstar_helper(binders, results, env)

	# Return
	return toReturn, env, len(ACL2ast)


def tr_mv(ACL2ast, env):
	"""
	Functions can return multiple values by using the `mv` expression.
	This translates as a tuple in Sail.

	Errors in the model are often reported by lists which begin with a short
	string descriptor followed by some optional data.  These are problematic
	in Sail because an empty error is represented by `nil`, which automatically
	translates as `false`.  In reality, we want error strings to translate as
	type `option(string)` and empty errors as `none()`.  A heuristic in this
	translation function implements this.

	We interpret an `mv` as reporting an error, and thus translate its first
	item as type `option(string)` when the first item or only item is either:
	 1. a string or
	 2. already an `option(string)`.
	"""
	# Extract things to return and translate them one by one
	args = ACL2ast[1:]
	sailArgs = []
	for a in args:
		(sa, env, _) = transform.transformACL2asttoSail(a, env)
		if len(sa) > 1: sys.exit(f"Error: length of argument to mv not 1 - {sa}")
		sailArgs.extend(sa)

	# Hack: interpret error lists
	# Option 1: the error list is actually a list
	if isinstance(sailArgs[0], SailTuple):
		errorString = sailArgs[0].getItems()[0]
		if isinstance(errorString.getType(), Sail_t_string):
			sailArgs = [someHelper(errorString)] + sailArgs[1:]
		if isinstance(errorString.getType(), Sail_t_option) and isinstance(errorString.getType().getTyp(), Sail_t_string):
			sailArgs = [errorString] + sailArgs[1:]
	# The error 'list' is actually single item
	if isinstance(sailArgs[0], SailStringLit):
		sailArgs = [someHelper(sailArgs[0])] + sailArgs[1:]

	return [SailTuple(sailArgs)], env, len(ACL2ast)

def tr_case(ACL2ast, env):
	"""
	An ACL2 case expression translates to a Sail `match` expression.
	Translate the ACL2 form `otherwise` to the underscore in Sail.
	"""
	# Split into constituent parts
	var = filterAST(ACL2ast[1], comments=True)
	cases = ACL2ast[2:]

	# Translate the var expression and perform rudimentary checks
	(varSail, env, _) = transform.transformACL2asttoSail(var, env)
	if len(varSail) != 1: sys.exit(f"Error: incorrect length of variable to match on in `case` - {varSail}")
	varSail = varSail[0]

	# The `cases` list is a list of (pattern, expr) pairs.  However, some
	# patterns may themselves be lists.  This indicates that if the variable
	# matches *any* of the items in that list, the expression is evaluated.
	# Here, we explode such lists so we can translate to Sail.
	originalCases = cases
	cases = []
	for (p, e) in originalCases:
		if isinstance(p, list):
			cases.extend([(pi, e) for pi in p])
		else:
			cases.append((p, e))

	# For each of the case statements...
	matches = [] # [(pattern : SailNumLit, expr : SailASTelems)]
	for case in cases:
		# ... extract pattern and expression
		if len(case) != 2: sys.exit(f"Error: case statement length incorrect - {ACL2ast}")
		pattern = case[0]
		expr = case[1]

		# Translate the pattern and check it's a literal
		if pattern.lower() in ['otherwise', 't']:
			patternSail = SailUnderScoreLit()
		else:
			(patternSail, env, _) = transform.transformACL2asttoSail(pattern, env)
			if len(patternSail) != 1: sys.exit(f"Error: incorrect length of `case` pattern - {ACL2ast}")
			if type(patternSail[0]) not in [SailNumLit, SailStringLit]: sys.exit(f"Error: `case` pattern not a literal - {patternSail[0]}")
			patternSail = patternSail[0]

		# Translate the expr and check its length
		(exprSail, env, _) = transform.transformACL2asttoSail(expr, env)
		if len(exprSail) != 1: sys.exit(f"Error: incorrect length of `case` expression - {ACL2ast}")
		exprSail = exprSail[0]

		# Add to the list of matches
		matches.append((patternSail, exprSail))

	# Construct the return Sail AST
	toReturn = SailMatch(varSail, matches)

	return [toReturn], env, len(ACL2ast)

def tr_def_inst(ACL2ast, env):
	"""
	`def-inst` is really a macro and is specific to the x86 model.  This
	function expands the macro using the running ACL2 instance but then picks
	important parts of the expansion before deferring to tr_define for the
	main translation.
	"""
	# Construct the term to send for evaluation
	toSend = [':trans', ACL2ast]

	# Send to the ACL2server for evaluation
	newAST = env.evalACL2(toSend, debracket=True)

	# Remove top level newlines from the result
	newAST = filterAST(newAST)

	# Result is of the form 
	# `if _ then (ACL2::MAKE-EVENT-FN '(STD::DEFINE-FN ... )) else _`
	# We are interested in the `define-fn` form, so extract as appropriate
	newAST = filterAST(newAST[0]) # `if ... `
	newAST = filterAST(newAST[2]) # 'make-event ...'
	newAST = newAST[1] # quote define-fn

	# `define-fn` has the form `define-fn name args world` - we ignore the
	# world here, just passing `name` and `args` to our tr_define,
	# effectively
	newAST = filterAST(newAST.getAST())
	name = filterAST(newAST[1].getAST())
	args_body = filterAST(newAST[2].getAST())

	# Call tr_define
	(toReturn, env, _) = tr_define(['define'] + [name] + args_body, env)

	return toReturn, env, len(ACL2ast)

def tr_xr(ACL2ast, env):
	"""
	XR is the register accessor function (rw is the updater).
	Format is (xr fld index x86)
		- fld is what we switch on
		- index is used by some fields
		- we ignore the x86 object because it is represented by global state
		  in Sail.

	The lists `implementedWithoutIndex` and `implementedWithIndex` specify
	which values of `fld` use the `index` parameter and which do not.

	See: https://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____XR
	"""
	fld = ACL2ast[1]
	index = ACL2ast[2]
	x86_dummy = SailNumLit(0)

	implementedWithoutIndex = [':rip', 'rflags']
	implementedWithIndex = [':seg-visible', ':seg-hidden-attr', ':seg-hidden-base', ':seg-hidden-limit',
							':msr', 'ctr', 'str',
							'ssr-hidden-base', 'ssr-hidden-limit']

	if fld.lower() in implementedWithoutIndex:
		returnAST = SailApp(
			fn=SailHandwrittenFn(
				name=f'{fld[1:].lower()}i',
				typ=Sail_t_fn([Sail_t_unit(), Sail_t_int()], Sail_t_int(), {'rreg'})
			),
			actuals=[x86_dummy])
	if fld.lower() in implementedWithIndex:
		(indexSail, env, _) = transform.transformACL2asttoSail(index, env)
		returnAST = SailApp(
			fn=SailHandwrittenFn(
				name=f'{fld[1:].lower()}i',
				typ=Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int(), {'escape', 'rreg'})
			),
			actuals=[indexSail[0], x86_dummy]
		)
	else:
		sys.exit(f"Error: {fld} is not listed as implemented in `tr_xr`")

	return [returnAST], env, len(ACL2ast)

def _cond_helper(sailClauses):
	"""
	Helper function for tr_cond.

	Takes a list of translated clauses of the form (<cond-expr> <then-expr>)
	and creates a nested 'if then else' structure.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=COMMON-LISP____COND
	"""
	# Base case, final element
	if len(sailClauses) == 1:
		clause = sailClauses[0]
		cond_expr = clause[0]
		then_expr = clause[1]
		# Handle the unusual case of <cond-expr> not being `t` in final clause
		if not (isinstance(cond_expr, SailBoolLit) and cond_expr.getBool()):
			print("Warning: final clause in `cond` doesn't have condition 't'")
			return SailIf(
				ifTerm=[cond_expr],
				thenTerm=[then_expr],
				elseTerm=[errorHelper("Translation error: final clause in cond was not `t` and that condition failed")]
			)
		else:
			return clause[1]

	# Recursive case
	else:
		clause = sailClauses[0]
		elseCase = _cond_helper(sailClauses[1:])

		return SailIf([clause[0]], [clause[1]], [elseCase])

def tr_cond(ACL2ast, env):
	"""
	Cond has form `(cond clause1 ... clausen)`.  Clauses can be of length 1 or
	2, but only translation of clauses of length 2 is implemented here.  In
	this case, the clause has the form `(<cond-form> <then-form>)`.  The
	contents of these clauses is translated before being placed in a nested
	'if then else' structure by _cond_helper().

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____COND
	"""
	clauses = ACL2ast[1:]
	clausesSail = []

	# Translate clauses
	for c in clauses:
		if len(c) == 1: sys.exit("Error: clause in cond length 1.  This is allowed but not yet implemented here")
		if len(c) != 2: sys.exit("Error: clause length not 2")
		cond = c[0]
		expr = c[1]

		# Translate the condition and expression
		(condSail, env, _) = transform.transformACL2asttoSail(cond, env)
		condSail = condSail[0]

		(exprSail, env, _) = transform.transformACL2asttoSail(expr, env)
		exprSail = exprSail[0]

		clausesSail.append((condSail, exprSail))

	# Construct nested if then else (if...)
	toReturn = _cond_helper(clausesSail)

	return [toReturn], env, len(ACL2ast)


def _list_helper(elems, env):
	"""
	Helper for tr_list.  Translates the elements in `elems` and creates a
	nested tuples structure.
	"""
	# Failure case, no items
	if elems == []: sys.exit("No elements in list constructor")

	# Translate the first item
	head = elems[0]
	(headSail, env, _) = transform.transformACL2asttoSail(head, env)

	# Base case, one element, create a singleton tuple
	if len(elems) == 1:
		return SailTuple(subItems=headSail), env

	# Recursive case: translate the rest and cons head element
	else:
		tail = elems[1:]
		(tailSail, env) = _list_helper(tail, env)
		headSail.append(tailSail)
		return SailTuple(subItems=headSail), env


def tr_list(ACL2ast, env):
	"""
	We represent heterogeneous lists as nested tuples.  This is a wrapper
	function round _list_helper().  In general we try to avoid translating
	lists representing data.
	"""
	elems = ACL2ast[1:]
	(sailList, env) = _list_helper(elems, env)

	return [sailList], env, len(ACL2ast)

def tr_mv_let(ACL2ast, env):
	"""
	`mv-let` binds multiple variables to the result of an expression which
	returns multiple values.  Its operation is entirely encompassed by `b*`,
	so all this function does is massage into a form where we can call
	tr_bstar().

	See here for the syntax: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____MV-LET
	"""
	varNames = ACL2ast[1]
	mvExpr   = ACL2ast[2]
	body     = ACL2ast[-1]

	bstarExpr = ['b*', [[['mv'] + varNames, mvExpr]], body]

	return tr_bstar(bstarExpr, env)

def tr_mbt(ACL2ast, env):
	"""
	In `(mbt <val>)` the form `val` must to proved to be true.  We can thus
	translate as `true` (having translated `val` anyway in case it modifies
	the environment.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____MBT
	"""
	# We'd better translate the body just in case it modifies the env in some way
	(_, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)

	# Other that than, just return true
	return [SailBoolLit(True)], env, len(ACL2ast)

def tr_cons(ACL2ast, env):
	"""
	Heterogeneous data lists are translated as nested tuples.  This translates
	a cons of two items.
	"""
	(sail1, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	(sail2, env, _) = transform.transformACL2asttoSail(ACL2ast[2], env)

	return [SailTuple([sail1[0], SailTuple([sail2[0]])])], env, len(ACL2ast)


def _change_helper(keywordsOrder, changeFn):
	"""
	Creates the translation function which can be used to translate calls to
	a changer function from a `defbitstruct`.  It takes a list keyword names
	and the top level `let` expression generated by _parseBitstructFields().
	Further details can be found in the comment for _parseBitstructFields().
	"""
	_printLine()

	def innerFn(ACL2ast, env):
		# Extract parts
		change_name = ACL2ast[0]
		change_input = ACL2ast[1]
		keywords = ACL2ast[2:]

		# Translate change_input
		(sail_change_input, env, _) = transform.transformACL2asttoSail(change_input, env)

		# Check result of the input flags
		sail_change_input = filterAST(sail_change_input, comments=True)
		if len(sail_change_input) != 1: sys.exit(f"Error: length of change_input not 1 - {sail_change_input}")

		# Extract each of the keyword values and translate
		# failOnRedef asserted so each list in return dict contains exactly 1 item
		(keywordsMap, _) = _extractAllKeywords(keywords, failOnRedef=True)

		# Check all keywords are known
		# The order of items in this list matters as it corresponds to the expected
		# order of arguments in the Sail function `change_rflagsBits`
		# keywordsOrder = [':CF', ':PF', ':AF', ':ZF', ':SF', ':OF']
		for k in keywordsMap:
			if k not in keywordsOrder:
				sys.exit(f"Error: unknown keyword in {change_name} - {k}")

		# Put the keyword values in the correct order and translate
		keywordActuals = []
		for k in keywordsOrder:
			if k in keywordsMap:
				(sailKeywordVal, env, _) = transform.transformACL2asttoSail(keywordsMap[k], env)
				sailKeywordVal = filterAST(sailKeywordVal, comments=True)
				if len(sailKeywordVal) != 1: sys.exit(
					f"Error: length of a keyword value incorrect in `change_rflagsBits`")

				# Some(flag)
				keywordActuals.append(someHelper(sailKeywordVal[0]))
			else:
				# None()
				keywordActuals.append(noneHelper(Sail_t_int()))

		# Coerce actuals to expected types
		actuals = sail_change_input + keywordActuals
		coercedActuals = coerceExprs(actuals, changeFn.getType().getLHS())
		if coercedActuals is not None: actuals = coercedActuals

		# Form the final return
		sailAST = SailApp(
			fn=changeFn,
			actuals=actuals)

		# Return
		return ([sailAST], env, len(ACL2ast))

	return innerFn


def _parseBitstructFieldWidth(typeName):
	# Constant - basically copied from basic-structs.lisp
	# TODO: find this automatically
	typeWidths = {
		'bitp' : 1,
		'2bits': 2,
		'3bits': 3,
		'4bits': 4,
		'5bits': 5,
		'6bits': 6,
		'7bits': 7,
		'8bits': 8,
		'10bits': 10,
		'11bits': 11,
		'12bits': 12,
		'13bits': 13,
		'16bits': 16,
		'17bits': 17,
		'19bits': 19,
		'22bits': 22,
		'24bits': 24,
		'31bits': 31,
		'32bits': 32,
		'40bits': 40,
		'54bits': 54,
		'64bits': 64
	}

	# Parse the field
	if typeName.lower() not in typeWidths: sys.exit(f"Error: unrecognised type in defbitstruct - {type}")
	return typeWidths[typeName.lower()]

def _getBitstructWidth(fields):
	def _getFieldWidth(field):
		return _parseBitstructFieldWidth(field[1])
	return sum(map(_getFieldWidth, fields))

def _generateBitstructFieldAccessorAndUpdater(typeName, structWidth, field, currentLow, env):
	"""
	Generates the accessor and updater functions for a particualr field of a
	`defbitstruct` as described in `_parseBitstructFields()`.

	Args:
		- typeName: string - name of the bitstruct
		- structWidth: int - total width of the bitstruct
		- field: [ACL2astElem] - of form `(<field-name> <field-type>)`
		- currentLow: int - how many bits have been used so far in the bitstruct
		- env: Env
	Returns:
		(	Accessor function : SailFn,
			Updater function  : SailFn,
			New bit index     : int,
			env				  : Env
		)
	"""
	# Parse the field
	fieldName = field[0]
	fieldWidth = _parseBitstructFieldWidth(field[1])

	# Create the accessor function
	accessorInputBitsBV = SailBoundVar(binding='inputBits', typ=Sail_t_bits(structWidth))
	accessorFn =\
		SailFn(
			name=f"{typeName.lower()}_get_{fieldName.lower()}",
			formals=[accessorInputBitsBV],
			body=[
				SailApp(
					fn=SailHandwrittenFn(
						name='genericBitstructAccessor',
						typ=Sail_t_fn([Sail_t_int(), Sail_t_bits(structWidth), Sail_t_int()], Sail_t_bits(fieldWidth))
					),
					actuals=[
						SailNumLit(fieldWidth),
						accessorInputBitsBV,
						SailNumLit(currentLow)
					]
				)
			]
		)

	# Create the updater function
	updaterSpliceBitsBV = SailBoundVar(binding='spliceBits', typ=Sail_t_bits(fieldWidth))
	updaterInputBitsBV = SailBoundVar(binding='inputBits', typ=Sail_t_bits(structWidth))
	updaterFn =\
		SailFn(
			name=f"set_{typeName.lower()}_get_{fieldName.lower()}",
			formals=[updaterSpliceBitsBV, updaterInputBitsBV],
			body=[
				SailApp(
					fn=SailHandwrittenFn(
						name='genericBitstructUpdater',
						typ=Sail_t_fn([Sail_t_int(), Sail_t_bits(fieldWidth), Sail_t_int(), Sail_t_bits(structWidth)], Sail_t_bits(structWidth))
					),
					actuals=[
						SailNumLit(fieldWidth),
						updaterSpliceBitsBV,
						SailNumLit(currentLow),
						updaterInputBitsBV
					]
				)
			]
		)

	# Register with the environment
	ACL2accessor = f"{typeName}->{fieldName}".upper()
	env.addToAuto(ACL2accessor, apply_fn_gen(funcToApply=accessorFn, numOfArgs=1))
	ACL2updater = f"!{typeName}->{fieldName}".upper()
	env.addToAuto(ACL2updater, apply_fn_gen(funcToApply=updaterFn, numOfArgs=2))

	# Return the function and the new width
	return accessorFn, updaterFn, currentLow + fieldWidth, env


def _parseBitstructFields(typeName, fieldsList, currentLow, previousBoundVar, env):
	"""
	Creates the 'accessor', 'updater' and 'changer' functions for
	`defbitstruct`.

	Here is an example ACL2 bitstructure:

		(defbitstruct vex-prefixes
			((byte0  8bits "Can either be #xC4 or #xC5")
			 (byte1  8bits "Byte 1 of VEX prefixes")
			 (byte2  8bits "Relevant only for 3-byte VEX prefixes")))

	We refer to `byte0`, `byte1` and `byte2` as the 'fields' of this
	bitstructure.

	And accessor and updater function is generated for each field.  They are
	relatively simple.  An accessor takes an int representing the bitstructure
	and returns the value of a particular field, for example, in Sail:

		val vex_prefixes_get_byte0 : (int) -> int
		function vex_prefixes_get_byte0 (inputbits) =
			genericBitstructAccessor(8, inputbits, 0)

	Updators are similar but take and int and a new value and return an int
	representing the bitstructure with a certain field updated.

	A 'changer' is created for each bitstructure.  It takes a series of
	option(int) and returns thus allows multiple fields to be updated at once.
	It is implemented as a series of nested `let` expressions.  For example:

		val change_vex_prefixes : (int, option(int), option(int), option(int)) -> int
		function change_vex_prefixes (input_bits, input_byte0, input_byte1, input_byte2) =
			let output_byte0 = match input_byte0 {
				Some(input_byte0) => set_vex_prefixes_get_byte0(input_byte0, input_bits),
				_ => input_bits
			} in
			let output_byte1 = match input_byte1 {
				Some(input_byte1) => set_vex_prefixes_get_byte1(input_byte1, output_byte0),
				_ => output_byte0
			}in
			let output_byte2 = match input_byte2 {
				Some(input_byte2) => set_vex_prefixes_get_byte2(input_byte2, output_byte1),
				_ => output_byte1
			} in
			output_byte2

			Etc.

	The recursive implementation means that the bound variables input_byte0,
	input_byte1 and input_byte_2 must be returned in a list to the top level
	caller to be used in the function.  The output_byte must also be
	accessible from the next level down (hence `previousBoundVar`).

	Args:
		typeName: string - the name of the bitstruct.
		fieldsList: [ACL2astElem] - describes the widths of each element of the bitstruct.
		currentLow: int - how many bits have been used so far (i.e. sum of previous widths)
		previousBoundVar: SailBoundVar - as described above.
		env: Env

	Returns:
		(	List of accessor functions,
			List of updater functions,
			Top level `let` expression for the changer function,
			List in input SailBoundVars for the changer functions,
			env : Env
		)

	TODO:
		bitstructures would be better represented in Sail by bitvectors.
	"""
	##### Base case: no more fields
	if fieldsList == []:
		return [], [], previousBoundVar, [], env

	##### Recursive case
	# Get the accessor and updater fns for the current field
	hd = fieldsList[0]
	structWidth = _getBitstructWidth(fieldsList) + currentLow
	(accessorFn, updaterFn, newLow, env) = _generateBitstructFieldAccessorAndUpdater(typeName, structWidth, hd, currentLow, env)
	fieldWidth = newLow - currentLow

	# Recursive call.  Get the accessor and updater fns and the let expression
	# for the change_ function for the remainder of the fields
	fieldName = hd[0]
	inputBV = SailBoundVar(f"input_{fieldName}", typ=Sail_t_option(Sail_t_bits(fieldWidth)))
	outputBV = SailBoundVar(f"output_{fieldName}", typ=previousBoundVar.getType())
	(accessorFns, updaterFns, changeLet, changeOrder, env) = _parseBitstructFields(typeName, fieldsList[1:], newLow, outputBV, env)

	# Generate the current let expression for the change_ function
	extractedInput = SailBoundVar(f"input_{fieldName}", typ=Sail_t_int())
	changeLetRet = SailLet(
		varName=outputBV,
		expr=[
			SailMatch(
				var=inputBV,
				matches=[
					(extractedInput, SailApp(
										fn=updaterFn,
										actuals=[extractedInput, previousBoundVar]
									)),
					(SailUnderScoreLit(), previousBoundVar)
				]
			)
		],
		body=[changeLet]
	)

	return ([accessorFn] + accessorFns,
			[updaterFn] + updaterFns,
			changeLetRet,
			[(f":{fieldName}", inputBV)] + changeOrder,
			env)


def tr_defbitstruct(ACL2ast, env):
	"""
	Bitstructures are represented as int in ACL2 and are translated as such in
	Sail.  Calls to helper functions _parseBitstructFields() and
	_change_helper() implement most of the functionality.  Details of
	bitstructs can be found in the docstring for _parseBitstructFields().

	Overall this function generates accessor, updater and changer functions for
	bitstruct definitions and thus substantially changes the environment.

	See here for syntax: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=FTY____DEFBITSTRUCT
	"""
	_printLine()

	(_, ACL2ast) = _extractAllKeywords(ACL2ast, False)

	typeName = ACL2ast[1]
	fields_or_width = ACL2ast[2]

	if not isinstance(fields_or_width, list):
		# Width
		sys.exit(f"Error: not yet implemented fixed width defbitstruct - {ACL2ast}")
	else:
		# List of fields
		width = _getBitstructWidth(fields_or_width)
		change_input = SailBoundVar('input_bits', typ=Sail_t_bits(width))
		(accessors, updaters, changeLet, changeOrder, env) = _parseBitstructFields(typeName,
																				   fields_or_width,
																				   0,
																				   change_input,
																				   env)
		# Encapsulate the changeLet in a function and register with env
		changeFn = SailFn(
			name=f"change_{typeName}",
			formals=[change_input] + [f for (_, f) in changeOrder],
			body=[changeLet]
		)
		env.addToAuto(token=f'change-{typeName}', fn=_change_helper(keywordsOrder=[kw.upper() for (kw, _) in changeOrder],
																	changeFn=changeFn))

	return accessors + updaters + [changeFn], env, len(ACL2ast)

def errorHelper(msg):
	"""
	The ACL2 model distinguishes between:
	 -  'model state' (ms) errors, which are exceptions an actual processor
	 	might encounter such as division by zero, and
	 -  'faults', which are errors caused by the model (e.g. not implementing
	 	a certain instruction).

	Additionally, a new 'translation' error is introduced.

	Translation of errors is not particularly sophisticated.  All errors use
	the `EMsg` exception defined in handwriten2.sail.  The ACL2 model adheres
	to the informal discipline of representing errors as a list where the first
	item is a descriptive string and any remaining items are additional
	information.  This additional information is mostly discarded here - we
	only print the string message.

	Errors are translated as inline exceptions.  They are not translated as
	function calls because this would not type check.

	In the ACL2 model the idea is that, certainly after an ms error, the
	simulation could be resumed - that is not the case in the translation.

	Args:
		- msg: str
	Returns:
		`throw(Emsg(<msg>))` for use inline.

	TODO:
		Make exceptions more sophisticated.  E.g.:
		- Different exceptions for ms, fault and translation error.
		- Generated different exception types to represent
		  the additional information.
	"""
	return SailApp(
		fn=SailHandwrittenFn(
			name='throw',
			typ=Sail_t_fn([], Sail_t_error(), {'escape'}) # Sort of
		),
		actuals=[SailApp(
			fn=SailHandwrittenFn(
				name='Emsg',
				typ=Sail_t_fn([Sail_t_string()], Sail_t_string()) # Sort of
			),
			actuals=[SailStringLit(msg)]
		)]
	)


def tr_er(ACL2ast, env):
	"""
	Translate `er` as an inline exception.  See errorHelper() docstring for
	more information.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____ER
	"""
	hard_or_soft = ACL2ast[1]
	if hard_or_soft.lower() == 'soft':
		sys.exit("Error: not yet implemented soft errors")
	if not hard_or_soft.lower().startswith('hard'):
		sys.exit("Error: unrecognised error type")

	toReturn = errorHelper(f"Error thrown from function: {env.defineSlot}")

	return [toReturn], env, len(ACL2ast)


def tr_ms_fresh(ACL2ast, env):
	"""
	Translate `ms-fresh` as an inline exception.  See errorHelper() docstring
	for more information.

	Interpret the first argument as a string to be returned as an error
	"""
	(errString, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	toReturn = errorHelper(f"Model state error: {errString[0].getString()}")
	return [toReturn], env, len(ACL2ast)


def tr_fault_fresh(ACL2ast, env):
	"""
	Translate `fault-fresh` as an inline exception.  See errorHelper()
	docstring for more information.
	"""
	toReturn = errorHelper(f"A fault occurred.  Original ACL2 AST:\n{ACL2ast}")
	return [toReturn], env, len(ACL2ast)


def tr_ifix(ACL2ast, env):
	"""
	Technically `ifix x` returns x if x is an integer, otherwise 0.  We
	simply assert statically that it should be an int using `the_int` in
	handwritten.sail.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____IFIX
	"""
	(sailArg, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	toReturn = _the_helper(Sail_t_int(), sailArg)
	return toReturn, env, len(ACL2ast)


def tr_nfix(ACL2ast, env):
	"""
	As per `tr_ifix` but we use `the_nat` instead.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____NFIX

	TODO: make this function return 0 when the value is negative in line with the spec at:
	"""
	(sailArg, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	toReturn = _the_helper(Sail_t_int(), sailArg)
	return toReturn, env, len(ACL2ast)


def tr_progn(ACL2ast, env):
	"""
	`(progn event1 event2 ...)` evaluates each of the events in turn.  In our
	case, an even is most often a function definition so we simply call the
	translator recursively on this list.

	An example of `progn` in use can be foundin `top-level-memory.lisp` where
	functions including `rme08` are defined.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____PROGN
	"""
	events = ACL2ast[1:]
	(sailAST, env, _) = transform.transformACL2asttoSail(CodeTopLevel(events), env)
	return sailAST, env, len(ACL2ast)


def _parse1DArrayLiteral(array, env):
	"""
	Although Lisp is well-known for its use of (heterogeneous) lists,
	homogeneous arrays are sometimes used, especially in the ACL2 model, for
	storing 'tables' of data.  This function parses such list literals and
	produces a Sail array.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____ARRAYS
	"""
	header = array[0]
	rest = array[1:]

	### Handle the header ###
	# Dimensions (required) - hope it's a single number literal, (dim1 dim2) if 2D
	# Maximum length (required) - ignore this
	# Default (optional) - if ommitted then nil
	# Name (optional) - probably ignore
	# Order (optional) - will affect direction.  `<` is the default; `>`; `:none`/nil (no reordering by compress1)
	dimension = _getValueOfKeyword(header, ':dimensions', errorOnMoreThanOne=True)[0]
	dimension = dimension[0]
	try:
		dimension = int(dimension)
	except ValueError:
		raise ValueError(f"Error parsing array header, dimensions not an integer literal - {header}")

	sailDefault = SailPlaceholderNil()
	if _hasKeyword(header, ':default'):
		maybeDefault = _getValueOfKeyword(header, ':default', errorOnMoreThanOne=True)
		maybeDefault = maybeDefault[0]
		if maybeDefault != 'X' and maybeDefault.lower()[-3:] != 'nil':
			(sailDefault, env, _) = transform.transformACL2asttoSail(maybeDefault, env)
			sailDefault = sailDefault[0]

	asc = True
	if _hasKeyword(header, ':order'):
		maybeOrder = _getValueOfKeyword(header, ':order', errorOnMoreThanOne=True)
		maybeOrder = maybeOrder[0]
		if maybeOrder == '>':
			asc = False
		elif maybeOrder.lower() not in [':none', 'nil']:
			sys.exit(f"Error in parsing array header, unrecognised value of order - {header}")

	### Handle the rest ###
	# Form `(index . value)` or just `(index)` if value is nil.
	# The dot will register as an item from the lexer so length with be either 3 or 1
	# Use default if no entry exists for the given index.  I think this should not happen given how the contant arrays
	# are defined in the model
	# Check indices are consecutive through 0..dim-1
	sailItems = []
	if not asc:
		rest = reversed(rest)
	for (i, item) in enumerate(rest):
		index = item[0]
		if int(index) != i or i > dimension - 1:
			sys.exit("Error parsing array literal: indices not consecutive or exceeded dimension")

		if len(item) == 1:
			sailItems.append(SailPlaceholderNil())
		elif len(item) == 3:
			sailItem, env, _ = transform.transformACL2asttoSail(item[2], env)
			sailItems.append(sailItem[0])
		else:
			sys.exit(f"Error parsing array literal: incorrect item length {item}")

	### Return ###
	return dimension, sailDefault, asc, sailItems, env


def tr_aref1(ACL2ast, env):
	"""
	Retrieves an item at a given index from an array.

	General form: `(aref1 name alist index)`
	 -  `name` has no effect on the semantics of the list and we can safely
		ignore it
	 -  We assume that `alist` is a list literal.  In the model they are often
	 	represented by constants which we can expand into their literal form.

	Care must be taken to respect the correct Sail ordering (inc or dec).

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____AREF1
	"""
	alist = env.evalACL2(ACL2ast[2])[0]
	dimension, sailDefault, asc, sailItems, env = _parse1DArrayLiteral(alist, env)

	# If each item is either nil or T then replace the nils with False
	if all(isinstance(i, (SailBoolLit, SailPlaceholderNil)) for i in sailItems):
		sailItems = [SailBoolLit(False) if isinstance(i, SailPlaceholderNil) else i for i in sailItems]

	# Construct the literal
	vectorLiteral = SailVectorLit(sailItems)

	# Translate the index
	sailIndex, env, _ = transform.transformACL2asttoSail(ACL2ast[3], env)
	sailIndex = sailIndex[0]

	# Construct the projection
	vectorProject = SailVectorProject(vectorLiteral, sailIndex)

	return [vectorProject], env, len(ACL2ast)


def tr_with_output(ACL2ast, env):
	"""
	We just ignore this and evaluate its contents

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____WITH-OUTPUT

	TODO: check this is a sensible implementation
	"""
	return transform.transformACL2asttoSail(ACL2ast[-1], env)


def tr_minus(ACL2ast, env):
	"""
	The minus symbol (-) can be used in unary or binary (or n-ary) form.  This
	function handles these differences.
	"""
	if len(ACL2ast) == 2:
		sailArg, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)
		sailIntArg = coerceExpr(sailArg[0], Sail_t_int())
		if sailIntArg is None:
			sys.exit(f"Error: Could not coerce {sailArg[0].pp()} to int in negation")
		return [SailApp(
			fn=SailHandwrittenFn(
				name='negate',
				typ=Sail_t_fn([Sail_t_int()], Sail_t_int())
			),
			actuals=[sailIntArg],
		)], env, len(ACL2ast)
	else:
		return num_op_gen('-', Sail_t_int(), operandType=Sail_t_int(), numOfArgs=2)(ACL2ast, env)


def tr_plus(ACL2ast, env):
	if len(ACL2ast) == 3:
		(arg1, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
		(arg2, env, _) = transform.transformACL2asttoSail(ACL2ast[2], env)
		typ1 = arg1[0].getType()
		typ2 = arg2[0].getType()
		if isinstance(arg1[0], SailNumLit) and isinstance(typ2, Sail_t_member):
			retTyp = Sail_t_member([arg1[0].getNum() + i for i in typ2.members])
		elif isinstance(typ1, Sail_t_member) and isinstance(arg2[0], SailNumLit):
			retTyp = Sail_t_member([arg2[0].getNum() + i for i in typ1.members])
		else:
			retTyp = None

		if retTyp is None:
			return num_op_gen('+', Sail_t_int(), operandType=Sail_t_int())(ACL2ast, env)
		else:
			fn = SailHandwrittenFn(name='+', typ=Sail_t_fn([typ1, typ2], retTyp))
			return [SailApp(fn=fn, actuals=[arg1[0], arg2[0]], infix=True)], env, len(ACL2ast)
	else:
		return num_op_gen('+', Sail_t_int(), operandType=Sail_t_int())(ACL2ast, env)

def tr_pe(ACL2ast, env):
	"""
	The function `64-bit-compute-mandatory-prefix-for-two-byte-opcode` is
	generated using a make-event.  Normally, in `(make-event <form>)` we would
	simply translate what <form> evaluates to.  Here we cannot because it's
	local, so we do send the following form to the running ACL2 instance:

		:pe 64-bit-compute-mandatory-prefix-for-two-byte-opcode

	Where `:pe` stands for 'print event'.  We then search for ">V d", which
	seems to precede the printing of the event.

	This ends up defining a new function.  Unfortunately, we are currently
	translating its application!  We thus add the translated definition to
	the auxiliary file, which is then `$included`.
	"""
	# Translate the required function
	fnName = ACL2ast[0]
	response = env.evalACL2raw(f':pe {fnName}')
	response = response.split('\n')
	for (i, line) in enumerate(response):
		if line.startswith(">V d"):
			response = [line[4:]] + response[i+1:]
	response = '\n'.join(response)
	tokens = lexLispString(response)
	(ACL2astFn, finalParseIndex) = parseACL2(tokens, 0, 0)

	sailASTFn, env, _ = tr_define(ACL2astFn[0], env)
	env.addToAuxiliary(sailASTFn)

	# Finally pass the overall AST through the new function
	if isinstance(sailASTFn[0], ACL2Comment):
		sailASTpruned = sailASTFn[1:]
	else:
		sailASTpruned = sailASTFn

	if len(sailASTpruned) == 1:
		sailFn = sailASTpruned[0]
		sailast, env, _ = apply_fn_gen(sailFn, len(sailFn.getFormals()))(ACL2ast, env)
	elif len(sailASTpruned) == 2:
		sailStruct = sailASTpruned[0]
		sailFn = sailASTpruned[1]
		sailast, env, _ = apply_fn_gen(sailFn, len(sailFn.getFormals()), sailStruct)(ACL2ast, env)
	else:
		sys.exit("Error: unexpected number of items in return from tr_define")

	return sailast, env, len(ACL2ast)

def tr_member_eq(ACL2ast, env):
	"""
	In reality `(member-eq x lst)` is the longest tail of `lst` that begins
	with `x`, or nil otherwise.  Here we chose to return a boolean as it's
	mostly used in a boolean context.

	We also assume the list we are given is a list of string literals.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____MEMBER
	"""
	x = ACL2ast[1]
	xSail, env, _ = transform.transformACL2asttoSail(x, env)

	ys = ACL2ast[2]
	if not isinstance(ys, ACL2quote):
		sys.exit("Error: unexpected list type")
	ys = ys.getAST()

	isStringList, ysSail = transform.convertToStringList(ys, env)
	if not isStringList:
		sys.exit("Error: list not a string list")

	sailAST = SailApp(
		fn=SailHandwrittenFn(
			name='in_list',
			typ=Sail_t_fn([Sail_t_string(), Sail_t_list(Sail_t_string())], Sail_t_bool())
		),
		actuals=[xSail[0], SailListLit(ysSail)]
	)

	return [sailAST], env, len(ACL2ast)


def tr_t(ACL2ast, env):
	"""
	Most of the time the token `t` should translate to boolean True.
	Exceptions to this are handled in manualInterventions.py.
	"""
	return [SailBoolLit(True)], env, 1

def tr_nil(ACL2ast, env):
	"""
	The token `nil` translates to a SailPlaceholderNil object.  The actual
	value is resolved later (or in a manual intervention).  If the current
	type context has been inferred as boolean then translate as resolve to
	False immediately.
	"""
	currentType = env.getCurrentType()

	if currentType is None or currentType == SailPlaceholderNil.DEFAULT:
		return [SailPlaceholderNil()], env, 1
	elif currentType == SailPlaceholderNil.BOOL:
		return [SailPlaceholderNil(SailPlaceholderNil.BOOL)], env, 1
	else:
		sys.exit(f"Unknown current type when translating token 'nil`: {currentType}")

def tr_trunc(ACL2ast, env):
	(nBytesSail, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	nBytesSail = nBytesSail[0]
	(operandSail, env, _) = transform.transformACL2asttoSail(ACL2ast[2], env)
	operandSail = operandSail[0]

	if isRangeType(nBytesSail.getType()):
		(low, high) = getRangeOfType(nBytesSail.getType())
		nBytes = high
		isConstant = (low == high)
	else:
		sys.exit(f"Error: Could not determine number of bytes for `trunc` from argument {args[0].pp()}")
	# If the result bitvector type has variable length, coerce to the maximum possible length for now
	# TODO: Handle variable length bitvectors properly
	resultType = Sail_t_bits(8 * nBytes)

	if not isinstance(operandSail.getType(), Sail_t_bits):
		# If the operand is an integer (or anything other than a bitvector), coerce it to a bitvector
		coercedOperand = coerceExpr(operandSail, resultType)
		if coercedOperand is None:
			sys.exit(f"Error: Could not coerce operand {operandSail.pp()} for `trunc` to a bitvector")
		operandSail = coercedOperand
	operandType = operandSail.getType() if isinstance(operandSail.getType(), Sail_t_bits) else resultType

	innerFn = SailHandwrittenFn('trunc', Sail_t_fn([nBytesSail.getType(), operandType], resultType))
	innerSail = SailApp(innerFn, [nBytesSail, operandSail])

	if isConstant:
		outerSail = innerSail
	else:
		# Add the coercion from variable to constant (maximum) bitvector length
		outerFn = SailHandwrittenFn('sail_mask', Sail_t_fn([Sail_t_int(), resultType], resultType))
		outerSail = SailApp(outerFn, [SailNumLit(8 * nBytes), innerSail])

	return [outerSail], env, len(ACL2ast)

def _filterActuals(ACL2ast, numOfArgs):
	"""
	Given an ACL2 AST representing a function call, this helper function
	checks that the non-keywords arguments are of a supported type and
	returns a list of all actual parameters and also the keyword parameters.

	Args:
		ACL2ast: [ACL2astElem]
		numOfArgs: int

	Returns:
		(	Actual parameters : [ACL2astElem],
			Keyword parameters : { (keyword : str) : (value : [[ACL2astElem]]) }
		)

	"""
	# Extract the non-keyword arguments and check they are of an expected type
	ACL2actuals = ACL2ast[1:numOfArgs+1]
	for a in ACL2actuals:
		if type(a) not in [list, str, ACL2String, ACL2quote]:
			sys.exit(f"Error: unexpected type in ACL2 AST in function application: {pp_acl2(ACL2actuals)}")
		if isinstance(a, ACL2quote):
			isStringList = transform.isStringList(a.getAST())
			if not isStringList:
				sys.exit(f"Error: unexpected type in ACL2 AST in function application: {pp_acl2(ACL2actuals)}")

	# Extract keyword arguments
	(ACL2keywords, _) = _extractAllKeywords(ACL2ast[numOfArgs + 1:], failOnRedef=True)

	return ACL2actuals, ACL2keywords

def apply_dependent_fn_gen(funcToApply_gen, numOfArgs, keywordStruct=None, coerceActuals=True):
	"""
	The ACL2 model defines its own functions and macros (using, for example,
	`(define <name> <formal-args> <body>)`).  When <name> is called using
	`(<name> <actual-args>)`, we need to translate this to a function call in
	Sail.  To do this, for each generated function we must also generate a
	Python translator function which can handle it - the purpose of this.
	This function generates such functions and registers them with the
	environment so that function call can be translated.

	Specifically, it generates functions with the same type as the translation
	function above: taking an ACL2 AST and the environment and returning the
	translated Sail, the new environment and the number of tokens processed.

	Args:
		- funcToApply : function - takes actual arguments and returns SailFn | SailHandwrittenFn
		- numOfArgs : int - number of arguments `funcToApply` expects
		- keywordStruct : SailStruct - if `funcToApply` takes keyword arguments
	Returns:
		- fn : as above
	"""
	def apply_fn_inner(ACL2ast, env):
		# Filter out the newlines and comments from the actuals
		# ACL2keywords : {(keyword : str): (value :[[ACL2astElems] | ACL2astElem])}
		(ACL2actuals, ACL2keywords) = _filterActuals(ACL2ast, numOfArgs)

		# Translate the non-keyword actuals.  Env should not change but don't translate for that.
		SailActuals = []
		for (i, a) in enumerate(ACL2actuals):

			# If not, do main translation
			(aSail, env, _) = transform.transformACL2asttoSail(a, env)
			SailActuals.extend(aSail)

		# Translate keyword actuals and form the appropriate struct
		if keywordStruct is not None:
			defaults = dict(keywordStruct.getDefaults().copy())
			for (kw, ACL2val) in ACL2keywords.items():
				kw = kw[1:].lower()
				if kw not in defaults:
					sys.exit(f"{kw} not in defaults - {defaults}")
				else:
					if isinstance(keywordStruct.getTypeOfName(kw), Sail_t_bool):
						env.setCurrentType(SailPlaceholderNil.BOOL)
					(sailVal, env, _) = transform.transformACL2asttoSail(ACL2val, env)
					env.clearCurrentType()
					defaults[kw] = sailVal[0]

			defaults = list(defaults.items())
			defaultLit = SailStructLit(keywordStruct, defaults)
			SailActuals.append(defaultLit)

		funcToApply = funcToApply_gen(SailActuals, env)

		if coerceActuals:
			# Try to coerce actuals to the expected type
			for (i, arg) in enumerate(SailActuals):
				try:
					new = coerceExpr(arg, funcToApply.getType().getLHS()[i])
					if new is not None:
						SailActuals[i] = new
				except:
					continue

		# Construct the Sail AST and return the number of ACL2 AST items consumed
		return [SailApp(funcToApply, SailActuals)], env, len(ACL2ast)

	return apply_fn_inner

def apply_fn_gen(funcToApply, numOfArgs, keywordStruct=None):
	return apply_dependent_fn_gen(lambda actuals, env: funcToApply, numOfArgs, keywordStruct)

def apply_macro_gen(numOfArgs, useTrans1=True):
	"""
	Similar to `apply_fn_gen`, this function generates another function which
	can be registered with the environment and which will, when given a macro
	name, expand it using the ACL2 server and translate the resulting code.

	Args:
		- numOfArgs : int | None - unused
		- useTrans1 : bool - 	Lisp can fully expand macros using `:trans` or
								expand one level only using `:trans1`.  The
								latter tends to give better results.
	Returns:
		- fn : as above
	"""
	def apply_macro_inner(ACL2ast, env):

		# DO NOT check the number of arguments presented as it may be wrong (e.g. in the presence of `&key` keyword
		# arguments.

		# Check that we have the correct number of items if necessary
		# if numOfArgs != None and len(ACL2ast) != numOfArgs + 1:
		# 	print("Error: macro application has an incorrect number of arguments")
		# 	print(f"Form:\n{ACL2ast}")
		# 	print(f"Expected: {numOfArgs} args")
		# 	sys.exit(1)

		# Construct the term to send for evaluation
		toSend = [':trans' if not useTrans1 else ':trans1', ACL2ast]
		if config_files.print_acl2_interactions:
			print(f'Sending this to be macro expanded: {toSend}')

		# Send to the ACL2server for evaluation
		newAST = env.evalACL2(toSend, debracket=True)
		if config_files.print_acl2_interactions:
			print(f'\nReceived this in return: {newAST}')

		# Deconstruct the newAST to get the result
		newAST = newAST[0]
		if config_files.print_acl2_interactions:
			print(f'\nAs an AST: {newAST}')

		# Translate this generated ast
		(SailAST, env, _) = transform.transformACL2asttoSail(newAST, env)

		# Return
		return SailAST, env, len(ACL2ast)

	return apply_macro_inner
