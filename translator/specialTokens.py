import transform
from lex_parse import ACL2String, ACL2quote, ACL2qq, CodeTopLevel, pp_acl2, lexLispString, parseACL2
from SailASTelems import *
from SailTypes import parseGuard, Sail_t_fn, dictAddOrInit, translateType, eqSet, isNumeric
import manualTranslations
import exclusions

import os
import sys

'''
As per Env class in transform.py, each function here must have the type:

	(ACL2ast : [ACL2astElem], env : Env)
	-> (Sailast' : [SailastElem],
		env' : Env,
		consumed : int)

Where:
	* ACL2astElem ::= 	ACL2Comment
					|	ACL2String
					|	[ACL2astElem]
					|	str
					|	NewLine
	* `ACL2ast` is the ast rooted at the current symbol
	* The first element of ACL2ast is a symbol (i.e. str type)

Register each function defined here with the function at the bottom which is
returned to the environment on setup.
'''

def printLine():
	print("-"*80)

def hasKeyword(ACL2ast, keyword):
	'''
	Args:
		ACL2ast: [ACL2astElem]
		keyword: str

	Returns:
		bool
	'''
	for elem in ACL2ast:
		if isinstance(elem, str) and elem.upper() == keyword.upper():
			return True

	return False

def getValueOfKeyword(ACL2ast, keyword, errorOnMoreThanOne=False):
	'''
	Some ACL2 terms have extra information in the form of keywords: symbols
	which begin with a colon, e.g. ':guard'.  The value associated with each
	keyword is the next term along.  This function extracts the values for the
	given keywords (values plural in case the keyword appears more than once).

	Args:
		- ACL2ast : [ACL2astElem] - incuding the colon
		- keyword : str (including the colon)
	Returns:
		- [ [ACL2astElem] | ACL2astElem ]
	'''
	# Filter out newlines and comments
	ACL2ast = [item for item in ACL2ast if type(item) not in [NewLine, ACL2Comment]]

	toReturn = []
	for i in range(len(ACL2ast)):
		if isinstance(ACL2ast[i], str) and  ACL2ast[i].upper() == keyword.upper():
			toAppend = ACL2ast[i+1]
			if type(toAppend) in [ACL2Comment, NewLine]:
				sys.exit(f"Error: incorrect type of keyword value '{toAppend}' in {ACL2ast}")
			toReturn.append(toAppend)

	if errorOnMoreThanOne and len(toReturn) > 1:
		sys.exit(f"Error: more than one instance of keywork {keyword} found in {ACL2ast}")

	return toReturn

def extractAllKeywords(ACL2ast, failOnRedef):
	'''
	Args:
		- ACL2ast : [ACL2astElems]
		- failOnRedef : bool - throw exception if a keyword is used twice

	Returns:
		(
			{(keyword : str) : (value : [[ACL2astElems] | ACL2astElem])},
			[ACL2astElems] - without keywords
		)
	'''
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

	return (keywords, remainder)

def filterExtract(ACL2ast, numOfElems, subTypes, subLengths, debugName, comments=False):
	'''
	Filter the ast, extract the elements and perform checks that they are of
	the correct length and type

	Args:
		- ACL2ast : [ACL2elem]
		- numOfElems : int | None - including the first element
		- subTypes : [[type] | [None] ] - permitted types of each of the numOfElems sub items
		- subLengths : [[int] | [None]] - permitted lengths of each of the numOfElems sub items which are lists
		- comments : bool - whether to filter comments from the top level of not
	Returns:
		Depends on if numOfElems is None or not
		TODO: flesh out this description
	'''
	# Filter out newlines and comments
	ACL2astFiltered = filterAST(ACL2ast, comments=comments)

	if numOfElems == None:
		numOfElems = len(ACL2astFiltered)		
	else:
		# Check overall length
		if len(ACL2astFiltered) != numOfElems:
			sys.exit(f"Error in {debugName}: '{debugName}' expected {numOfElems} elements, got {len(ACL2astFiltered)} in:\n{ACL2astFiltered}")

		# Check parameters the other parameters to this function
		if len(subLengths) != numOfElems - 1: sys.exit(f"Error: Expected {numOfElems - 1} items in lengths formal in {debugName}, got {subLengths}")
		if len(subTypes) != numOfElems - 1: sys.exit(f"Error: Expected {numOfElems - 1} items in types formal in {debugName}, got {subTypes}")

	# Check types (and if neccessary lengths) of sub items
	for (item, length, types) in zip(ACL2astFiltered[1:], subLengths, subTypes):
		if types != None and type(item) not in types: sys.exit(f"Error: {item} not in permitted types {types} whilst decoding {debugName}")
		if length != None and isinstance(item, list) and len(item) != length: sys.exit(f"Error: {item} not of length {length} whilst decoding {debugName}")

	# Return
	return tuple(ACL2astFiltered[1:])

def filterAST(ACL2ast, comments=False):
	'''
	Filter out NewLines and optionally comments from the top level of a
	Sail AST
	'''
	if isinstance(ACL2ast, str):
		return ACL2ast

	if comments:
		return [item for item in ACL2ast if type(item) not in [NewLine, ACL2Comment]]
	else:
		return [item for item in ACL2ast if type(item) not in [NewLine]]

def _in_package_fn(ACL2ast, env):
	return ([None], env, len(ACL2ast))

def _include_book_fn(ACL2ast, env):
	printLine()

	# If a ':dir' keyword is specified, use that, otherwise just use the specified path/name
	keywordDirs = getValueOfKeyword(ACL2ast, ':dir')
	if keywordDirs == []:
		# Extract path/name of file to include
		thisPath, thisFile = os.path.split(ACL2ast[1].getString())
	elif len(keywordDirs) > 1:
		sys.exit("Error: too many :dir keywords specified")
	else:
		keywordDir = keywordDirs[0]
		if keywordDir == ':utils':
			thisPath = '../utils' # TODO: make this more correct
			thisFile = ACL2ast[1].getString()
		else:
			sys.exit(f"Error: unknow :dir - {keywordDir}")

	# Hack to avoid collision of the two segmentation files
	# TODO: handle this properly, probably by having a better file structure (with an 'instructions/' folder).
	thisFileRename = thisFile
	if thisFile.lower() == 'segmentation' and env.getFile() == 'top':
		thisFileRename = 'segmentationInst'
	if thisFile.lower() == 'top' and env.getFile() == 'top':
		thisFileRename = 'topFP'

	# The file may already have been translated
	isIncluded, ast = env.isIncluded(thisFileRename)
	print(f"Include test: {thisFileRename}, {isIncluded}, {ast}")
	if isIncluded:
		print(f"Returning from `include` of {thisFileRename} to file: {env.getFile()}")
		return [ast], env, len(ACL2ast)

	# Skip certain files
	if thisFileRename not in exclusions.exclusions_files:
		# Add the path and file to the env
		env.appendToPath(thisPath)
		env.appendToFile(thisFileRename)
		
		print(f"Generating Sail for included file: {thisFileRename}")

		# Perform the translation on the new file
		# TODO: maybe catch and ignore errors here?
		sailAST, env2, consumed = transform.transformACL2FiletoSail(f'{thisFile}.lisp', env)

		# Pop the path/file from the end
		env2.popPath()
		env2.popFile()

		# Add a Sail `include` for the file translated above
		toReturn = SailInclude(sailAST,
							 exclusions.outputFolder,
							 thisFileRename,
							 includeHeaders=False,
							 env=env)
		env.addToIncluded(thisFileRename, toReturn)
		print(f"Returning from `include` of {thisFileRename} to file: {env2.getFile()}")
		return ([toReturn], env, len(ACL2ast))
	else:
		print(f"Skipping `include`: {thisFileRename}")

		# Simply return nothing
		return ([None], env, len(ACL2ast))

def _local_fn(ACL2ast, env):
	# TODO: we probably don't want to just peek, but to search through the context stack
	ctx = env.peekContext2()
	if ctx == None:
		print("Warning: you really should include local books when in the top-level context")
		return ([None], env, len(ACL2ast))
	if ctx.lower() not in ['include-book', 'defsection', 'encapsulate']:
		# TODO: fill this in for when we do need to eval `local` events
		sys.exit(f"Error: non-implemented handling of local event in an unknown context.  Context stack: {env.getFullContext()}")
	else: # Context is 'include-book', 'defsection' or 'encapsulate'
		return ([None], env, len(ACL2ast))

def _defsection_fn(ACL2ast, env):
	# http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____DEFSECTION
	# We ignore these fields: name, parents, autodoc, extension
	# TODO: maybe make 'name' field a comment in Sail

	printLine()

	# Remove newlines and the first items ('defsection name')
	ACL2astFiltered = filterAST(ACL2ast)
	ACL2astFiltered = ACL2astFiltered[2:]

	SailAST = []

	newTopLevel = []
	index = 0
	end = False
	while not end:
		acl2Item = ACL2astFiltered[index]
		if acl2Item in [':short', ':long']:
			# Add as a comment to the translated AST
			SailAST.append(ACL2Comment(ACL2astFiltered[index+1]))
			consumed = 2
		elif acl2Item in [':parents', ':autodoc', ':extension']:
			# Ignore
			consumed = 2
		else:
			# Defer to main translator
			newTopLevel.append(acl2Item)
			consumed = 1

		index += consumed
		if index > len(ACL2astFiltered):
			print("Error: defsection transformer consumed too many tokens")
			sys.exit(1)
		end = (index == len(ACL2astFiltered))

	if len(newTopLevel) == 0:
		return ([None], env, len(ACL2ast))
	(SailTopLevel, env, _) = transform.transformACL2asttoSail(CodeTopLevel(newTopLevel), env)
	SailAST.extend(SailTopLevel)

	# If we consumed more symbols than we have, we threw an error
	# If we consumed less, we'd still be in the loop
	# Thus, by getting to here, we consumed exactly the right number
	# We actually 'consume' the number in the non-filtered list
	return (SailAST, env, len(ACL2ast))

def parseNormalFormal(f, env):
	"""
	Parses a `Formal` as per the syntax here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS

	Args:
		f: a formal

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
		if hasKeyword(f, ':type'):
			# Use ':type' keyword if it exists
			possibleTypes = getValueOfKeyword(f, ':type')
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
			if isinstance(guardWord, list):
				types = parseGuard(guardWord).get(name, None)

		# Return
		return name, types
	else:
		sys.exit("Error: item in formals not a symbol or list")

def parseKeywordFormal(f, env):
	"""
	Parses an `OptFormal` as per the syntax here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS

	Args:
		f: a formal
		env: as above

	Returns:
		(name : str,
		types : eqSet(SailType) | None,
		default: SailASTelem,
		env)
	"""
	if isinstance(f, str): # I.e. just the name
		name, typ = parseNormalFormal(f, env)
		default = SailPlaceholderNil()
	elif isinstance(f, list) and len(f) == 1: # I.e. (varname Item*) where Item can be xdoc, guard, :key val
		name, typ = parseNormalFormal(f, env)
		default = SailPlaceholderNil()
	elif isinstance(f, list) and len(f) == 2: # I.e. (Formal, 'val) - a default value is given
		if isinstance(f[1], ACL2quote): toTranslate = f[1].getAST()
		else: toTranslate = f[1]
		name, typ = parseNormalFormal(f[0], env)
		if typ is not None and isinstance(typ.resolve(), Sail_t_bool):
			env.setCurrentType(SailPlaceholderNil.BOOL)
		default, env, _ = transform.transformACL2asttoSail([toTranslate], env)
		env.clearCurrentType()
		default = default[0]
	else:
		sys.exit(f"Error: malformed keyword formal - {f}")

	return name, typ, default, env

def parseFormals(fnFormals, mode, env):
	"""

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
	# Partial implementation of syntax described here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=STD____EXTENDED-FORMALS
	### Base case: none left
	if fnFormals == []:
		return [], {}, {}, env

	### Recursive cases
	# Extract structure from list of formals
	head = fnFormals[0]
	tail = fnFormals[1:]

	# Start of keyword/optional
	if isinstance(head, str):
		if head.lower() == '&optional':
			return parseFormals(tail, 'optional', env)
		if head.lower() == '&key':
			return parseFormals(tail, 'key', env)

	# Parse a normal formal
	if mode == 'normal':
		name, types = parseNormalFormal(head, env)
		tailNames, tailTypes, tailKeyDefaults, env = parseFormals(tail, 'normal', env)
		if name in tailNames: sys.exit(f"Error: name '{name}' already parsed, mode='normal'")
		if types != None:
			tailTypes[name] = types
		return [name] + tailNames, tailTypes, tailKeyDefaults, env

	# Parse an optional formal
	elif mode == 'optional':
		sys.exit("Error: &optional arguments not yet implemented")

	# Parse a keyword formal
	elif mode == 'key':
		name, types, keyDefault, env = parseKeywordFormal(head, env)
		tailNames, tailTypes, tailKeyDefaults, env = parseFormals(tail, 'key', env)
		if name in tailNames: sys.exit(f"Error: name '{name}' already parsed, mode='key'")
		tailKeyDefaults[name] = keyDefault
		return tailNames, tailTypes, tailKeyDefaults, env

	# Otherwise error
	else:
		sys.exit(f"Error: unrecognised formal parsing mode {mode}")

	sys.exit("Reached end of parseFormals function")

def _define_fn(ACL2ast, env):
	printLine()

	# Extract extended options first as per: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____DEFINE
	extendedOpts = [] # Type: [(:name, value)]
	ACL2astRemainder = []

	index = 0
	while index < len(ACL2ast):
		item = ACL2ast[index]
		if isinstance(item, str) and item.startswith(':'):
			extendedOpts.append((item, ACL2ast[index+1]))
			index += 2
		else:
			ACL2astRemainder.append(item)
			index += 1

	# Setup the return value and remaining ACL2 AST
	SailAST = []

	# Create a dummy SailFn object with no content 
	thisSailFn = SailFn()

	# Deal with comments in the extended options
	# Ignore: parents, inline, no-function
	# We deal with typeing (and hence the ':guard' option) later
	for (name, value) in extendedOpts:
		if name in [':long', ':short']:
			SailAST.append(ACL2Comment(value))

	# Split the rest into pre- and post-///
	# We use the content before ///, but ignore the stuff after
	if '///' in ACL2astRemainder:
		slashesIndex = ACL2astRemainder.index('///')
		preSlashes = ACL2astRemainder[:slashesIndex]
		postSlashes = ACL2astRemainder[slashesIndex+1:]
	else:
		preSlashes = ACL2astRemainder
		postSlashes = []

	# Split pre-/// into component parts
	# assert preSlashes[0] == 'define'
	fnName = preSlashes[1]
	fnFormals = preSlashes[2]
	fnRest = preSlashes[3:-1]
	fnBody = preSlashes[-1]

	print(f"Name: {fnName}")
	print(f"Formals: {fnFormals}")
	print(f"Rest: {fnRest}")
	print(f"Body: {fnBody}")

	env.setDefineSlot(fnName)

	# If the function body is a quote or quasi quote, do not proceed - we hope
	# it will be used in a useful way later
	if type(fnBody) in [ACL2quote, ACL2qq]:
		print("WARNING: function body is a quote/QQ, ignoring it.")
		return ([None], env, len(ACL2ast))

	# Set the type if we've specified it manually
	if fnName.lower() in exclusions.forced_return_types:
		thisSailFn.setForceRHSType(exclusions.forced_return_types[fnName.lower()])

	# Perform rudimentary checks
	if not isinstance(fnName, str): sys.exit("Error: function name not a symbol")
	if not isinstance(fnFormals, list): sys.exit("Error: function formals not a list")
	for f in fnFormals:
		if type(f) not in [str, list]: sys.exit(f"Error: item in formals not a symbol or list, found: {type(f)}")
	if not isinstance(fnBody, list): sys.exit("Error: function body not a list")

	# === Translate the formals and create associated struct/struct function if needed.  Mutate the formals arguments
	# to take account of the new struct
	# 		(fnFormalsFiltered : [str],
	# 		fnFormalsTyped : {str: eqSet(SailType},
	# 		keyDefaults : {str : SailASTelem},
	fnFormalsFiltered, fnFormalsTyped, keyDefaults, env = parseFormals(fnFormals, 'normal', env)
	numNonKeywordFormals = len(fnFormalsFiltered)

	# === Translate the body
	# Add the formal parameter bindings to the stack with their types if we have them
	fnFormalsBVs = []
	for f in fnFormalsFiltered:
		if f in fnFormalsTyped:
			generalisedType = fnFormalsTyped[f].resolve()
			bv = env.pushToBindings([f], [generalisedType])
		else:
			bv = env.pushToBindings([f], [Sail_t_unknown()])
		fnFormalsBVs.append(bv[0])
	# Deal with keyword arguments
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
			# TODO: pushToBindings can take lists - make this for loop into list comprehensions
			env.pushToBindings(tokens=[kd], customSail=[SailStructProject(keywordBV, kd)])
			keywordsToPop.append(kd)
	# Add the name and formals to the dummy SailFn and register in the global
	# environment in order to allow recursion
	thisSailFn.setName(fnName)

	thisSailFn.setFormals(fnFormalsBVs)
	# thisSailFn.setFormals(fnFormalsFiltered)

	env.addToGlobal(fnName, apply_fn_gen(thisSailFn, numNonKeywordFormals, struct))
	# Evaluate the body - no events should take place but use the returned environment anyway
	# TODO: add an equality check to Env to make sure passed and returned environments are the same.  This may be hard because the Env references will actualy point to the same object, so a copy might need to be taken
	# TODO: check this recursive call does actually consumer all of the function definition (i.e. are all comments consumed?)
	(SailItem, env, _) = transform.transformACL2asttoSail(fnBody, env)
	# Ammend the SailFn object to contain the body definition and add to the AST
	thisSailFn.setBody(SailItem)
	SailAST.append(thisSailFn)
	# Pop the formal parameter bindings
	env.popWithCheck(keywordsToPop)
	env.popWithCheck(fnFormalsFiltered)
	# Return early if no Sail ast was produced
	if SailItem == [None]:
		return ([None], env, len(ACL2ast))

	# Ignore the post-slashes
	pass

	# =========================================================================
	# Deal with typing information.  There are various places we can gain this
	# information
	# =========================================================================

	# Knowledge of the input type is gained from:
	# - guards
	# - the functions which use them
	# - ':type' annotations in the formal parameters
	# TODO: implement formal type info extraction by looking at the functions
	# which use them

	# Set up a list of types for the input type
	inputType = []

	# 1) Examine the guards to get some information
	guards = [item[1] for item in extendedOpts if item[0] == ':guard']
	if len(guards) == 0:
		guardInfo = {}
	elif len(guards) == 1:
		# Get the information from the guards
		guardInfo = parseGuard(guards[0])
		print(f"Guard info: {guardInfo}")
	else:
		sys.exit(f"Error: more than one guard statement: {guards}")

	# 2) Examine where formals are used to get some information
	print("WARNING: still need to implement looking at the function which use the formals")

	# 3) Examine ':type' annotations to get some information.
	# We already did the extraction of this information above
	print(fnFormalsTyped)

	# 4) Try to square the above information, and hope that it gives us exactly
	# one solution
	for name in fnFormalsFiltered:
		# Gather the possibilities
		possibilities = eqSet()
		if name in guardInfo:
			possibilities.extend(guardInfo[name])
		if name in fnFormalsTyped:
			possibilities.extend(fnFormalsTyped[name])

		# Check if the possibilities are all numeric
		if possibilities.all(isNumeric):
			inputType.append(Sail_t_int())
		else:
			if len(possibilities) != 1:
				sys.exit(f"Error: non-numeric formal and multiple possibilities for it's type: {possibilities}")
			inputType.append(possibilities.peek())

		# Do something with them
		# if len(possibilities) != 1:
		# 	print(possibilities)
		# 	sys.exit(f"Too many type possibilities for name '{name}'")
		# else:
		# 	typeObject = possibilities.peek()
		# 	inputType.append(typeObject)

	# # The return type is the type of the top-level function call within this
	# # function or may be forced manually
	# if fnName in exclusions.forced_return_types:
	# 	outputType = exclusions.forced_return_types[fnName]
	# else:
	# 	sailBody = filterAST(thisSailFn.getBody(), comments=True)
	# 	if len(sailBody) != 1:
	# 		sys.exit(f"Error: couldn't find top level expression in {sailBody}")
	# 	outputType = sailBody[0].getType()
	#
	# # Add the type to the SailFn object
	# thisSailFn.setType(Sail_t_fn(inputType, outputType))

	# Return
	return (SailAST, env, len(ACL2ast))

def _make_event_fn(ACL2ast, env):
	'''
	From here: http://www.cs.utexas.edu/users/moore/acl2/v6-3/MAKE-EVENT.html

			'The expression (make-event form) replaces itself with the result
			of evaluating form, say, ev, as though one had submitted ev
			instead of the make-event call.'
	'''
	printLine()

	# Filter out newlines and extract `form` from `(make-event form)`
	ACL2astFiltered = filterAST(ACL2ast)
	form = ACL2astFiltered[1]

	# Send form to the ACL2 server for evaluation
	newAST = env.evalACL2(form)

	# Translate this generated ast
	(SailAST, env, _) = transform.transformACL2asttoSail(newAST[0], env)

	# Return
	return (SailAST, env, len(ACL2ast))

def _defmacro_fn(ACL2ast, env):
	"""
	We do not attempt to translate macro bodies, instead expanding macros
	at their use site by querying the ACL2 server.  Do do this, however, we
	do need to record the name and number of arguments each macro takes.

	General Form:
	(defmacro name macro-args doc-string dcl ... dcl body)

	Fortunately, we only concern ourselves with the first three items.

	The calculation for numOfFormals is incorrect: we may have complicating factors such as `&key` keyword arguments.
	This does not matter as apply_macro_gen no longer checks this value.
	"""
	printLine()

	# Filter out newlines
	ACL2astFiltered = filterAST(ACL2ast)

	# Extract the name and formals
	macroName = ACL2astFiltered[1]
	macroFormals = ACL2astFiltered[2]

	# Perform rudimentary checks
	if not isinstance(macroName, str): sys.exit("Error: macro name not a symbol")
	if not isinstance(macroFormals, list): sys.exit("Error: macro formals not a list")

	# Add a function to the global environment which will deal with this macro
	# if it appears in the rest of the code
	numOfArgs = len(macroFormals)
	env.addToGlobal(macroName, apply_macro_gen(numOfArgs))

	# Return
	return ([None], env, len(ACL2ast))

def _add_macro_alias_fn(ACL2ast, env):
	'''
	`add-macro-alias` is used only four time throughout the codebase, all in
	the `rflags-spec` file, and these uses appear extraneous.  Thus, we ignore
	it for now
	'''
	return ([None], env, 3)

def _mbe_fn(ACL2ast, env):
	'''
	Selects either the logic or exec part of the mbs (curently the :logic part)

	TODO: make this switch a global parameter
	'''
	printLine()

	# Filter out newlines and comments
	ACL2astFiltered = filterAST(ACL2ast, comments=True)

	# Check we have the right number of elements in the mbe
	if len(ACL2astFiltered) != 5:
		print(f"Error: unexpected number of items in `mbe` ast: {len(ACL2astFiltered)} items")
		print(f"AST:\n{ACL2astFiltered}")
		sys.exit(1)

	# Extract elements
	logicCode = getValueOfKeyword(ACL2astFiltered, ':logic')
	execCode  = getValueOfKeyword(ACL2astFiltered, ':exec')

	# Perform rudimentary checks
	if len(logicCode) != 1: sys.exit(f'Error: wrong number of :logic keywords in: {ACL2astFiltered}')
	if len(execCode)  != 1: sys.exit(f'Error: wrong number of :exec keywords in: {ACL2astFiltered}')
	if not isinstance(logicCode[0], list): sys.exit(f"Error: Unexpected MBE :logic code type, expected list, got: {type(logicCode[0])}")
	if type(execCode[0]) not in [list, str]: sys.exit(f"Error: Unexpected MBE :exec code type, expected list or string, got: {type(execCode[0])}")

	# Select the :LOGIC segment
	(SailAST, env, consumed) = transform.transformACL2asttoSail(logicCode[0], env)

	# Return
	return (SailAST, env, len(ACL2ast))

def _defthm_fn(ACL2ast, env):
	'''
	We skip defthms because there is no concept of them in Sail

	NOTE: also used as a general ignoring function

	TODO: OTOH, they may contain useful typing information
	'''
	return ([None], env, len(ACL2ast)) # So much information... just... gone

def _if_fn(ACL2ast, env):
	'''
	Translates an `if` statement
	'''
	printLine()

	# Check we have the right number of elements
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
	# - Also optimise for `true` constant
	# - If we have just `if _ t nil` then force t and nil to both be bools (don't just return the predicate as it
	#	might not be a bool.
	# TODO: should probably check that the environment does not change on each translation check consumed values
	# TODO: for the app-view hack, maybe make the `elseTerm` a failure instead (i.e. `assert(false)`).
	if (isinstance(ifTerm, list) and ifTerm[0].lower() == 'app-view') or\
			(isinstance(ifTerm, list) and isinstance(ifTerm[0], SailBoolLit) and ifTerm[0].getBool()):
		(thenTermSail, env, _) = transform.transformACL2asttoSail(thenTerm, env)
		toReturn = thenTermSail
	elif isinstance(thenTerm, str) and thenTerm.lower() == 't' and isinstance(elseTerm, str) and elseTerm.lower() == 'nil':
		(ifTermSail, env, _) = transform.transformACL2asttoSail(ifTerm, env)
		toReturn = [SailIf(ifTermSail, [SailBoolLit(True)], [SailBoolLit(False)])]
	else:
		(ifTermSail, env, _) = transform.transformACL2asttoSail(ifTerm, env)
		(thenTermSail, env, _) = transform.transformACL2asttoSail(thenTerm, env)
		(elseTermSail, env, _) = transform.transformACL2asttoSail(elseTerm, env)
		toReturn = [SailIf(ifTermSail, thenTermSail, elseTermSail)]

	# Construct the Sail ast `if` element and return
	return (toReturn, env, len(ACL2ast))

def _encapsulate_fn(ACL2ast, env):
	'''
	See here for detailed info: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____ENCAPSULATE

	Encapsulates come in two forms, indicated by the first symbol after `encapsulate`:
	1) First symbol is nil: a 'trival' encapsulate
	2) Second symbol is non-nul: a 'non-trivial' encapsulate

	Here we implement only the former.  There is only a single instance of the latter, and I suspect
	we wil want to deal with it manually as it's to do with undefined behaviour.
	'''
	printLine()

	# Filter and extract elements
	filtered = filterExtract(ACL2ast, None, [[list]], [None], f"`encapsulate`", comments=True)
	trivialp = filtered[0]
	body = filtered[1:]

	# Perform rudimentary checks
	if trivialp != []: print("WARNING: non-trivial encapsulate not implemented, treating as trivial")

	# For each further element, just translate it - `_local_fn` handles whether it should be included or not
	toReturn = []
	for acl2Term in body:
		(sailTerm, env, consumed) = transform.transformACL2asttoSail(acl2Term, env)
		if consumed != len(acl2Term): print(f"WARNING: not all of an encapsulate event was translated.  Item:\n{acl2Term}")
		toReturn.extend(sailTerm)

	# Return
	return (toReturn, env, len(ACL2ast))

def _general_equal_fn(ACL2ast, env):
	"""
	Mostly just using '==' works fine, but sometimes we need to do something else.  For instance, when testing
	strings, we must use a match expression.
	"""
	printLine()

	# Extract the arguments
	(lhs, rhs) = filterExtract(ACL2ast, 3, [[list, str], [list, str]], [None, None], f"`==`")

	# If either the lhs or rhs start with a colon, we know we will compare strings
	if (isinstance(lhs, str) and lhs.startswith(':')):
		stringLit = lhs
		other = rhs
	elif (isinstance(rhs, str) and rhs.startswith(':')):
		stringLit = rhs
		other = lhs
	# Otherwise just use '=='
	else:
		fn = _num_op_gen('==', Sail_t_bool(), 2, infix=True)
		return fn(ACL2ast, env)

	# Generate the match expression if we're matching on strings
	(otherSail, env, _) 	= transform.transformACL2asttoSail(other, env)
	(stringLitSail, env, _) = transform.transformACL2asttoSail(stringLit, env)
	otherSail = otherSail[0]
	stringLitSail = stringLitSail[0]

	return ([SailMatch(
				var=otherSail,
				matches=[(stringLitSail, SailBoolLit(True)),
						 (SailUnderScoreLit(), SailBoolLit(False))])],
			env, len(ACL2ast))

def _zp_fn(ACL2ast, env):
	'''
	Test for 0

	TODO: generalise to other 1-input infix funcs
	'''
	printLine()

	# Filter and extract tlements
	(operand, ) = filterExtract(ACL2ast, 2, [[list, str]], [None], "`zp`")

	# Translate
	(operandSail, env, _) = transform.transformACL2asttoSail([operand], env)

	# Return
	return ([SailApp(
				SailHandwrittenFn('==', Sail_t_fn([], Sail_t_bool())),
				[operandSail[0], SailNumLit(0)],
				infix = True)],
			env, len(ACL2ast))

def _num_op_gen(op, resultType, numOfArgs=None, infix=True):
	'''
	Args:
		- op : str - e.g. '+'
		- resultType : SailType - e.g. Sail_t_int()
		- numOfArgs : int | None
	'''
	def _num_op(ACL2ast, env):
		# Filter and extract arguments
		args = [item for item in ACL2ast if not isinstance(item, NewLine)]
		args = args[1:]

		# Perform checks on the args
		if not all(type(item) in [list, str, ACL2quote] for item in args): sys.exit(f"Error: type of num op argument not permitted: {ACL2ast}")
		if numOfArgs != None and len(args) != numOfArgs: sys.exit(f"Error: incorrect number of args to num op {ACL2ast}")

		# Translate the elements and get their types
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

		# # TODO: hack - if we're returning a bool but testing option types, wrap in is_some
		for (i, arg) in enumerate(argsSail):
			typ = typesSail[i]
			if isinstance(typ, Sail_t_option) and isinstance(resultType, Sail_t_bool):
				print("Got here")
				argsSail[i] = [SailApp(
					fn=SailHandwrittenFn(name='is_some', typ=Sail_t_fn([], Sail_t_bool())),
					actuals=arg
				)]
				typesSail[i] = Sail_t_bool()

		# Contruct the base AST and remove those elements from the args/types lists
		currentType = Sail_t_fn([typesSail[-2], typesSail[-1]], resultType)
		currentAST = [SailApp(
						fn = SailHandwrittenFn(op, typ = currentType),
						actuals = argsSail[-2] + argsSail[-1],
						infix = infix)]

		argsSail = argsSail[:-2]
		typesSail = typesSail[:-2]

		# Construct the rest of the tree
		while len(argsSail) > 0:
			currentType = Sail_t_fn([typesSail[-1], resultType], resultType)
			currentAST = [SailApp(
							fn = SailHandwrittenFn(op, typ = currentType),
							actuals = argsSail[-1] + currentAST,
							infix = infix)]
			argsSail = argsSail[:-1]
			typesSail = typesSail[:-1]


		# Return
		return (currentAST, env, len(ACL2ast))

	return _num_op

def _the_helper(theType, sailTerm):
	'''
	Args:
		- theType : SailType
		- sailTerm : [SailASTelem] | SailASTelem
	Returns:
		- [SailAstElem]
	'''
	# The sail term should be a symbol or single valued list
	if type(sailTerm) not in [str, list]: sys.exit(f"Error: `the` term not a string or list - {sailTerm}")
	if isinstance(sailTerm, list) and len(sailTerm) != 1: sys.exit(f"Error: `the` list not length - {sailTerm}")

	# Select the correct Sail `the`
	if isinstance(theType, Sail_t_int):
		retTerm = [SailApp(	fn = SailHandwrittenFn(	name = "the_int",
													typ = Sail_t_fn([Sail_t_int()], Sail_t_int(), {'escape'})),
							actuals = sailTerm)]
	elif isinstance(theType, Sail_t_nat):
		retTerm = [SailApp(	fn = SailHandwrittenFn(	name = "the_nat",
													typ = Sail_t_fn([Sail_t_int()], Sail_t_nat, {'escape'})),
							actuals = sailTerm)]
	elif isinstance(theType, Sail_t_range):
		(low, high) = theType.getRange()
		retTerm = [SailApp(	fn = SailHandwrittenFn(	name = "the_range",
													typ = Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_range(low, high), {'escape'})),
							actuals = [SailNumLit(low), SailNumLit(high), sailTerm[0]])]
	else:
		sys.exit(f"Error: unexpected type spec in `the` - {theType} in {sailTerm}")

	return retTerm

def _the_fn(ACL2ast, env):
	'''
	`the` allows the forcing of a type.
	'''
	(typeSpec, rest) = filterExtract(ACL2ast, 3, [[list], [list, str]], [None, None], "`the`")

	# Decode typespec
	theType = translateType(env, typeSpec[0], typeSpec[1:])

	# Decode the rest
	(sailTerm, env, _) = transform.transformACL2asttoSail(rest, env)

	# Get the return ast
	retTerm = _the_helper(theType, sailTerm)

	# Encapsulate and return	
	return (retTerm, env, len(ACL2ast))


def _parts_helper(lowAST, hiAST, widAST, env):
	"""
	Used for both _part_select_fn and _part_install_fn

	Args:
		lowAST: [ACL2astElem]
		hiAST: [ACL2astElem]
		widAST: [ACL2astElem]
		env: As above

	Returns:
		(size : SailNumLit, sizeLit : Int | None, lowASTsail : [SailASTelem, env : as above)
	"""
	# If :high and :width defined, fail
	if hiAST != [] and widAST != []:
		sys.exit(f"Error: can't use :high and :width in `part-select`.")

	# Otherwise check the other options
	if len(lowAST) == 1 and len(hiAST) == 1:
		# Translate
		(lowASTsail, env, _) = transform.transformACL2asttoSail(lowAST[0], env) # lowASTsail is expected to be a list after the `if` so don't unpack yet
		(hiASTsail, env, _) = transform.transformACL2asttoSail(hiAST[0], env)

		# Create size AST
		size = SailApp(fn=SailHandwrittenFn('-', typ=Sail_t_fn([Sail_t_int(), Sail_t_int()], Sail_t_int())),
					   actuals=[hiASTsail[0], lowASTsail[0]],
					   infix=True)

		# Create size literal
		if not isinstance(hiASTsail[0], SailNumLit) or not isinstance(lowASTsail[0], SailNumLit):
			print(f"Warning: expected :low and :high to be number literals for easy type conversation, instead got {lowASTsail[0]} and {hiASTsail[0]} respectively")
			sizeLit = None
		else:
			sizeLit = hiASTsail[0].getNum() - lowASTsail[0].getNum()

	elif len(lowAST) == 1 and len(widAST) == 1:
		# Translate
		(lowASTsail, env, _) = transform.transformACL2asttoSail(lowAST[0], env)
		(widASTsail, env, _) = transform.transformACL2asttoSail(widAST[0], env)

		# Create size AST
		size = widASTsail[0]

		# Create size literal
		if not isinstance(size, SailNumLit):
			print(f"Warning: expected :width to be a literal for simple type conversation, instead got {size}")
			sizeLit = None
		else:
			sizeLit = size.getNum()
	else:
		sys.exit(f"Error: incorrect keywords in this `_part_helper`")

	return (size, sizeLit, lowASTsail, env)

def _part_select_fn(ACL2ast, env):
	"""
	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-SELECT

	TODO: Possible problem if the `target` argument comes after keywords - assumed it's the first argument here
	"""
	# Get keyword values
	lowAST = getValueOfKeyword(ACL2ast, ':low')
	hiAST  = getValueOfKeyword(ACL2ast, ':high')
	widAST = getValueOfKeyword(ACL2ast, ':width')

	# Convert keywords to a size and start index
	(size, sizeLit, lowASTsail, env) = _parts_helper(lowAST, hiAST, widAST, env)

	# Translate the target of the part_select
	(target, _, _, _, _) = filterExtract(ACL2ast, 6, [None] * 5, [None] * 5, "`part_select`")
	(targetSail, env, _) = transform.transformACL2asttoSail(target, env)

	# Create the get_slice_int function application
	if sizeLit != None:
		innerRetType = Sail_t_bits(sizeLit)
		outerRetType = Sail_t_range(0, 2^sizeLit - 1)
	else:
		innerRetType = Sail_t_int()
		outerRetType = Sail_t_int()
	inner = SailApp(fn = SailHandwrittenFn(
							name = 'get_slice_int',
							typ = Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], innerRetType)),
					actuals = [size, targetSail[0] , lowASTsail[0]])

	# And convert the resulting bits to a range
	outer = SailApp(fn = SailHandwrittenFn(
							name = 'unsigned',
							typ = Sail_t_fn([], outerRetType)),
					actuals = [inner])

	return ([outer], env, len(ACL2ast))

def _part_install_fn(ACL2ast, env):
	'''
	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____PART-INSTALL
	'''
	# Get keyword values
	lowAST = getValueOfKeyword(ACL2ast, ':low')
	hiAST = getValueOfKeyword(ACL2ast, ':high')
	widAST = getValueOfKeyword(ACL2ast, ':width')

	# Convert keywords to a size and start index
	(size, _, lowASTsail, env) = _parts_helper(lowAST, hiAST, widAST, env)

	# Translate the target and value of the part_install
	(val, x, _, _, _, _) = filterExtract(ACL2ast, 7, [None] * 6, [None] * 6, "`part_install`")
	(valSail, env, _) = transform.transformACL2asttoSail(val, env)
	(xSail, env, _) = transform.transformACL2asttoSail(x, env)

	# Create the setting function application
	inner = SailApp(
		fn=SailHandwrittenFn(
			name='changeBits',
			typ=Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int())),
		actuals=[xSail[0], lowASTsail[0], size, valSail[0]])

	return ([inner], env, len(ACL2ast))

def _bstar_helper(bindersRemaining, results, env):
	'''
	Args:
		bindersRemaining : [[ACL2astElem] | ACL2astElem] - assume filtered
		results : [ACL2astElem] | ACL2astElem
		env : Env
	Returns:
		- ([ACL2astElem], env')
	'''
	# ===== Base case: no binders remaining, translate the result ===== #
	if len(bindersRemaining) == 0:
		(resultsSail, env, _) = transform.transformACL2asttoSail(results, env)
		return (resultsSail, env)

	# ===== Helper function to handle leading `!` and `?!` from names ===== #
	def sanatiseBstarName(name):
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
		b = sanatiseBstarName(b)

		# TODO: remove this old code
		# Often we bind `ctx` to a quote, which is awkward so we ignore
		# if b.lower() == 'ctx':
		# 	# Recurse on rest of list
		# 	(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)
		#
		# 	# Create sail term
		# 	toReturn = recursedSail[0]
		# else:


		# Translate the body first, find its type, then register name with env and boundNames
		(exprSail, env, _) = transform.transformACL2asttoSail(binding[1], env)
		if len(exprSail) != 1: sys.exit(f"Error: body length not 1 in `let*` in `b*` - {exprSail}")
		exprType = exprSail[0].getType()
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

			# Perform the bindings
			afters = [] # [(name:str, the_expr : ACL2astElem)]
			boundVars = []
			# Each name we bind may either be a raw symbol (easy) or a list (e.g. `the` - harder)
			for ident in b[1:]:
				if type(ident) == str:
					name = sanatiseBstarName(ident)
					boundVars.extend(env.pushToBindings([name]))
					boundNames.append(name)
				elif type(ident) == list and ident[0].lower() == 'the':
					# This is a bit of a hack as we can have more general patterns in an mv b* binder
					name = sanatiseBstarName(ident[2])
					ident[2] = name # _the_fn needs to be able to look up the sanatised symbol
					boundVars.extend(env.pushToBindings([name]))
					boundNames.append(name)
					(sailThe, env, _) = _the_fn(ident, env)
					afters.append((name, sailThe[0]))
				else:
					sys.exit(f"Unknown type of mv binder - {ident}")

			# Get type information from the function call
			bodyType = bodySail[0].getType().getSubTypes()
			for (i, t) in enumerate(bodyType):
				try:
					boundVars[i].getType()
				except:
					# TODO: make exception more specific
					boundVars[i].setType(t)

			# Apply necessary transformations to deal with structure inside the mv
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

					return (returnTerm, [name], env)

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

					return (returnTerm, [name] + recursedNames, env)

			if afters != []:
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
			(typeSpec, name) = filterExtract(b, 3, [[list], [str]], [None, None], "`the`")
			name = sanatiseBstarName(name)

			# Translate the type		
			theType = translateType(env, typeSpec[0], typeSpec[1:])

			# Register name with env and boundNames, translate the body and encapsulate in correct `the`
			bv = env.pushToBindings([name], [theType])
			boundNames.append(name)
			body = filterAST(binding[1])
			(bodySail, env, _) = transform.transformACL2asttoSail(body, env)
			bodySail = _the_helper(theType, bodySail)

			# Recurse on the rest of the list and encapsulate in correct `the`
			(recursedSail, env) = _bstar_helper(bindersRemaining[1:], results, env)

			# Create Sail term
			# E.g. `let n = foo(z) in the_range(0, 100, result)`
			toReturn = SailLet(
							varName = bv[0],
							expr = bodySail,
							body = recursedSail
						)
		# `when`, `if` and `unless` binders
		elif bindType.lower() in ['when', 'if', 'unless']:
			# Extract and translate the conditional expression
			cond, = filterExtract(b, 2, [[list, str]], [None], "`when`")
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


	# De-registed names bound in this call from env
	env.popWithCheck(boundNames)

	# Return
	return ([toReturn], env)

def _bstar_fn(ACL2ast, env):
	'''
	We could use the macro expander to expand the b* macro, but this gives us
	some really low-level garbage which, when translated to Sail, would likely
	be pretty horrible.
	See here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____B_A2
	'''
	printLine()

	# Filter and extract
	(binders, results) = filterExtract(ACL2ast, 3, [[list], None], [None, None], '`b*`')

	# Filter binders and results
	binders = filterAST(binders)
	if isinstance(results, list):
		results = filterAST(results)

	# If result is a quote, we don't want to deal with it
	if type(results) in [ACL2quote, ACL2qq]:
		return ([None], env, len(ACL2ast))

	# Use helper
	(toReturn, env) = _bstar_helper(binders, results, env)

	# Return
	return (toReturn, env, len(ACL2ast))


def _mv_fn(ACL2ast, env):
	# Extract things to return and translate them one by one
	args = ACL2ast[1:]
	sailArgs = []
	for a in args:
		(sa, env, _) = transform.transformACL2asttoSail(a, env)
		if len(sa) > 1: sys.exit(f"Error: length of argument to mv not 1 - {sa}")
		sailArgs.extend(sa)

	'''
	Errors in the model are reported by lists with a short string descriptor and optionally some associated data.
	These are problematic in Sail, and, for now, we replace them with an Option type representing the presence or
	absence of an error.

	Assumptions:
	1.	That error lists are the first item to be returned in an `mv`
	2.	That the first item in an error list is a string or option(string)
	'''
	# The error list is a real list
	if isinstance(sailArgs[0], SailTuple):
		errorString = sailArgs[0].getItems()[0]
		if isinstance(errorString.getType(), Sail_t_string):
			sailArgs = [someHelper(errorString)] + sailArgs[1:]
		if isinstance(errorString.getType(), Sail_t_option) and isinstance(errorString.getType().getTyp(), Sail_t_string):
			sailArgs = [errorString] + sailArgs[1:]
	# The error list is a single item
	if isinstance(sailArgs[0], SailStringLit):
		sailArgs = [someHelper(sailArgs[0])] + sailArgs[1:]

	return ([SailTuple(sailArgs)], env, len(ACL2ast))

def _case_fn(ACL2ast, env):
	# Filter comments and newlines and extract top-level elements
	ACL2astFiltered = filterAST(ACL2ast, comments=True)

	var = filterAST(ACL2astFiltered[1], comments=True)
	cases = ACL2astFiltered[2:]

	# Translate the var expression
	(varSail, env, _) = transform.transformACL2asttoSail(var, env)
	if len(varSail) != 1: sys.exit(f"Error: incorrect length of variable to match on in `case` - {varSail}")
	varSail = varSail[0]

	# The `cases` list is a list of (pattern, expr) pairs.  However, some patterns may themselves be lists.  This
	# indicates that if the variable matches *any* of the items in that list, the expression is evaluated.  Here, we
	# explode such lists so we can translate to Sail.
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
		# ... filter newlines and comments and extract pattern and expression
		case = filterAST(case, comments=True)
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

	return ([toReturn], env, len(ACL2ast))

def _def_inst_fn(ACL2ast, env):
	# Construct the term to send for evaluation
	toSend = [':trans', ACL2ast]

	# Send to the ACL2server for evaluation
	newAST = env.evalACL2(toSend, debracket=True)

	# Remove top level newlines
	newAST = filterAST(newAST)

	# Result is of the form 
	# `if _ then (ACL2::MAKE-EVENT-FN '(STD::DEFINE-FN ... )) else _`
	# We are interested in the `define-fn` form, so extract as appropriate
	newAST = filterAST(newAST[0]) # `if ... `
	newAST = filterAST(newAST[2]) # 'make-event ...'
	newAST = newAST[1] # quote define-fn

	# `define-fn` has the form `define-fn name args world` - we ignore the
	# world here, just passing `name` and `args` to our _define_fn,
	# effectively
	newAST = filterAST(newAST.getAST())
	name = filterAST(newAST[1].getAST())
	args_body = filterAST(newAST[2].getAST())
	print(f"Name: {name}")
	print(f"Args and body: {args_body}")

	# Call _define_fn
	(toReturn, env, _) = _define_fn(['define'] + [name] + args_body, env)

	return (toReturn, env, len(ACL2ast))

def _xr_fn(ACL2ast, env):
	'''
	XR is the register accessor function (rw is the updater).
	Format is (xr fld index x86)
		- fld is what we switch on
		- index is used by some fields
		- we ignore the x86 object because it is represented by global state in Sail

	See: https://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____XR
	'''
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
		sys.exit(f"Error: not implemented field {fld} in `_xr_fn`")

	return ([returnAST], env, len(ACL2ast))

def _cond_helper(sailClauses):
	# Base case, final element
	if len(sailClauses) == 1:
		clause = sailClauses[0]
		if not (isinstance(clause[0], SailBoolLit) and clause[0].getBool()):
			print("Warning: final clause in `cond` doesn't have condition 't'")
			# TODO: this is really a hack to get `chk-exc-fn` working.  See comment in `feature_flags_fn` for why this is OK.
			return noneHelper(Sail_t_string())
			# return errorHelper("Error message added during translation - final cond expression not `t`.")
		else:
			return clause[1]

	# Recursive case
	clause = sailClauses[0]
	elseCase = _cond_helper(sailClauses[1:])

	return SailIf([clause[0]], [clause[1]], [elseCase])

def _cond_fn(ACL2ast, env):
	'''
	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/COMMON-LISP____COND

	Cond has form `cond clause1 ... clausen`
	'''
	clauses = ACL2ast[1:]
	clausesSail = []

	# Translate clauses
	for c in clauses:
		if len(c) == 1: sys.exit("Error: clause in cond length 1.  This is allowed but not yet implemented here")
		if len(c) != 2: sys.exit("Error: clause length not 2")
		cond = c[0]
		expr = c[1]

		# Bit of a hack if the cond is 't'
		if isinstance(cond, str) and cond.upper() == 'T':
			condSail = SailBoolLit(True)
		else:
			(condSail, env, _) = transform.transformACL2asttoSail(cond, env)
			condSail = condSail[0]

		(exprSail, env, _) = transform.transformACL2asttoSail(expr, env)
		exprSail = exprSail[0]

		clausesSail.append((condSail, exprSail))

	# Construct nested if then else (if...)
	toReturn = _cond_helper(clausesSail)

	return ([toReturn], env, len(ACL2ast))


def _list_helper(elems, env):
	# Failure case, no items
	if elems == []: sys.exit("No elements in list constructor")

	# Translate the first item
	head = elems[0]
	(headSail, env, _) = transform.transformACL2asttoSail(head, env)

	# Base case, one element, create a singleton tuple
	if len(elems) == 1:
		return (SailTuple(subItems=headSail), env)

	# Recursive case: translate the rest and cons head element
	else:
		tail = elems[1:]
		(tailSail, env) = _list_helper(tail, env)
		headSail.append(tailSail)
		return (SailTuple(subItems=headSail), env)


def _list_fn(ACL2ast, env):
	'''
	For now we represent heterogeneous lists as nested tuples
	'''
	elems = ACL2ast[1:]
	(sailList, env) = _list_helper(elems, env)

	return ([sailList], env, len(ACL2ast))

def _mv_let_fn(ACL2ast, env):
	'''
	Massage into a form amenable to using b*

	See here for the syntax: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____MV-LET
	'''
	varNames = ACL2ast[1]
	mvExpr   = ACL2ast[2]
	body     = ACL2ast[-1]

	bstarExpr = ['b*', [[['mv'] + varNames, mvExpr]], body]

	return _bstar_fn(bstarExpr, env)

def _mbt_fn(ACL2ast, env):
	# We'd better translate the body just in case it modifies the env in some way
	(_, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)

	# Other that than, just return true
	return ([SailBoolLit(True)], env, len(ACL2ast))

def _cons_fn(ACL2ast, env):
	(sail1, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	(sail2, env, _) = transform.transformACL2asttoSail(ACL2ast[2], env)

	return [SailTuple([sail1[0], SailTuple([sail2[0]])])], env, len(ACL2ast)


def _change_helper(keywordsOrder, changeFn):
	printLine()

	def innerFn(ACL2ast, env):
		# This function was originally a standalone function specified for change_rflagsBits.  It has now been
		# generalised

		# Filter and extract
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
		(keywordsMap, _) = extractAllKeywords(keywords, failOnRedef=True)

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

		# Form the final return
		sailAST = SailApp(
			fn=changeFn,
			actuals=sail_change_input + keywordActuals)

		# Return
		return ([sailAST], env, len(ACL2ast))

	return innerFn


def _generateBitstructFieldAccessorAndUpdater(typeName, field, currentLow, env):
	# Constant - basically copied from basic-structs.lisp
	# TODO: make this automatic
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
	fieldName = field[0]
	type = field[1]
	if type.lower() not in typeWidths: sys.exit(f"Error: unrecognised type in defbitstruct - {type}")
	width = typeWidths[type.lower()]

	# Create the accessor function
	accessorInputBitsBV = SailBoundVar(binding='inputBits', typ=Sail_t_int())
	accessorFn =\
		SailFn(
			name=f"{typeName.lower()}_get_{fieldName.lower()}",
			formals=[accessorInputBitsBV],
			body=[
				SailApp(
					fn=SailHandwrittenFn(
						name='genericBitstructAccessor',
						typ=Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int())
					),
					actuals=[
						SailNumLit(width),
						accessorInputBitsBV,
						SailNumLit(currentLow)
					]
				)
			]
		)

	# Create the updater function
	updaterSpliceBitsBV = SailBoundVar(binding='spliceBits', typ=Sail_t_int())
	updaterInputBitsBV = SailBoundVar(binding='inputBits', typ=Sail_t_int())
	updaterFn =\
		SailFn(
			name=f"set_{typeName.lower()}_get_{fieldName.lower()}",
			formals=[updaterSpliceBitsBV, updaterInputBitsBV],
			body=[
				SailApp(
					fn=SailHandwrittenFn(
						name='genericBitstructUpdater',
						typ=Sail_t_fn([Sail_t_int(), Sail_t_int(), Sail_t_int(), Sail_t_int()], Sail_t_int())
					),
					actuals=[
						SailNumLit(width),
						updaterSpliceBitsBV,
						SailNumLit(currentLow),
						updaterInputBitsBV
					]
				)
			]
		)

	# Register with the environment
	ACL2accessor = f"{typeName}->{fieldName}".upper()
	env.addToGlobal(ACL2accessor, apply_fn_gen(funcToApply=accessorFn, numOfArgs=1))
	ACL2updater = f"!{typeName}->{fieldName}".upper()
	env.addToGlobal(ACL2updater, apply_fn_gen(funcToApply=updaterFn, numOfArgs=2))

	# Return the function and the new width
	return (accessorFn, updaterFn, currentLow + width, env)


def _parseBitstructFields(typeName, fieldsList, currentLow, previousBoundVar, env):
	##### Base case: no more fields
	if fieldsList == []:
		return ([], [], previousBoundVar, [], env)

	##### Recursive case
	# Get the accessor and updater fns for the current field
	hd = fieldsList[0]
	(accessorFn, updaterFn, newLow, env) = _generateBitstructFieldAccessorAndUpdater(typeName, hd, currentLow, env)

	# Get the accessor and updater fns and the let expression for the change_ function for the remainder of the fields
	fieldName = hd[0]
	inputBV = SailBoundVar(f"input_{fieldName}", typ=Sail_t_option(Sail_t_int()))
	outputBV = SailBoundVar(f"output_{fieldName}", typ=Sail_t_int())
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


def _defbitstruct_fn(ACL2ast, env):
	'''
	See here for syntax: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=FTY____DEFBITSTRUCT

	TODO: handle keywords

	This function returns lots of function definitions as well as substantially changing the env
	'''
	printLine()

	(_, ACL2ast) = extractAllKeywords(ACL2ast, False)

	typeName = ACL2ast[1]
	fields_or_width = ACL2ast[2]

	if not isinstance(fields_or_width, list):
		# Width
		sys.exit(f"Error: not yet implemented fixed width defbitstruct - {ACL2ast}")
	else:
		# List of fields
		change_input = SailBoundVar('input_bits', typ=Sail_t_int())
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
		env.addToGlobal(token=f'change-{typeName}', fn=_change_helper(keywordsOrder=[kw.upper() for (kw, _) in changeOrder],
																	  changeFn=changeFn))

	return (accessors + updaters + [changeFn], env, len(ACL2ast))

def errorHelper(msg):
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

def _er_fn(ACL2ast, env):
	'''
	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____ER
	'''
	hard_or_soft = ACL2ast[1]
	if hard_or_soft.lower() == 'soft':
		sys.exit("Error: not yet implemented soft errors")
	if not hard_or_soft.lower().startswith('hard'):
		sys.exit("Error: unrecognised error type")

	# toReturn = SailApp(
	# 	fn=SailHandwrittenFn(
	# 		name='error',
	# 		typ=Sail_t_fn([Sail_t_string()], Sail_t_error(), effects={'escape'})
	# 	),
	# 	actuals=[SailStringLit(f"Error thrown from function: {env.defineSlot}")]
	# )

	toReturn = errorHelper(f"Error thrown from function: {env.defineSlot}")

	return ([toReturn], env, len(ACL2ast))

def _ms_fresh_fn(ACL2ast, env):
	'''
	A macro defined in 'decoding-and-spec-utils.lisp` which, for now, we handle manually
	Interpret the first argument as a string to be returned as an error

	TODO: maybe merge with _er_fn above
	'''
	(errString, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)

	# toReturn = SailApp(
	# 	fn=SailHandwrittenFn(
	# 		name='error',
	# 		typ=Sail_t_fn([Sail_t_string()], Sail_t_error(), effects={'escape'})
	# 	),
	# 	actuals=[SailStringLit(f"Model state error: {errString[0].getString()}")]
	# )

	toReturn = errorHelper(f"Model state error: {errString[0].getString()}")

	return ([toReturn], env, len(ACL2ast))

def _fault_fresh_fn(ACL2ast, env):
	'''
	TODO: make this more useful, e.g. at least specifying file and line number
	'''
	# toReturn = SailApp(
	# 	fn=SailHandwrittenFn(
	# 		name='error',
	# 		typ=Sail_t_fn([Sail_t_string()], Sail_t_error(), effects={'escape'})
	# 	),
	# 	actuals=[SailStringLit(f"A fault occurred.  Original ACL2 AST:\n{ACL2ast}")]
	# )

	toReturn = errorHelper(f"A fault occurred.  Original ACL2 AST:\n{ACL2ast}")

	return ([toReturn], env, len(ACL2ast))

def _ifix_fn(ACL2ast, env):
	'''
	To 'coerce to an integer' we use Sail type checking, levereging `the_int`
	'''
	(sailArg, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	toReturn = _the_helper(Sail_t_int(), sailArg)
	return (toReturn, env, len(ACL2ast))

def _nfix_fn(ACL2ast, env):
	'''
	See description for `_ifix_fn`

	TODO: make this function return 0 when the value is negative in line with the spec at:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____NFIX
	'''
	(sailArg, env, _) = transform.transformACL2asttoSail(ACL2ast[1], env)
	toReturn = _the_helper(Sail_t_int(), sailArg)
	return (toReturn, env, len(ACL2ast))


def _progn_fn(ACL2ast, env):
	'''
	E.g. in `top-level-memory.lisp` where functions including `rme08` are defined.  We simply evaluate each
	event in turn.  A little like `_defsection_fn` we wrap the arguments in CodeTopLevel.

	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____PROGN
	'''
	events = ACL2ast[1:]
	(sailAST, env, _) = transform.transformACL2asttoSail(CodeTopLevel(events), env)

	print(f"progn ast: {sailAST}")

	return (sailAST, env, len(ACL2ast))


def parse1DArrayLiteral(array, env):
	'''
	For arrays in general see:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____ARRAYS
	'''
	header = array[0]
	rest = array[1:]

	### Handle the header ###
	# Dimensions (required) - hope it's a single number literal, (dim1 dim2) if 2D
	# Maximum length (required) - ignore this
	# Default (optional) - if ommitted then nil
	# Name (optional) - probably ignore
	# Order (optional) - will affect direction.  `<` is the default; `>`; `:none`/nil (no reordering by compress1)
	dimension = getValueOfKeyword(header, ':dimensions', errorOnMoreThanOne=True)[0]
	dimension = dimension[0]
	try:
		dimension = int(dimension)
	except ValueError:
		raise ValueError(f"Error parsing array header, dimensions not an integer literal - {header}")

	sailDefault = SailPlaceholderNil()
	if hasKeyword(header, ':default'):
		maybeDefault = getValueOfKeyword(header, ':default', errorOnMoreThanOne=True)
		maybeDefault = maybeDefault[0]
		if maybeDefault != 'X' and maybeDefault.lower()[-3:] != 'nil':
			(sailDefault, env, _) = transform.transformACL2asttoSail(maybeDefault, env)
			sailDefault = sailDefault[0]

	asc = True
	if hasKeyword(header, ':order'):
		maybeOrder = getValueOfKeyword(header, ':order', errorOnMoreThanOne=True)
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
	# Check indicies are consecutive through 0..dim-1
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


def _aref1_fn(ACL2ast, env):
	'''
	For aref1 see:
	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____AREF1

	General form: `(aref1 name alist index)`
	 - `name` has no effect on the semantics of the list and we can safely ignore it
	 - We assume that `alist` is a list literal.  In the model they are often represnted by contants which we can
	   expand into their literal form.
	'''
	alist = env.evalACL2(ACL2ast[2])[0]
	dimension, sailDefault, asc, sailItems, env = parse1DArrayLiteral(alist, env)

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

def _with_output_fn(ACL2ast, env):
	'''
	See: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____WITH-OUTPUT

	We just ignore this and evaluate its contents

	TODO: read the manual properly and check this is correct
	'''
	return transform.transformACL2asttoSail(ACL2ast[-1], env)

def _minus_fn(ACL2ast, env):
	'''
	The minus symbol (-) can be used in unary or binary (or n-ary, as it happens) form.
	'''
	if len(ACL2ast) == 2:
		sailArg, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)
		return [SailApp(
			fn=SailHandwrittenFn(
				name='negate',
				typ=Sail_t_fn([Sail_t_int()], Sail_t_int())
			),
			actuals=sailArg,
		)], env, len(ACL2ast)
	else:
		return _num_op_gen('-', Sail_t_int(), 2)(ACL2ast, env)

def _consp_fn(ACL2ast, env):
	'''
	TODO: this will liekly need fixing to deal with more cases
	TODO: remove as currently not used
	'''
	if len(ACL2ast) != 2: sys.exit("Error: expected AST of length 2 in consp")

	sailArg, env, _ = transform.transformACL2asttoSail(ACL2ast[1], env)

	return convertToBool(sailArg), env, _

def _feature_flag_fn(ACL2ast, env):
	'''
	`feature-flags` has the guard that its argument is a subset of the support features.  It checks that all features
	in its arguemnt are enabled by checking by passing each one to `feature-flag` (not plural).  In turn,
	`feature-flag` uses `cpuid-flag` macro which uses `cpuid-flag-fn`... which is always 1 because all features are
	enabled by default.  Thus, translate this simple as '1'.

	'''
	newAST = env.evalACL2(ACL2ast)
	sailAST, env, _ = transform.transformACL2asttoSail(newAST, env)

	return sailAST, env, len(ACL2ast)

def _pe_fn(ACL2ast, env):
	'''
	E.g. the function `64-bit-compute-mandatory-prefix-for-two-byte-opcode` is generated using a make-event.  Normally
	we would just translate the code that the thing the make-event calls generates.  here we cannot because it's local,
	so we use a :pe instead.
	'''
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

	sailASTFn, env, _ = _define_fn(ACL2astFn[0], env)
	env.addToAuxillary(sailASTFn)

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
		sys.exit("Error: unexpected number of items in return from _define_fn")

	return sailast, env, len(ACL2ast)

def _member_eq_fn(ACL2ast, env):
	'''
	We choose to return this as a bool even through technically, the spec says it should be a list

	http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=COMMON-LISP____MEMBER

	Assume the list we are given is a list of string literals.
	'''
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


def filterActuals(ACL2ast, numOfArgs):
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
	# Returns: {(keyword : str): (value :[[ACL2astElems] | ACL2astElem])}
	(ACL2keywords, _) = extractAllKeywords(ACL2ast[numOfArgs+1:], failOnRedef=True)

	return (ACL2actuals, ACL2keywords)

def apply_fn_gen(funcToApply, numOfArgs, keywordStruct=None):
	'''
	Generates a function which can be registered with the environment given a
	translated function to execute and the number of arguments it takes.

	Args:
		- funcToApply : SailFn | SailHandwrittenFn
		- numOfArgs : int
		- keywordStruct : SailStruct
	Returns:
		- fn : as above
	'''
	def apply_fn_inner(ACL2ast, env):
		# Filter out the newlines and comments from the actuals
		# ACL2keywords : {(keyword : str): (value :[[ACL2astElems] | ACL2astElem])}
		(ACL2actuals, ACL2keywords) = filterActuals(ACL2ast, numOfArgs)

		# Translate the non-keyword actuals.  Env should not change but don't test for that.
		# TODO: maybe test for env changing.  See comment in `_define_fn` for detail.
		SailActuals = []
		for (i, a) in enumerate(ACL2actuals):
			# Bit of a hack: if symbol is NIL and the arg is a boolean translate to False
			if isinstance(a, str) and a.upper() == 'NIL' and isinstance(funcToApply.getType().getLHS()[i], Sail_t_bool):
				SailActuals.append(SailBoolLit(False))
				continue

			# If not, do main translation
			(aSail, env, _) = transform.transformACL2asttoSail(a, env)
			SailActuals.extend(aSail)

		# Translate keyword actuals and form the appropriate struct
		if keywordStruct != None:
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

		# Construct the Sail AST and return the number of ACL2 AST items consumed
		return ([SailApp(funcToApply, SailActuals)], env, len(ACL2ast))

	return apply_fn_inner

def apply_macro_gen(numOfArgs, useTrans1=True):
	'''
	Generates a function which can be registered with the environment which
	will, given a macro name, expand it using the ACL2 server and translate the
	resulting code

	Args:
		- numOfArgs : int | None
	Returns:
		- fn : as above
	'''
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
		return (SailAST, env, len(ACL2ast))

	return apply_macro_inner

def loadManualEnv():
	# These definitions are high-level acl2/lisp keywords and are
	# implemented using Python code
	manualDefinitions = {
		'in-package'.upper() : 		_in_package_fn,
		'include-book'.upper() : 	_include_book_fn,
		'local'.upper() : 			_local_fn,
		'defsection'.upper() :		_defsection_fn,
		'define'.upper() : 			_define_fn,
		'make-event'.upper() : 		_make_event_fn,
		'defmacro'.upper() : 		_defmacro_fn,
		'add-macro-alias'.upper() : _add_macro_alias_fn,
		'mbe'.upper() : 			_mbe_fn,	
		'defthm'.upper() : 			_defthm_fn,
		'defthmd'.upper() :			_defthm_fn,
		'defthm-signed-byte-p'.upper():	_defthm_fn,
		'if'.upper() : 				_if_fn,
		'encapsulate'.upper() : 	_encapsulate_fn,
		'the'.upper() :				_the_fn,
		'+'.upper() :				_num_op_gen('+', Sail_t_int()),
		'binary-+'.upper() :		_num_op_gen('+', Sail_t_int()),
		'*'.upper() :				_num_op_gen('*', Sail_t_int()),
		'-'.upper() :				_minus_fn,
		'='.upper() :				_num_op_gen('==', Sail_t_bool(), 2, infix=True),
		'equal'.upper() : 			_num_op_gen('==', Sail_t_bool(), 2, infix=True),
		'eql'.upper() :				_num_op_gen('==', Sail_t_bool(), 2, infix=True),
		'int='.upper() :			_num_op_gen('==', Sail_t_bool(), 2, infix=True),
		'eq'.upper(): 				_num_op_gen('==', Sail_t_bool(), 2, infix=True),
		'/='.upper() :				_num_op_gen('!=', Sail_t_bool(), 2, infix=True),
		'<'.upper() :				_num_op_gen('<', Sail_t_bool(), 2),
		'>'.upper() :				_num_op_gen('>', Sail_t_bool(), 2),
		'<='.upper() :				_num_op_gen('<=', Sail_t_bool(), 2),
		'and'.upper() :				_num_op_gen('&', Sail_t_bool(), None, infix=True),
		'or'.upper() :				_num_op_gen('|', Sail_t_bool(), None, infix=True),
		'truncate'.upper() :		_num_op_gen('tdiv_int', Sail_t_int(), 2, infix=False),
		'rem'.upper() :				_num_op_gen('tmod_int', Sail_t_int(), 2, infix=False),
		'zp'.upper() :				_zp_fn,
		'part-select'.upper() :		_part_select_fn,
		'part-install'.upper() :	_part_install_fn,
		'b*'.upper() : 				_bstar_fn,
		'mv'.upper() :				_mv_fn,
		'case'.upper() :			_case_fn,
		'def-inst'.upper() :		_def_inst_fn,
		'defrulel'.upper() : 		_defthm_fn,
		'in-theory'.upper() :		_defthm_fn,
		'xr'.upper() :				_xr_fn,
		'cond'.upper() :			_cond_fn,
		'let'.upper() :				_bstar_fn,
		'let*'.upper() :			_bstar_fn,
		'list'.upper() :			_list_fn,
		'mv-let'.upper() :			_mv_let_fn,
		'mbt'.upper() :				_mbt_fn,
		'cons'.upper() :			_cons_fn,
		'defbitstruct'.upper() :	_defbitstruct_fn,
		'def-ruleset'.upper() :		_defthm_fn,
		'er'.upper() :				_er_fn,
		'!!ms-fresh'.upper() :		_ms_fresh_fn, # `!!ms-fresh` etc. are macros defined in `decoding-and-spec-utils.lisp`
		'!!fault-fresh'.upper() :	_fault_fresh_fn, # Again, macro defined in `decoding-and-spec-utils.lisp`
		'ifix'.upper() :			_ifix_fn,
		'nfix'.upper() :			_nfix_fn,
		'progn'.upper() :			_progn_fn,
		'aref1'.upper() :			_aref1_fn,
		'with-output'.upper() :		_with_output_fn,
		'consp'.upper() :			_consp_fn,
		'set-non-linearp'.upper():	_defthm_fn, # I.e. ignore
		'feature-flag-macro'.upper() :	_feature_flag_fn,
		'feature-flags-macro'.upper():	_feature_flag_fn,
		'64-bit-compute-mandatory-prefix-for-two-byte-opcode'.upper() : _pe_fn,
		'32-bit-compute-mandatory-prefix-for-two-byte-opcode'.upper() : _pe_fn,
		'member-eq'.upper() :		_member_eq_fn,

		# TODO: these are here as a bit of a hack.  The actual implementations update the RIP incase of restart attempts
		# so would be worth translating properly at some point
		'x86-illegal-instruction'.upper() :		_fault_fresh_fn,
		'x86-step-unimplemented'.upper() :		_ms_fresh_fn,
		'x86-general-protection'.upper() :		_fault_fresh_fn,
		'x86-device-not-available'.upper() :	_fault_fresh_fn,
	}

	# These definitions have been implemented as handwritten Sail code
	# Maps strings to SailHandwrittenFn
	handwrittenDefinitions = manualTranslations.generateHandwrittenDefinitions()

	for (name, funcToApply) in handwrittenDefinitions.items():
		name = name.upper()
		manualDefinitions[name] = apply_fn_gen(funcToApply, funcToApply.getNumFormals())

	# These are macros.  Note that numOfArgs is now redundant
	macroNames = [
		# Name					numOfArgs	useTrans1
		('1-'.upper(),				1,		True),
		('1+'.upper(),				1,		True),
		('cpl'.upper(),				1,		True), # Buried away as `(defabbrev cpl ...)` in `paging.lisp`
		('ntoi'.upper(),			2,		True), # Defined in `utilities.lisp` - seems approrpiate to palce here given out custom `generateutils.py` file
	]
	for (mn, numOfArgs, useTrans1) in macroNames:
		manualDefinitions[mn] = apply_macro_gen(numOfArgs, useTrans1)

	# These ones have not been implemented, but are registered here
	# as placeholders
	specFnType = Sail_t_fn([Sail_t_int()]*3, Sail_t_tuple([Sail_t_int()]*3))
	unimplementedNames = [
		# ('vex-decode-and-execute', 7, Sail_t_fn([Sail_t_int()]*7, Sail_t_int(), {'escape'})),
		('evex-decode-and-execute', 7, Sail_t_fn([Sail_t_int()] * 7, Sail_t_int(), {'escape'})),
	]

	######
	# Warning: some things are macros, e.g.s `+`, `<`, `part-select`, `1-`
	# See some macros here: https://www.cs.utexas.edu/users/moore/acl2/v6-2/ACL2-BUILT-INS.html
	######

	# And these have not yet been implemented
	for (name, numOfArgs, typ) in unimplementedNames:
		manualDefinitions[name.upper()] = apply_fn_gen(
									manualTranslations.unimplementedFunctionGen(
										name,
										numOfArgs,
										typ),
									numOfArgs
									)

	return manualDefinitions




