from lex_parse import lexLispFile, lexLispString, parseACL2, NewLine, ACL2Comment, ACL2String, pp_acl2, ACL2quote, \
	CodeTopLevel
from SailASTelems import *
import socketFuncs
import config_patterns
import config_function_maps
import manualInterventions
import utils

import sys
import os
from pathlib import Path
from shutil import copyfile
import socket
import argparse
import tomli


"""
Top level file for the translator.  Defines the environment, which is threaded
through most code, and top level translation functions.
"""


class Env:
	"""
	The environment is threaded through most of the translation and gives us
	the ability to: lookup translation functions for a Lisp token; register
	new translation functions when Lisp functions are defined; lookup and
	register current bindings (for functions and `let`); lookup and register
	other pieces of context such as the current function and Lisp file being
	translated.

	Most methods are simply getters and setters, although lookup() and
	evalACL2() are worth looking at.
	"""

	def __init__(self, config):
		"""
		Initialise the environment state.
		"""

		self.config = config

		'''
		The manual and automatic environments map Sail tokens (of type str) to
		the appropriate translation function.  Such a translation function
		takes an AST beginning with that token and translates it to Sail
		(recursively translating sub-expressions as necessary).  Thus, each
		translation function has the following type.  

			(ACL2ast : [ACL2astElem],
			env : Env)
			-> (Sailast' : [SailastElem],
				env' : Env,
				consumed : int)
				
		The `manualEnv` is a dictionary of manually-defined translation
		functions listed in config_function_maps and represents special tokens,
		handwritten functions, utility functions and some macros.
		
		The `autoEnv` is a dictionary of translation functions generated over
		the course of the translation.  For instance, if a function definition
		was encountered (a `define`), `autoEnv` allows us to translate its
		application to arguments elsewhere.
		'''
		self.manualEnv = config_function_maps.loadManualEnv()
		self.autoEnv = {}

		'''
		Bitfield types defined so far
		'''
		self.bitfieldTypes = {}

		'''
		The `bindStack` tracks the current bound variables.  It is a list
		because order matters: more recent bindings are near the end.  It
		maps Lisp tokens (str) to a Sail AST list (although most often just
		a SailBoundVar).
		
		I.e. [ (str, SailBoundVar) ] or [ (str, [SailASTelem]) ] 
		'''
		self.bindStack = []


		'''
		These fields track the path and file of the current Lisp file being
		translated.  It is necessary to record the history for when we return
		from translating one file to the file which called `include-book`.  
		'''
		self.path = ''
		self.pathStack = []
		self.fileStack = []

		'''
		A dictionary mapping the name of the Lisp files translated thus far
		to their translated ASTs.
		'''
		self.included = {}


		'''
		Stores the stack of Lisp tokens currently being translated.  Consider
		the following example:
		
		File-A.lisp:
			(include-book "File-B.lisp")
		
		File-B.lisp:
			(define mult (x y) 
				(if (eq y 1) x (+ x (mult x (- y 1))))
			)
			
		If we set the translator translating file A and the translator is
		currently translating the `(+ ...)` form, currentStack will contain:
		
			['include-book', 'define', 'if', '+']
		
		This is useful for debugging and for constraining manual interventions.
		'''
		self.contextStack = []

		'''
		`tr_define` sets this variable with the name of the function it is
		translating.  Tracking this information is useful for debugging and
		for constraining manual interventions. 
		'''
		self.defineSlot = ""

		'''
		The Lisp token `nil` cannot be translated unambiguously.  Sometimes,
		however, translation functions can infer a boolean context (e.g. the
		conditional of an `if`).  In these cases they can set this variable to
		inform translation of `nil`.
		'''
		self.currentType = None


		'''
		For the most part nested function definitions are impossible in Lisp.
		In one case, however, due to an unfortunate technicality, we do end up
		trying to translate a function definition in the middle of translating
		another.  To handle this, we place this nested translation inside a
		file called `auxiliary.sail` (we store these definitions in
		`auxiliaryFns`).  We then need to `$include` auxiliary.sail in the
		current file (keeping track of these files in `auxiliaryInclude`).

		See the translation function `tr_pe` for more info on this exceptional
		case.
		'''
		self.auxiliaryInclude = set()
		self.auxiliaryFns = []


		'''
		Setup the socket connection to the running ACL2 instance.
		'''
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			self.s.connect(('localhost', self.config.acl2_port))
		except:
			print("Warning: env could not connect to ACL2 server")


	def try_lookup(self, token, ignoreBindings=False):
		"""
		Given a token, this function looks up the translation function (TF)
		which allows the translator to actually consume the it.  In order:

		 -  If the token is a keyword (starts with a colon) return a
		 	TF which translates it as a string.
		 -  Search for the token in the local bindings, return a TF which
		 	simply returns the binding itself.
		 -  If the token is likely to be a number literal, return a TF which
		 	gives this literal.
		 -  Search through the automatic then manual dictionaries for a TF.

		Sometimes a built-in function can be used as the name for a bound
		variable.  Whilst this binding is valid, the symbol can still be used
		as a function.  Thus, there is the option to not search through the
		binding stack.

		Args:
			token : str
			ignoreBindings : bool
		Returns:
			fn : as above
		"""
		# Convert the token to uppercase
		token = token.upper()

		# If the token is a keyword (i.e. starts with a colon) return a string
		if token.startswith(':'):
			return lambda _, env: ([SailStringLit(token)], env, 1)

		# Check the local bindings - pick the last one to cope with re-binding
		if not ignoreBindings:
			for i in reversed(range(len(self.bindStack))):
				(acl2Token, sailToken) = self.bindStack[i]
				if acl2Token == token:
					return lambda _, env: ([sailToken], env, 1)

		# Check if the token is a number literal or a macro which translates into one
		# TODO: translate into Sail hex literals as appropriate
		if token.startswith(('#.*', '*', '#')) and token != '*': # Used for constants and, for example, hex numbers
			literal = self.evalACL2([':trans', token], debracket=True)
			literal = utils.convertLiteral(literal[0].getAST())
			return lambda _, env: ([SailNumLit(literal)], env, 1)

		maybeLiteral = utils.convertLiteral(token)
		if maybeLiteral is not None:
			return lambda _, env: ([SailNumLit(maybeLiteral)], env, 1)

		# Then check for automatically/manually defined symbols
		for e in [self.autoEnv, self.manualEnv]:
			if token in e:
				return e[token]

		return None

	def lookup(self, token, ignoreBindings=False):
		result = self.try_lookup(token, ignoreBindings)

		if result is None:
			# Hack: If we encounter an error flag variable that was previously removed from the translation, return False
			if is_acl2_flag_symbol(token):
				return lambda _, env: ([SailBoolLit(False)], env, 1)
			else:
				# If lookup fails, abort and print debug information.
				print()
				print("Token lookup failed: {}".format(token))
				print(f"Current file: {self.fileStack[-1]}")
				print(f"DefineStack: {self.defineSlot}")
				print(f"File stack: {self.fileStack}")
				print(f"Context stack: {self.contextStack}")
				print()
				raise KeyError(f"Token lookup failed: {token}")
		else:
			return result

	# === Environment manipulation
	def addToAuto(self, token, fn):
		"""
		Register a defined function with the automatic dictionary so a call to
		it can be translated later.

		Args:
			- token : str
			- fn : as above
		"""
		token = token.upper()

		if token in self.autoEnv:
			print(f"Error: overwriting token: {token}")
			sys.exit(1)
		else:
			self.autoEnv[token] = fn

	def addToAuxiliary(self, fn):
		"""
		Add a function definition to the list of function to be placed in the
		auxiliary file.  Add the name of the current file to the list of files
		which must include the auxiliary file.

		Args:
			fn: [SailASTelem]

		Returns:
			None
		"""
		self.auxiliaryInclude.add(utils.sanitiseSymbol(self.getFile()))
		self.auxiliaryFns.extend(fn)

	# === Bitfield types
	def addBitfieldType(self, type):
		if isinstance(type, Sail_t_bitfield) and type.getName().upper() not in self.bitfieldTypes:
			self.bitfieldTypes[type.getName().upper()] = type
		else:
			sys.exit(f"Error: Failed to add {type.pp()} to bitfield types")

	def lookupBitfieldType(self, name):
		return self.bitfieldTypes.get(name.upper())

	# === Bindings stack
	def pushToBindings(self, tokens, types=None, customSail=None):
		"""
		Push a list of bound names and, optionally, their types or some
		custom Sail code, to the binding stack.

		The local environment handles formal function parameters and
		`b*`/`let` bindings.  In most cases we're really just checking the
		current symbol has actually been bound, but sometimes we want to
		have a piece of custom Sail instead.

		Only one of `types` and `customSail` should be not None

		Args:
			- tokens: [str]
			- types : [SailType] | None
			- customSail : [SailASTelem] | None
		Returns:
			- [SailBoundVar] - only if customSail is None
		"""
		# Check inputs
		if types is not None and len(tokens) != len(types): sys.exit(
			f"Error: length of tokens doesn't equal length of types when pushing a binding - {tokens} and {types}")
		if customSail is not None and len(tokens) != len(customSail): sys.exit(
			f"Error: length of tokens doesn't equal length of customSail when pushing a binding - {tokens} and {customSail}")
		if types is not None and customSail is not None: sys.exit(
			"Error: only one of `tokens` and `customSail` should be not None when pushing a binding")

		# Add to the stack
		toReturn = []
		for (i, token) in enumerate(tokens):
			token = token.upper()

			if types is not None:
				boundVar = SailBoundVar(token, types[i])
				self.bindStack.append((token, boundVar))
			elif customSail is not None:
				self.bindStack.append((token, customSail[i]))
				boundVar = None
			else:
				boundVar = SailBoundVar(token)
				self.bindStack.append((token, boundVar))

			toReturn.append(boundVar)

		return toReturn

	def popWithCheck(self, tokens):
		"""
		Pop the list of names from the bind stack.  To help avoid programmer
		error, we check `tokens` really is the the tail of self.bindStack.

		Args:
			- tokens: [str]
		"""
		# We don't want to reverse in place so copy() first
		tokens = tokens.copy()
		tokens.reverse()
		for token in tokens:
			token = token.upper()
			if self.bindStack[-1][0] != token:
				sys.exit(f"Error: token '{token}' is not at top of bind stack whilst trying to unbind it")
			self.bindStack.pop()

	# === Current path functions
	'''
	As explained above, these paths and files are the current and historical
	paths and files to Lisp files being translated.
	'''
	def getPath(self):
		return self.path

	def pushPath(self, toAppend):
		"""
		Pushes the current path to the stack and appends the argument
		"""
		self.pathStack.append(self.path)
		self.path = os.path.join(self.path, toAppend)

	def popPath(self):
		self.path = self.pathStack.pop()

	def getFile(self):
		return self.fileStack[-1]

	def pushFile(self, toAppend):
		"""
		Pushes the current filename to the stack
		"""
		self.fileStack.append(toAppend)

	def popFile(self):
		self.fileStack.pop()

	'''
	As explained above, map Lisp file names to their translated ASTs. 
	'''
	def addToIncluded(self, file, ast):
		if file in self.included:
			sys.exit("Error: trying to add already included file")
		self.included[file] = ast

	def isIncluded(self, file):
		"""
		Tests if `file` has been translated on this run.
		"""
		return file in self.included, self.included.get(file, None)

	# === Current context functions
	'''
	As explained above, the context stack given the current path of Lisp
	tokens.
	'''
	def pushContext(self, ctx):
		self.contextStack.append(ctx)

	def popContext(self):
		return self.contextStack.pop()

	def peekContext(self):
		return self.contextStack[-1]

	def peekContext2(self):
		"""
		The second last item in the context stack is often more useful than
		the current item (which simply gives the current token being
		translated).
		"""
		if len(self.contextStack) >= 2:
			return self.contextStack[-2]
		else:
			return None

	'''
	As explained above, the define slot is the current function being
	translated.
	'''
	def setDefineSlot(self, item):
		self.defineSlot = item

	def getDefineSlot(self):
		return self.defineSlot

	# === Current type functions
	'''
	As explained above, this context helps resolve some Lisp `nil` instances 
	'''
	def setCurrentType(self, typ):
		"""
		Args:
			typ - should be one of the enums defined in SailPlaceholderNil
		"""
		self.currentType = typ

	def clearCurrentType(self):
		self.currentType = None

	def getCurrentType(self):
		return self.currentType

	# === Request an evaluation from the ACL2 server
	def evalACL2(self, expr, debracket=False):
		"""
		Takes an acl2 expression, sends it to the acl2 server for evaluation
		there, and returns the lexed and parsed result, leaving the caller to
		deal with the actual translation.

		Sometimes brackets are appropriate round the whole expression,
		sometimes they are not, hence the debracket parameter.

		Args:
			- expr : [ACL2astElem]
			- debracket : bool
		Returns:
			- [ACL2astElem]
		"""
		# Convert the AST into a concrete string
		toSend = pp_acl2(expr)
		if self.config.print_acl2_interactions:
			print("\n\n")
			print(f'With brackets: {toSend}')

		# Remove outer brackets if asked
		if debracket:
			toSend = toSend[1:-1]
			if self.config.print_acl2_interactions:
				print(f"Without brackets: {toSend}")

		# Send to the server
		socketFuncs.reliableSend(toSend, self.s)

		# Receive response
		response = socketFuncs.reliableRecv(self.s)
		if response == []:
			sys.exit("Error: null response from ACL2 server")

		# Convert response from [bytes] -> [str] -> str -> [acl2ASTelem]
		response = [b.decode("utf-8") for b in response]
		response = ''.join(response)
		if self.config.print_acl2_interactions:
			print(f"\nString response:\n{response}")

		# Lex and parse the response
		tokens = lexLispString(response)
		(ACL2ast, finalParseIndex) = parseACL2(tokens, 0, 0)
		if finalParseIndex != len(tokens):
			sys.exit("Parse error: did not reach or overran list of tokens in some generated code")

		return ACL2ast

	def evalACL2raw(self, toSend):
		"""
		Like evalACAL2() but does not lex or parse the result.
		"""
		socketFuncs.reliableSend(toSend, self.s)
		response = socketFuncs.reliableRecv(self.s)
		response = [b.decode("utf-8") for b in response]
		response = ''.join(response)
		return response

	def tidyUp(self):
		"""
		Run at the end of the program - closes the connection to the running
		ACL2 instance.
		"""
		self.s.close()


class Config:
	def __init__(self, config = dict()):
		self.top_level_file = config.get('top_level_file', '../../acl2/books/projects/x86isa/machine/x86.lisp')
		self.acl2_process = config.get('acl2_process', 'acl2')
		self.acl2_port = config.get('acl2_port', 1159)
		self.output_folder = config.get('output_folder', '../output')
		self.output_filename = config.get('output_filename', 'step')
		self.patch_folder = config.get('patch_folder', 'patches')
		self.unresolved_types_file = config.get('unresolved_types_file', '../output/unresolved_types.log')
		self.mbe_branch = config.get('mbe_branch', ':logic')
		self.print_acl2_interactions = config.get('print_acl2_interactions', False)

def isStringList(ys):
	"""
	Helper function: tests if a list comprises only keywords (strings
	beginning with a colon).

	Args:
		ys: list
	Returns:
		bool
	"""
	return all(isinstance(y, str) and y.startswith(':') for y in ys)


def convertToStringList(ys, env):
	"""
	If ys is comprises only keywords, converts to a Python list of strings.
	Indicates success/failure in its first return value.

	Args:
		ys: list
		env: Env
	Returns:
		Either (True, [SailASTelem]) or (False, None)
	"""
	if not isStringList(ys):
		return False, None

	ysSail = []
	for y in ys:
		ySail, env, _ = transformACL2asttoSail(y, env)
		ysSail.append(ySail[0])

	return True, ysSail


def listStartsWith(l, pattern, convertCase=True):
	"""
	Tests if `pattern` is the start of `l`.  Intended for lists of strings but
	is used on other datatypes (most notably `ACL2String`s in
	config_patterns.py)

	Args:
		l: list
		pattern: list
		convertCase: bool
	Returns:
		bool
	"""
	if len(l) < len(pattern):
		return False

	for (i, p) in enumerate(pattern):
		# Perform the check
		if (not convertCase) or (not isinstance(l[i], str)) or (not isinstance(p, str)):
			if l[i] != p:
				return False
		else:
			if l[i].upper() != p.upper():
				return False

	return True


def transformACL2FiletoSail(file, env):
	"""
	Reads, lexes, parses, translates and post-processes an ACL2 file and the
	tree of files required by `include-book`s.  The returned AST includes
	these extra files.  (In reality the behaviour of `include-book`
	translation is dictated by tr_include_book()).

	Here, post processing the AST means that unknown types are resolved.

	Args:
		- file : string
		- env  : Env.
	Returns:
		- (Sailast : [SailastElem],
		   env' : Env,
		   consumed : Int)
	"""
	##### Read file, lex and parse #####
	tokens = lexLispFile(os.path.join(env.getPath(), file))
	(ACL2ast, finalParseIndex) = parseACL2(tokens, 0, 0)
	if finalParseIndex != len(tokens):
		print("Parse error: did not reach or overran list of tokens")
		sys.exit(1)
	ACL2ast = CodeTopLevel(ACL2ast)
	print("Parsed file: {}".format(file))

	##### Translate #####
	ACL2ast = manualInterventions.patch_acl2_tree(ACL2ast, env)
	(SailAST, env, length) = transformACL2asttoSail(ACL2ast, env)

	##### Post-process the AST - resolve unknown types#####
	# Filter out 'None's
	SailAST = [item for item in SailAST if item is not None]

	# Collect apps and lets with unknown types
	def collectSet():
		predSet = set()
		for item in SailAST:
			# TODO: remove this hack by dealing with comments properly
			if isinstance(item, ACL2Comment):
				continue

			# Predicates which get apps and lets with unknown types in either their formals or actuals
			appPred = lambda e : isinstance(e, SailApp) and\
								 (any(a.getType().containsUnknown() for a in e.getActuals()) or
								  e.getFn().getType().containsUnknown())
			letPred = lambda e : isinstance(e, SailLet) and\
								 (e.getVarName().getType() == None or
								  e.getVarName().getType().containsUnknown() or
								  e.getExpr()[0].getType() == None or
								  e.getExpr()[0].getType().containsUnknown())
			ifPred = lambda e : isinstance(e, SailIf) and isUnknownType(e.getCondition().getType())
			overallPred = lambda e : appPred(e) or letPred(e) or ifPred(e)

			# Collect such apps and lets
			predSet = predSet.union(item.getChildrenByPred(overallPred))

		return predSet

	# Run the resolution algorithm and log results of each step to the log
	# file.  Anecdotally running three times is sufficient at the minute.
	f = open(env.config.unresolved_types_file, 'a')
	f.write(f"{file}\n")
	f.write(f"{'=' * len(file)}\n\n")
	for i in range(3):
		f.write(f"Still unresolved after {i} pass(es):\n----------------------------------\n")

		print(f"Collecting SailApps with unknown type for file {file} - iteration {i}")
		predSet = collectSet()
		if len(predSet) == 0:
			break

		print("Resolving the unknown types")
		for e in predSet:
			e.resolveTypes(f)
		print()

	f.close()

	return SailAST, env, length


def transformFunCall(ACL2ast, env):
	firstItem = ACL2ast[0]
	fn = env.lookup(firstItem, ignoreBindings=len(ACL2ast) > 1)
	env.pushContext(firstItem)
	(result, env, length) = fn(ACL2ast, env)
	env.popContext()
	return (result, env, length)

def transformACL2asttoSail(ACL2ast, env):
	"""
	Translates a lexed/parsed ACL2 AST into Sail.  Switches on the type of
	Lisp syntax of the AST, most often taking the 'list' route which indicates
	some sort of function application.  In this case, the lookup() function in
	Env provides a translation function which translates the list.

	Args:
		- ACL2ast : [ACL2astElem] | str
		- env : Env.
	Returns:
		- (Sailast : [SailastElem],
		   env' : Env,
		   consumed : Int)
	"""
	# First check if there are any manual replacements
	(doReplace, replacement, env) = manualInterventions.replacePatterns(ACL2ast, env)
	if doReplace:
		return replacement, env, 0

	# If the input is just a string, encapsulate in a list
	if type(ACL2ast) in [str]:
		ACL2ast = [ACL2ast]

	##### Main switch
	SailAST = []

	# Top Level
	if isinstance(ACL2ast, CodeTopLevel):
		# Translate first item
		currentItem = ACL2ast.first()

		# Test exclusions
		translate = True
		currFile = env.getFile()
		# We're in a file which is normally excluded ...
		if currFile in config_patterns.only_translate:
			# ... but we include some functions
			if not any(listStartsWith(currentItem, inc) for inc in config_patterns.only_translate[currFile]):
				translate = False

		if translate:
			(sailFirst, env, _) = transformACL2asttoSail(currentItem, env)
			SailAST.extend(sailFirst)

		# Translate the rest
		ACL2rest = ACL2ast.rest()
		if len(ACL2rest) != 0:
			(sailRest, env, _) = transformACL2asttoSail(ACL2ast.rest(), env)
			SailAST.extend(sailRest)
	# List
	elif isinstance(ACL2ast, list):
		# Test exclusions
		currFile = env.getFile()
		if currFile in config_patterns.exclude_lisp and \
		 any(listStartsWith(ACL2ast, ex) for ex in config_patterns.exclude_lisp[currFile]):

				SailAST = [None]
		# Do translation if necessary
		else:
			# Perform the translation - lookup translation function and use to
			# translate
			(SailItem, env, _) = transformFunCall(ACL2ast, env)
			SailAST.extend(SailItem)
	# NewLine (ignore - these should have been filtered out)
	elif isinstance(ACL2ast, NewLine):
		pass
	# ACL2Comment - in reality these should have been filtered out
	elif isinstance(ACL2ast, ACL2Comment):
		SailAST.append(ACL2ast)
	# ACL2String
	elif isinstance(ACL2ast, ACL2String):
		return [SailStringLit(ACL2ast.getString())], env, 1
	# Symbol - translates as string.  Never executes because we wrap strings
	# in lists above.
	elif isinstance(ACL2ast, str):
		fn = env.lookup(ACL2ast)
		env.pushContext(ACL2ast)
		(SailItem, env, _) = fn(ACL2ast, env)
		env.popContext()
		SailAST.extend(SailItem)
	# Quote - attempt to translate the contents
	elif isinstance(ACL2ast, ACL2quote):
		# Test if the contents is a number literal
		innerAST = ACL2ast.getAST()
		try:
			int(innerAST)
			innerASTNumeric = True
		except:
			innerASTNumeric = False

		# If the contents is a string, translate as such
		if isinstance(innerAST, str) and not innerASTNumeric:
			SailItem = SailStringLit(innerAST)
			SailItem = [SailItem]
		else:
			(SailItem, env, _) = transformACL2asttoSail(ACL2ast.getAST(), env)

		# If the contents is a list of strings, translate as such
		isStringList, stringList = convertToStringList(innerAST, env)
		if isStringList:
			return [SailListLit(stringList)], env, 1

		SailAST.extend(SailItem)
		return SailAST, env, 1
	# Otherwise
	else:
		print("Error: unexpected type in ACL2 ast: {}".format(type(ACL2ast)))
		sys.exit()

	return SailAST, env, len(ACL2ast)


def getPatchPath(name, env, original=False):
	dir = Path(env.config.patch_folder)
	fileName = utils.sanitiseSymbol(name) + ('.osail' if original else '.sail')
	return (dir / fileName)

def hasPatch(name, env):
	return getPatchPath(name, env).is_file()

def loadPatch(name, typ, env):
	body = getPatchPath(name, env).read_text()
	return SailPatchedDefinition(name, typ, body)

def translate(config):
	"""
	Creates the environment and initiates translation of the file specified in
	the configuration file.  Saves resulting ASTs to Sail files - output folder
	specified in the config.  Copies handwritten support files to output
	folder.
	"""
	env = Env(config)
	try:
		# Load, lex, parse and translate
		thisPath, thisFile = os.path.split(config.top_level_file)
		env.pushPath(thisPath)
		env.pushFile(thisFile[:-5])
		finalAST, _, _ = transformACL2FiletoSail(thisFile, env)

		# Save translated output
		saveSail(finalAST, config.output_folder, config.output_filename, env, includeHeaders=True)
		saveSail(env.auxiliaryFns, config.output_folder, 'auxiliary', env, includeHeaders=False)

		# Copy handwritten support
		for file in os.listdir('handwrittenSupport'):
			if file.endswith('.sail'):
				copyfile(
					src=os.path.join('handwrittenSupport', file),
					dst=os.path.join(config.output_folder, file))
	except:
		env.tidyUp()
		raise


def testACL2server():
	env = Env()
	try:
		env.evalACL2([':trans', ['GENERAL-PF-SPEC', '8', 'RESULT']], debracket=True)
	except:
		env.tidyUp()
		raise


if __name__ == '__main__':
	"""
	Main entry point
	"""
	parser = argparse.ArgumentParser(description='Translate ACL2 files into Sail')
	parser.add_argument('-c', '--config', default="config.toml")
	args = parser.parse_args()

	# Load config
	with open(args.config, "rb") as f:
		config = Config(tomli.load(f))

	# Reset unresolved types file
	with open(config.unresolved_types_file, 'w'):
		pass
	# Do main program
	translate(config)
