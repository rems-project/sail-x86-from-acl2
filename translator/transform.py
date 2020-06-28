from lex_parse import lexLispFile, lexLispString, parseACL2, NewLine, ACL2Comment, ACL2String, pp_acl2, ACL2quote, \
	CodeTopLevel
from SailASTelems import *
import socketFuncs
import config_files
import config_patterns
import config_function_maps
import manualInterventions
import utils

import sys
import os
from shutil import copyfile
import socket

'''
TODO:
	* Deal with collisions in environment (e.g. trying to add a symbol which
	  already exists)
	* Deal with converting to upppercase (i.e. when to do so)
	* Deal with replacing problematic stuff with underscores
	* How to deal with non-trivial coments (e.g. at the end of lines; interleaved with other lines)
'''




class Env():
	"""Running environment for AST transformation"""

	def __init__(self):
		'''
		The functions contained in each environment must have the type:

			(ACL2ast : [ACL2astElem],
			env : Env)
			-> (Sailast' : [SailastElem],
				env' : Env,
				consumed : int)

		Where:
			ACL2astElem ::= 	ACL2Comment
							|	ACL2String
							|	[ACL2astElem]
							|	str
							|	NewLine
							|	ACL2quote
							|	ACL2qq
			`ast` is the ast rooted at the current symbol

		When an ACL2astElem is looked up, the environments are tested in the
		order below.

		TODO: it's more like manual vs auto envs than manual vs global
		'''
		self.globalEnv = {}
		self.manualEnv = manualCode.loadManualEnv()

		self.bindStack = []  # [(acl2token, sailtoken)]

		self.auxillaryInclude = set()
		self.auxillaryFns = []

		# TODO: merge these into a single stack
		self.path = ''
		self.pathStack = []
		self.fileStack = []

		# Contains a list of the files included thus far
		self.included = {}

		self.contextStack = []

		# Helps to resolve nil
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

		# Setup the connection to the ACL2 server
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			self.s.connect(('localhost', 1159))
		except:
			print("Warning: env could not connect to ACL2 server")

		# Open a new file with a list of unknown tokens
		# TODO: remove
		self.errorFile = open(exclusions.unknownTokensFile, "w")

		# A debugging variable containing the name of the current function being translated
		self.defineSlot = ""

	def printGlobalNames(self):
		print("Global names:")
		print("=============")
		for name in self.globalEnv:
			print(name)

		print("\nManual names:")
		print("=============")
		for name in self.manualEnv:
			print(name)

	def lookup(self, token, ignoreBindings=False):
		'''
		Args:
			token : str.
		Returns:
			fn : as above
		'''
		# If 't', return True and hope for the best
		if token.lower() == 't':
			return lambda _, env: ([SailBoolLit(True)], env, 1)
			# if self.currentType == SailPlaceholderNil.LIST:
			# 	return lambda _, env: ([manualCode.someHelper(SailStringLit("Empty error"))], env, 1)
			# elif self.currentType == None or self.currentType == SailPlaceholderNil.UNKNOWN or self.currentType == SailPlaceholderNil.BOOL:
			# 	return lambda _, env: ([SailBoolLit(True)], env, 1)
			# else:
			# 	sys.exit(f"Unknown current type when translating token 't`: {self.currentType}")

		# If 'nil' then return placeholder nil
		if token.lower() == 'nil':
			if self.currentType == None or self.currentType == SailPlaceholderNil.DEFAULT:
				return lambda _, env: ([SailPlaceholderNil()], env, 1)
			elif self.currentType == SailPlaceholderNil.BOOL:
				return lambda _, env: ([SailPlaceholderNil(SailPlaceholderNil.BOOL)], env, 1)
			else:
				sys.exit(f"Unknown current type when translating token 'nil`: {self.currentType}")

		# If 'x86' then return 0 as a placeholder because we want to ignore it
		if token.lower() == 'x86':
			return lambda _, env: ([SailNumLit(0)], env, 1)

		# If the token is a keyword (i.e. starts with a colon) return a string and hope for the best
		# TODO: use enums instead
		if token.startswith(':'):
			return lambda _, env: ([SailStringLit(token.upper())], env, 1)

		# Convert the token to uppercase
		token = token.upper()

		# Check the local bindings - pick the last one to cope with re-binding
		if not ignoreBindings:
			for i in reversed(range(len(self.bindStack))):
				(acl2Token, sailToken) = self.bindStack[i]
				if acl2Token == token:
					return lambda _, env: ([sailToken], env, 1)

		# Check if the token is a number literal or a macro which translates into one
		# TODO: translate into Sail hex literals as appropriate
		if token.startswith('#.*') or (token.startswith('*') and token != '*') or token.startswith('#'): # Used for constants and, for example, hex numbers
			literal = self.evalACL2([':trans', token], debracket=True)
			literal = utils.convertLiteral(literal[0].getAST())
			return lambda _, env: ([SailNumLit(literal)], env, 1)

		maybeLiteral = utils.convertLiteral(token)
		if maybeLiteral is not None:
			return lambda _, env: ([SailNumLit(maybeLiteral)], env, 1)

		# Then check for automatically/manually defined symbols
		for e in [self.globalEnv, self.manualEnv]:
			if token in e:
				return e[token]

		# If lookup fails, inform the user and ask if we want to do something
		# about it
		print()
		print("Token lookup failed: {}".format(token))
		print(f"Current file: {self.fileStack[-1]}")
		print(f"DefineStack: {self.defineSlot}")
		print(f"File stack: {self.fileStack}")
		print(f"Context stack: {self.contextStack}")
		print()
		sys.exit(-1)

		resp = None
		while resp not in ['f', 's', 'e']:
			resp = input("Options: add as a function; add as a symbol; exit (f/s/e)? ")
		if resp == 'f':
			numOfArgs = None
			while numOfArgs == None:
				try:
					numOfArgs = int(input("Number of arguments: "))
				except:
					pass
			self.manualEnv[token] = manualCode.apply_fn_gen(
				manualTranslations.unimplementedFunctionGen(token, numOfArgs), numOfArgs)
			self.errorFile.write(f"{token}\t{numOfArgs}\n")
			return self.manualEnv[token]
		if resp == 's':
			self.manualEnv[token] = lambda _, env: ([ACL2String(token)], env, 1)
			self.errorFile.write(f"{token}\n")
			return self.manualEnv[token]
		if resp == 'e':
			self.errorFile.write(f"{token}\n")
			sys.exit(1)

	# === Environment manipulation
	def addToGlobal(self, token, fn):
		'''
		Adds value to global environment
		TODO: maybe make the function call explicit here (i.e. it's always going to be the __apply_fn)

		Args:
			- token : str
			- fn : as above
		'''
		token = token.upper()

		if token in self.globalEnv:
			print(f"Error: overwriting token: {token}")
			sys.exit(1)
		else:
			self.globalEnv[token] = fn

	def addToAuxillary(self, fn):
		'''
		Args:
			fn: [SailASTelem]

		Returns:
			None
		"""
		self.auxiliaryInclude.add(utils.sanitiseSymbol(self.getFile()))
		self.auxiliaryFns.extend(fn)

	# === Bindings stack
	def pushToBindings(self, tokens, types=None, customSail=None):
		'''
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
		'''
		# Check inputs
		if types != None and len(tokens) != len(types): sys.exit(
			f"Error: length of tokens doesn't equal length of types when pushing a binding - {tokens} and {types}")
		if customSail != None and len(tokens) != len(customSail): sys.exit(
			f"Error: length of tokens doesn't equal length of customSail when pushing a binding - {tokens} and {customSail}")
		if types != None and customSail != None: sys.exit(
			"Error: only one of `tokens` and `customSail` sould be not None when pushing a binding")

		# Add to the stack
		toReturn = []
		for (i, token) in enumerate(tokens):
			token = token.upper()

			if types != None:
				boundVar = SailBoundVar(token, types[i])
				self.bindStack.append((token, boundVar))
			elif customSail != None:
				self.bindStack.append((token, customSail[i]))
				boundVar = None
			else:
				boundVar = SailBoundVar(token)
				self.bindStack.append((token, boundVar))

			toReturn.append(boundVar)

		return toReturn

	def setBindingType(self, token, typ):
		'''
		Args:
			- token : str
			- typ : SailType

		TODO: remove
		'''
		# Extract the most recent sail item given the string token
		sailItem = [sailItem for (acl2Item, sailItem) in self.bindStack if acl2Item.upper() == token.upper()]
		sailItem = sailItem[-1]

		# Set the type
		sailItem.setType(typ)

	def popWithCheck(self, tokens):
		'''
		Args:
			- tokens: [str]
		'''
		# We don't want to reverse in place so copy() first
		tokens = tokens.copy()
		tokens.reverse()
		for token in tokens:
			token = token.upper()
			if self.bindStack[-1][0] != token:
				sys.exit(f"Error: token '{token}' is not at top of bind stack whilst trying to unbind it")
			self.bindStack.pop()

	# === Current path functions
	def getPath(self):
		return self.path

	def appendToPath(self, toAppend):
		'''
		Pushes the current path to the stack and appends the argument
		'''
		self.pathStack.append(self.path)
		self.path = os.path.join(self.path, toAppend)

	def popPath(self):
		self.path = self.pathStack.pop()

	def appendToFile(self, toAppend):
		'''
		Pushes the current filename to the stack
		'''
		self.fileStack.append(toAppend)

	def popFile(self):
		self.fileStack.pop()

	def getFile(self):
		return self.fileStack[-1]

	def addToIncluded(self, file, ast):
		if file in self.included:
			sys.exit("Error: trying to add aready included file")
		self.included[file] = ast

	def isIncluded(self, file):
		return (file in self.included, self.included.get(file, None))

	# === Current context functions
	def pushContext(self, ctx):
		self.contextStack.append(ctx)

	def popContext(self):
		return self.contextStack.pop()

	def peekContext(self):
		return self.contextStack[-1]

	def peekContext2(self):
		if len(self.contextStack) >= 2:
			return self.contextStack[-2]
		else:
			return None

	def getFullContext(self):
		return self.contextStack

	def setDefineSlot(self, item):
		self.defineSlot = item

	def getDefineSlot(self):
		return self.defineSlot

	# === Current type functions - used for resolveing some PlaceholderNil instances
	def setCurrentType(self, typ):
		'''
		Args:
			typ - should be one of the enums defined in SailPlaceholderNil
		'''
		self.currentType = typ

	def clearCurrentType(self):
		self.currentType = None

	def addToUnresolvedTypes(self, item):
		self.unresolvedTypes.append(item)

	def getUnresolvedTypes(self):
		return self.unresolvedTypes

	# === Request an evaluation from the ACL2 server
	def evalACL2(self, expr, debracket=False):
		'''
		Takes an acl2 expression, sends it to the acl2 server for evaluation
		there, and returns the lexed and parsed result, leaving the caller to
		deal with the actual translation.

		For example, from `rflags-spec.lisp`: `(cf-spec-gen-fn  8)`

		Args:
			- expr : [ACL2astElem]
		Returns:
			- [ACL2astElem]
		'''
		# Convert the AST into a concrete string
		toSend = pp_acl2(expr)
		if config_files.print_acl2_interactions:
			print("\n\n")
			print(f'With brackets: {toSend}')

		# Remove outer brackets if asked
		if debracket:
			toSend = toSend[1:-1]
			if config_files.print_acl2_interactions:
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
		if config_files.print_acl2_interactions:
			print(f"\nString response:\n{response}")

		# Lex and parse the response
		tokens = lexLispString(response)
		(ACL2ast, finalParseIndex) = parseACL2(tokens, 0, 0)
		if finalParseIndex != len(tokens):
			sys.exit("Parse error: did not reach or overran list of tokens in some generated code")

		return ACL2ast

	def evalACL2raw(self, toSend):
		socketFuncs.reliableSend(toSend, self.s)
		response = socketFuncs.reliableRecv(self.s)
		response = [b.decode("utf-8") for b in response]
		response = ''.join(response)
		return response

	def tidyUp(self):
		self.s.close()
		self.errorFile.close()


def saveSail(SailAST, path, name, env, includeHeaders):
	'''
	Args:
		- SailAST : [SailASTelems]
		- path : str
		- name : str - without extension
		- includeHeaders : bool
	'''
	print(f"Pretty printing and saving to path: {path}; name: {name}")
	# Get the pretty printed version
	pp = "\n".join([elem.pp() for elem in SailAST])

	# Print it
	# print('-' * 80)
	# print(pp)

	# Also output into a file
	with open(os.path.join(path, f"{sanatiseSymbol(name)}.sail"), 'w') as f:
		if includeHeaders:
			f.write("$ifndef _DEFAULT_DEC\n")
			f.write("\tdefault Order dec\n")
			f.write("$endif\n\n")

		f.write("$include <prelude.sail>\n")
		f.write("$include <string.sail>\n") # TODO: only include this when we need

		if name in env.auxillaryInclude:
			f.write('$include "auxillary.sail"\n')

		if includeHeaders:
			f.write('$include "handwritten2.sail"\n')
			f.write('$include "utils.sail"\n\n')

		f.write(pp)

		# A trailing new line is needed for, for example, file only containing `$include`s.
		f.write("\n")

		print(f"Successfully saved file path: {path}; name: {name}")


def listStartsWith(l, pattern, convertCase=True):
	'''
	Tests if `pattern` is the start of `l`
	Args:
		- l : list
		- pattern : list
	'''
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
	'''
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

	(SailAST, env, length) = transformACL2asttoSail(ACL2ast, env)

	##### Post-process the AST #####
	# Filter out 'None's
	SailAST = [item for item in SailAST if item != None]

	# Collect apps and lets
	def collectSet():
		predSet = set()
		for item in SailAST:
			# TODO: remove this hack by dealing with comments properly
			if isinstance(item, ACL2Comment):
				continue

			# Predicates which get apps and lets with unknown types in either their formals or actuals
			appPred = lambda e : isinstance(e, SailApp) and\
								 (any(a.getType().containsUnknown() for a in e.getActuals()) or\
								  e.getFn().getType().containsUnknown())
			letPred = lambda e : isinstance(e, SailLet) and\
								 (e.getVarName().getType().containsUnknown() or e.getExpr()[0].getType().containsUnknown())
			pred = lambda e : appPred(e) or letPred(e)

			# Collect such apps and lets
			predSet = predSet.union(item.getChildrenByPred(pred))

		return predSet

	# TODO: reset this file on each run of the whole program in a nicer way (currently in top level) and thread through.
	# TODO: or just remove wholsesale
	f = open(exclusions.unresolvedTypesFile, 'a')
	f.write(f"{file}\n")
	f.write(f"{'=' * len(file)}\n\n")
	for i in range(3):
		f.write(f"Still unresolved after {i} pass(es):\n----------------------------------\n")

		predSet = collectSet()

		for e in predSet:
			e.resolveTypes(f)

	f.close()

	return SailAST, env, length


def transformACL2asttoSail(ACL2ast, env):
	'''
	Args:
		- ACL2ast : [ACL2astElem] | str
		- env : Env.
	Returns:
		- (Sailast : [SailastElem],
		   env' : Env,
		   consumed : Int)
	'''
	# First check if there are any manual replacements
	(doReplace, replacement, env) = exclusions.replacePatterns(ACL2ast, env)
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
		if currFile in exclusions.include_list:
			# ... but we include some functions
			if not any(listStartsWith(currentItem, inc) for inc in exclusions.include_list[currFile]):
				print(f"Excluding a function from {currFile}")
				translate = False

			# ... we also add in some others
			# if currFile in exclusions.include_instead:
			# 	for repl in exclusions.include_instead[currFile]:
			# 		if listStartsWith(currentItem, repl):
			# 			currentItem2 = currentItem.copy()
			# 			for (i, item) in enumerate(exclusions.include_instead[currFile][repl]):
			# 				currentItem2[i] = item
			# 			currentItem = currentItem2
			# 			translate = True
			# 			break

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
		if any(listStartsWith(ACL2ast, ex) for ex in exclusions.exclusions_list):
			translate = False
			ACL2ast = [None]
		# Do translation if necessary
		else:
			# Perform the translation - switch on the first item
			firstItem = ACL2ast[0]
			fn = env.lookup(firstItem, ignoreBindings=len(ACL2ast) > 1)
			env.pushContext(firstItem)
			(SailItem, env, _) = fn(ACL2ast, env)
			env.popContext()
			SailAST.extend(SailItem)
	# NewLine (ignore)
	elif isinstance(ACL2ast, NewLine):
		pass
	# ACL2Comment
	elif isinstance(ACL2ast, ACL2Comment):
		SailAST.append(ACL2ast)
	# ACL2String
	elif isinstance(ACL2ast, ACL2String):
		return [SailStringLit(ACL2ast.getString())], env, 1
		# print("Error: unexpected ACL2String: {}".format(ACL2ast.__str__()))
		# sys.exit(1)
	# Symbol (i.e. str)
	# TODO: make this more aligned to actual symbol translation as opposed to functions
	elif isinstance(ACL2ast, str):
		fn = env.lookup(ACL2ast)
		env.pushContext(ACL2ast)
		(SailItem, env, _) = fn(ACL2ast, env)
		env.popContext()
		SailAST.extend(SailItem)
	# Quote - attempt to translate the contents / return as string
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
		return (SailAST, env, 1)
	# Otherwise
	else:
		print("Error: unexpected type in ACL2 ast: {}".format(type(ACL2ast)))
		sys.exit()

	return (SailAST, env, len(ACL2ast))

# def resolveNils(Sailast):
# 	# Go through the tree assigning parent pointers and collecting NILs along the way
# 	pass

def test():
	env = Env()
	globalEnvironment.globalEnv = env
	try:
		thisPath, thisFile = os.path.split(exclusions.translateFile)
		env.appendToPath(thisPath)
		env.appendToFile(thisFile[:-5])
		finalAST, _, _ = transformACL2FiletoSail(thisFile, env)

		# Save translated output
		saveSail(finalAST, config_files.outputFolder, 'out', env, includeHeaders=True)
		saveSail(env.auxiliaryFns, config_files.outputFolder, 'auxiliary', env, includeHeaders=False)

		# Copy handwritten support
		for file in os.listdir('handwrittenSupport'):
			if file.endswith('.sail'):
				copyfile(
					src=os.path.join('handwrittenSupport', file),
					dst=os.path.join(config_files.outputFolder, file))
	except:
		env.tidyUp()
		raise


def testACL2server():
	env = Env()
	try:
		# env.evalACL2(['cf-spec-gen-fn', '8'])
		env.evalACL2([':trans', ['GENERAL-PF-SPEC', '8', 'RESULT']], debracket=True)
	except:
		env.tidyUp()
		raise


if __name__ == '__main__':
	# Reset unresolvedtypes file
	with open(exclusions.unresolvedTypesFile, 'w'):
		pass
	# Do main program
	test()
