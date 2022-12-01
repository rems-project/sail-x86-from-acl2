import sys

"""
This file contains functions for lexing and parsing Lisp/ACL2 code and classes
for representing ACL2 ASTs.

Important functions are:
 -  lexLispFile
 -  lexLispString
 -  parseACL2
 -  ASTprinter
 -  pp_acl2
"""

################################################################################
# Classes
################################################################################


class CodeTopLevel:
	"""
	Class representing a code file for both ACL2 and Sail.
	TODO: maybe derive from List
	"""
	def __init__(self, topLevels):
		"""
		Args:
			 - topLevels : [ACL2astElem] | [SailASTelem]
		"""
		self.topLevels = topLevels

	def first(self):
		if len(self.topLevels) != 0:
			return self.topLevels[0]
		else:
			return None

	def rest(self):
		if len(self.topLevels) != 0:
			return CodeTopLevel(self.topLevels[1:])
		else:
			return None

	def append(self, item):
		self.topLevels.append(item)

	def extend(self, item):
		self.topLevels.extend(item)

	def __len__(self):
		return len(self.topLevels)


class ACL2Comment:
	"""Class for representing Lisp comments"""
	def __init__(self, comment):
		"""
		Args:
			- comment : str
		"""
		self.comment = comment

	def __str__(self):
		return "COMMENT: {}".format(self.comment)

	def __repr__(self):
		"""TODO: remove"""
		return self.__str__()

	def getComment(self):
		return self.comment

	def pp(self):
		"""
		TODO: the ACL2Comment object should really not be present in the Sail
		ast, but, for now, it is, hence this function is necessary when pretty
		printing a Sail ast.
		"""
		return f"/*! {self.comment} */"


class ACL2String:
	"""Class for representing ACL2 strings"""
	def __init__(self, string):
		"""
		Args:
			- string : str
		"""
		self.string = string

	def __str__(self):
		return "STRING: {}".format(self.string)

	def __repr__(self):
		"""TODO: remove"""
		return self.__str__()

	def __eq__(self, other):
		if type(self) != type(other):
			return False
		return self.string == other.getString()

	def __hash__(self):
		return id(self)

	def getString(self):
		return self.string


class NewLine:
	"""
	Class for newlines.  Originally intended for handling inline
	comments, however now filtered out of ASTs.
	"""
	def __init__(self):
		pass

	def __str__(self):
		return "\n"


class ACL2quote:
	"""Class for quotes"""
	def __init__(self):
		self.ast = None

	def __str__(self):
		return "QUOTE:"

	def setAST(self, ast):
		"""
		Either a bracketed expression or a symbol can be quoted

		Args:
			- ast : [ACL2astElem] | str
		"""
		self.ast = ast

	def getAST(self):
		return self.ast


class ACL2qq:
	"""Class for quasi quotes"""
	def __init__(self):
		self.ast = None
	def __str__(self):
		return "QQ:"

	def setAST(self, ast):
		"""
		Either a bracketed expression or a symbol can be QQed

		Args:
			- ast : [ACL2astElem] | str
		"""
		self.ast = ast

	def getAST(self):
		return self.ast


################################################################################
# Functions
################################################################################


def lexLispString(rawLisp):
	"""
	Use technique here (https://norvig.com/lispy.html) as a simple lexer.

	Args:
		- rawLisp : string
	Returns:
		- [str] : lisp split into tokens

	TODO:
		- Implement a proper lexer.  The current implementation works but could
		be more robust and complete.  E.g. handling the presence of \" in
		strings quoted with quotations.
	"""
	padded = rawLisp.replace('(', ' ( ',) \
					.replace(')', ' ) ',) \
					.replace("'", " ' ") \
					.replace('\\"', '\'') \
					.replace('"', ' " ') \
					.replace("`", " ` ") \
					.replace(";;", " ;; ") \
					.replace("\t", "    ") \

	spaceSplit = padded.split(' ')

	# Also split on newlines, retaining adding in a Newline object to
	# represent where they were
	nlSplit = []
	for item in spaceSplit:
		splitItem = item.split("\n")
		toAdd = []
		for subitem in splitItem[:-1]:
			toAdd.extend([subitem, NewLine()])
		toAdd.append(splitItem[-1])
		nlSplit.extend(toAdd)

	# Remove extraneous empty strings
	return [x for x in nlSplit if x != '']

def lexLispFile(file):
	"""
	Wrapper to lex from a file rather than a string.

	Args:
		- file : string
	Return:
		- [str] : lisp file split into tokens
	"""
	with open(file, 'r') as f:
		return lexLispString(f.read())

def parseACL2Comment(tokens, index):
	"""
	Args:
		- tokens : [str].  The full list of tokens.
		- index : int.  The index of the ';' or ';;' or '#||' token
	Returns:
		- (comment : ACL2Comment, index' : int).
		  The parsed comment and index into the remaining tokens
	"""
	# Single line comment
	if tokens[index] in [';', ';;']:
		stopPred = lambda token: isinstance(token, NewLine)
	elif tokens[index] == '#||':
		stopPred = lambda token: token == '||#'
	else:
		sys.exit(f"Error: parseACL2Comment called with incorrect first token - {tokens[index]}")

	# Parse the comment
	comment = []
	end = False
	runningIndex = index + 1
	while not end:
		nextWord = tokens[runningIndex]
		if stopPred(nextWord) or runningIndex == len(tokens) - 1:
			end = True
		else:
			comment.append(nextWord)
		runningIndex += 1

	# Replace NewLines with "\n" if we're parsing a multiline comment
	if tokens[index] == '#||':
		comment = [i if not isinstance(i, NewLine) else "\n" for i in comment]

	# Return
	return ACL2Comment(' '.join(comment)), runningIndex

def parseACL2String(tokens, index):
	"""
	Args:
		- tokens : [str].  The full list of tokens.
		- index : int.  The index of the '"' token
	Returns:
		- (string : ACL2String, index' : int).
		  The parsed string and index into the remaining tokens

	TODO: some strings contain \", which is currently handled above when padding during in lexing.
	"""
	string = []
	end = False
	index += 1
	while not end:
		nextWord = tokens[index]
		if nextWord == "\"":
			end = True
		else:
			string.append(nextWord)
		index += 1

	# Convert NewLines objects to strings
	string = [s.__str__() for s in string]
	return ACL2String(' '.join(string)), index

def parseACL2Quote(tokens, index):
	"""
	This function only adds in an empty quote object to the AST -
	encapsulation is done at a later stage

	Args:
		- tokens : [str].  	The full list of tokens.  Not used but required as
							this function is passed around.
		- index : int.  The index of the "'" or 'quote' token
	Returns:
		- (quote : ACL2quote, index' : int = index + 1)
		  An empty quote object and the index incremented
	"""
	return ACL2quote(), index + 1

def parseACL2QQ(tokens, index):
	"""
	This function only adds in an empty QQ object to the AST -
	encapsulation is done at a later stage

	Args:
		- tokens : [str].  	The full list of tokens.  Not used but required as
							this function is passed around.
		- index : int.  The index of the "`"
	Returns:
		- (quote : ACL2qq, index' : int = index + 1)
		  An empty QQ object and the index incremented
	"""
	return ACL2qq(), index + 1

def encapsulateQuotes(ast):
	"""
	Takes a parsed AST with empty quote/QQ objects and encapsulates the
	succeeding symbols at the top level into them.

	Args:
		- ast : [ACL2astElem]
	Returns:
		- ast' : [ACL2astElem]

	TODO:
		- Deal properly with double quotes.  The current model only uses double
		quotes in theorems, which we ignore.
	"""
	# Test for an empty AST first
	if len(ast) == 0:
		return []

	# If not empty, continue
	newAST = []
	index = 0
	end = False
	while not end:
		# Get astElem and deal with it
		astElem = ast[index]
		inc = 1

		# Encapsulate the next real symbol
		if type(astElem) in [ACL2quote, ACL2qq]:
			# Get the next astElem
			nextAstElem = ast[index+1]
			# If it's blatantly the wrong type, raise an error
			if type(nextAstElem) in [ACL2quote, ACL2qq]:
				print("WARNING: next symbol after quote/QQ is another quote/QQ - this is not implemented properly yet")
			elif type(nextAstElem) not in [list, str]:
				print("\n")
				print(ast)
				sys.exit("Error: next symbol after a quote/QQ not a list or symbol")
			# Add the next AST to the quote object and add to the new AST
			astElem.setAST(nextAstElem)
			newAST.append(astElem)
			# We've consumed two objects so set increment accordingly
			inc = 2
		# Just add anything else
		else:
			newAST.append(astElem)

		# Increment index
		index += inc

		# Test for end
		if index == len(ast):
			end = True
		if index > len(ast):
			sys.exit("Error: overran AST when encapsulating quotes")

	return newAST

def consumeSpecialToken(token):
	"""
	Consumes a token with which we want to do something special using the
	functions above.

	Args:
		- token : str.  The current token
	Returns:
		- (	success : Bool,
			fn : 	(tokens : [str], index : int)
					-> (ACL2astElem, index' : int))
	"""
	tokenSwitchTable = {
		";" : parseACL2Comment,
		";;": parseACL2Comment,
		"#||":parseACL2Comment,
		"\"": parseACL2String,
		"'": parseACL2Quote,
		"quote": parseACL2Quote,
		"`": parseACL2QQ
	}
	if token in tokenSwitchTable:
		return True, tokenSwitchTable.get(token)
	else:
		return False, None

def parseACL2(tokens, index, level):
	"""
	Takes the tokens and transforms them into an AST based on bracketing
	and comments.

	Args:
		- tokens : [str].  The tokenised LISP
		- index : int.  Index from which to parse
		- level : current recursion level
	Return:
		- (ast : [ACL2astElem], index' : int).
		  AST only - no meaning or types yet.  New index.

	TODO:
		- Maybe include encapsulation of quotes/QQs in their respective
		parsing functions, rather than a dedicated pass through the AST.
	"""

	# First pass - basic parsing
	ast = []
	end = False
	while not end:
		currentToken = tokens[index]
		# Deal with special tokens
		(special, fn) = consumeSpecialToken(currentToken)
		if special:
			(result, newIndex) = fn(tokens, index)
			ast.append(result)
			index = newIndex
		# Deal with open bracket
		elif currentToken == "(":
			(result, newIndex) = parseACL2(tokens, index + 1, level + 1)
			ast.append(result)
			index = newIndex
		# Deal with close bracket
		elif currentToken == ")":
			if level == 0:
				print("ERROR: source ill-bracketed (found close bracket at level 0)")
				print("Previous tokens: {}".format(tokens[index-10:index]))
				print("Next tokens: {}".format(tokens[index:index+10]))
			index += 1
			end = True
		# Deal with everything else
		else:
			ast.append(currentToken)
			index += 1
		# Test if we need to end
		if index >= len(tokens):
			end = True
			if level != 0 and currentToken != ")":
				print("ERROR: source ill-bracketted (reached end without close bracket)")
				print(f"Level: {level}")
				print("Previous tokens: {}".format(tokens[index-10:index]))
				print("Next tokens: {}".format(tokens[index:index+10]))

	# Second pass: scan the current level of the AST for newlines and
	# comments and remove them
	ast = [a for a in ast if type(a) not in [ACL2Comment, NewLine]]

	# Third pass: scan the current level of the AST for quotes and QQs,
	# encapsulating the next 'real' ACL2 symbol in the relevant quote/QQ
	# object.  Because parseACL2 is, itself, recursive, quotes/QQs lower in
	# the hierarchy will have been handled already.
	ast = encapsulateQuotes(ast)

	return ast, index

def ASTprinter(ast, level):
	"""
	Prints parsed AST with an indent at each level.  Items can be an
	ACL2Comment, an ACL2String, a list, a symbol (i.e. str) or a Newline

	Args:
		- ast : [ACL2astElem]
		- level : int.  Current bracket level
	Returns:
		- None

	TODO: update this with new ACL2 AST classes.
	"""
	for x in ast:
		# List (recurse)
		if isinstance(x, list):
			ASTprinter(x, level+1)
		# NewLine (ignore)
		elif isinstance(x, NewLine):
			pass
		# Quotes and QQs
		elif isinstance(x, ACL2quote) or isinstance(x, ACL2qq):
			print(" "*level + x.__str__())
			ASTprinter([x.getAST()], level+1)
		# ACL2Comment, ACL2String, symbol (just print)
		else:
			print(" "*level + x.__str__())

def pp_acl2(ACL2ast):
	"""
	Create a concrete string from an AST structure

	Args:
		- ACL2ast : [ACL2astElem]
	Returns:
		- str
	"""
	toJoin = []
		
	# List (recurse)
	if isinstance(ACL2ast, list):
		toJoin.append(f"({' '.join([pp_acl2(item) for item in ACL2ast])})")
	# NewLine (ignore)
	elif isinstance(ACL2ast, NewLine):
		toJoin.append('\n')
	# ACL2Comment
	elif isinstance(ACL2ast, ACL2Comment):
		toJoin.append(f'; {ACL2ast.getComment()}\n')
	# ACL2String
	elif isinstance(ACL2ast, ACL2String):
		toJoin.append(f'"{ACL2ast.getString()}"')
	# Symbol (i.e. str)
	elif isinstance(ACL2ast, str):
		toJoin.append(ACL2ast)
	# Quote
	elif isinstance(ACL2ast, ACL2quote):
		toJoin.append("'")
		toJoin.append(pp_acl2(ACL2ast.getAST()))
	# QQ
	elif isinstance(ACL2ast, ACL2qq):
		toJoin.append("`")
		toJoin.append(pp_acl2(ACL2ast.getAST()))
	# Otherwise
	else:
		print("Error: unexpected type in ACL2 ast in pp_acl2(): {}".format(type(ACL2ast)))
		sys.exit(1)

	return ''.join(toJoin)

def test():
	tokens = lexLispFile('x86.lisp')
	(ast, _) = parseACL2(tokens, 0, 0)
	ASTprinter(ast, 0)
	return tokens, ast

if __name__ == '__main__':
	test()