import sys, os
sys.path.insert(1, os.path.join(sys.path[0], '..'))
import lex_parse

def convertNum(stringValue):
	if stringValue.startswith('#x'):
		return int(stringValue[2:], 16)
	else:
		return int(stringValue)

class ProcessorState:
	REGMAP = [
		('rax', 0),
		('rbx', 1),
		('rcx', 2),
		('rdx', 3),
		('rsi', 4),
		('rdi', 5),
		('rbp', 6),
		('rsp', 7),
		('r8', 8),
		('r9', 9),
		('r10', 10),
		('r11', 11),
		('r12', 12),
		('r13', 13),
		('r14', 14),
		('r15', 15),
	]
	def __init__(self):
		self.gprs = None
		self.rflags = None
		self.rip = None

		self.nameToNum = {}
		self.numToName = {}
		for (name, num) in ProcessorState.REGMAP:
			self.nameToNum[name] = num
			self.numToName[num] = name

	def stateFromACL2ast(self, ast):
		'''
		Args:
			ast: ACL2ast

		Returns:
			None
		'''
		# We have ACL2ast as:
		# [
		# 	['@@GPR', '.', [['#.*RAX', '.', 'value'], ...], '@@'],
		#	['@@RFLAGS', '.', 'value', '@@'],
		#	['@@RIP', '.', 'value', '@@']]
		# ]
		# Where `value` is either `#xnum` for a hex number `num` or just a number
		self.gprs = []
		for gpr in ast[0][2]:
			stringValue = gpr[2]
			value = convertNum(stringValue)
			self.gprs.append(value)

		rflagsStringValue = ast[1][2]
		self.rflags = convertNum(rflagsStringValue)

		ripStringValue = ast[2][2]
		self.rip = convertNum(ripStringValue)

	def stateFromACL2string(self, s):
		'''
		Args:
			s: str - the output from `(printing-x86-components x86 16 state)`

		Returns:
			None
		'''
		ACL2ast, index = lex_parse.parseACL2(tokens=lex_parse.lexLispString(s), index=0, level=0)
		self.stateFromACL2ast(ACL2ast)

	def stateFromSailString(self, s):
		"""
		We have the string:
			rax = 0x0000000000000002
			rbx = 0x0000000000000000
			...
			r15 = 0x0000000000000000
			rflags = 0x0000000000000002
			rip = 0x0000000000400662

		Args:
			s: str - the output from my Sail printing

		Returns:
			None
		"""
		lines = s.split("\n")

		self.gprs = []
		for gpr in lines[:-2]:
			stringValue = gpr.split(' ')[2][2:]
			self.gprs.append(int(stringValue, 16))

		rflagsStringValue = lines[-2].split(' ')[2][2:]
		self.rflags = int(rflagsStringValue, 16)

		ripStringValue = lines[-1].split(' ')[2][2:]
		self.rip = int(ripStringValue, 16)

	def printState(self):
		if self.gprs is not None:
			for (num, val) in enumerate(self.gprs):
				print(f"{self.numToName[num]}: {val}")

		if self.rflags is not None:
			print(f"rflags: {self.rflags}")

		if self.rip is not None:
			print(f"rip: {self.rip}")

	def __eq__(self, other):
		for (gpr, othergrp) in zip(self.gprs, other.gprs):
			if gpr != othergrp:
				return False

		if self.rflags != other.rflags:
			return False

		if self.rip != other.rip:
			return False

		return True

	def doublePrint(self, other):
		"""
		Assumes `other` has been initialised

		Args:
			other: ProcessorState

		Returns:
			None
		"""
		toPrint = []

		for (num, (val1, val2)) in enumerate(zip(self.gprs, other.gprs)):
			toPrint.append(f"{self.numToName[num]:6} {val1:19} {val2:19} {'X' if val1 != val2 else ''}")

		toPrint.append(f"rflags {self.rflags:19} {other.rflags:19} {'X' if self.rflags != other.rflags else ''}")

		toPrint.append(f"rip    {self.rip:19} {other.rip:19} {'X' if self.rip != other.rip else ''}")

		return toPrint

	def __hash__(self):
		return id(self)

class MemoryEvent:
	R = 0
	W = 1
	def __init__(self):
		self.ip = None
		self.type = None
		self.addr = None
		self.len = None
		self.data = None # Either result of a read or data for a write

		self.initialised = False

	def stateFromString(self, s):
		'''
		We have strings of the form:
			ip: R|W addr len data
		Where the numbers are decimal integers
		'''
		s = s.split()
		self.ip = int(s[0][:-1])
		type = s[1]
		if type == 'R':
			self.type = MemoryEvent.R
		elif type == 'W':
			self.type = MemoryEvent.W
		else:
			sys.exit(f"Error: unknown memory event type: {s}")
		self.addr = int(s[2])
		self.len = int(s[3])
		self.data = int(s[4])

		self.initialised = True

	def printState(self):
		if self.initialised:
			print(f"{self.ip}: {'R' if self.type == MemoryEvent.R else 'W'} {self.addr} {self.len} {self.data}")
		else:
			print("Error: memory event not initialised")

def statesFromACL2log(log):
	'''
	Takes an ALC2 log with states from multiple cycles and produces a list of ProcessorStates.

	Args:
		log: str - the read log file

	Returns:
		[ProcessorState]
	'''
	ast, _ = lex_parse.parseACL2(tokens=lex_parse.lexLispString(log), index=0, level=0)
	pss = []
	for i in range(0, len(ast), 3):
		ps = ProcessorState()
		ps.stateFromACL2ast(ast[i:i+3])
		pss.append(ps)

	return pss

def statesFromACL2file(file):
	'''
	Takes an ALC2 log file with states from multiple cycles and produces a list of ProcessorStates.

	Args:
		file: str - points to a log file produced by ACL2

	Returns:
		[ProcessorState]
	'''
	with open(file, 'r') as f:
		s = f.read()
		return statesFromACL2log(s)

def memoryFromACL2log(file):
	'''
	Takes an ACL2 file recording memory events from multiple cycles and produces a list of MemoryEvents

	Args:
		file: str - points to the log file

	Returns:
		[MemoryEvent]
	'''
	with open(file, 'r') as f:
		mes = []
		for line in f.readlines():
			if line != "\n":
				line = line[:-1] # Remove trailing '\n'
				me = MemoryEvent()
				me.stateFromString(line)
				mes.append(me)
	return mes

def statesAndMemoryFromSailLog(file):
	'''
	Unlike in ACL2 the Sail output is a single log with all the processor state and memory trace information dumped all
	in one.  Thus we have to deal with them together

	Args:
		file: str - Sail Log File

	Returns:
		([ProcessorState], [MemoryEvent])
	'''
	pss = []
	mes = []
	with open(file, 'r') as f:
		currLine = f.readline()
		while currLine != "":
			if currLine.startswith('rax'):
				stateString = currLine + "".join([f.readline() for _ in range(17)])
				stateString = stateString[:-1] # Remove trailing newline
				ps = ProcessorState()
				ps.stateFromSailString(stateString)
				pss.append(ps)
			elif currLine.startswith('[Sail]'):
				pass
			elif currLine.startswith('Steps'):
				pass
			elif currLine[0] in [str(i) for i in range(10)]: # I.e. first character is an integer
				me = MemoryEvent()
				me.stateFromString(currLine[:-1])
				mes.append(me)
			else:
				# print(f"Warning: Unknown start to line in Sail log: {currLine}")
				pass

			currLine = f.readline()

	return pss, mes

def mergeMEs(me1 : MemoryEvent, me2 : MemoryEvent):
	"""
	Two consecutive MEs can be merged if they have the same IP, same type, destination of #2 is destination of #1 plus
	length of #1.

	Args:
		me1: MemoryEvent
		me2: MemoryEvent

	Returns:
		MemoryEvent | None
	"""
	if me1.ip == me2.ip and me1.type == me2.type and me1.addr + me1.len == me2.addr:
		meRet = MemoryEvent()
		meRet.ip = me1.ip
		meRet.type = me1.type
		meRet.addr = me1.addr
		meRet.len = me1.len + me2.len
		meRet.data = (me1.data * (2**(8*me2.len))) + me2.data
		meRet.initialised = True
		return meRet

	return None

def mergeMElist(mes):
	if len(mes) <= 1:
		return mes
	me1 = mes[0]
	me2 = mes[1]
	meRet = mergeMEs(me1, me2)
	if meRet is None:
		return [me1] + mergeMElist(mes[1:])
	return [meRet] + mergeMElist(mes[2:])

def comparePSs(ACL2pss, sailPss, writeout=None):
	strings = []

	if len(ACL2pss) != len(sailPss):
		strings.append(f"Warning: Processor state lists not the same length.  ACL2: {len(ACL2pss)}; Sail: {len(sailPss)}.  Using the shorter of the two")
	minLength = min(len(ACL2pss), len(sailPss))
	ACL2pss = ACL2pss[:minLength]
	sailPss = sailPss[:minLength]

	for (i, (aps, sps)) in enumerate(zip(ACL2pss, sailPss)):
		thisString = []
		if aps != sps:
			thisString.append(f"Difference in state at cycle {i}:")
			thisString.extend(aps.doublePrint(sps))
			thisString.append("\n")
		for s in thisString:
			print(s)
		strings.extend(thisString)

	if writeout is not None:
		with open(writeout, 'w') as f:
			for s in strings:
				f.write(s)
				f.write("\n")

	print("If there was no output up till this point then the states were the same - well done.")


if __name__ == '__main__':
	# Parse ACL2 states
	ACL2pss = statesFromACL2file('add1example/OutputSampleACL2.log')

	# Parse ACL2 memory
	ACL2mes = memoryFromACL2log('add1example/acl2-instrument.log')

	# Parse Sail states and memory
	sailPss, sailMes = statesAndMemoryFromSailLog('add1example/OutputSampleSail.log')

	# Compare the processor states
	comparePSs(ACL2pss, sailPss)

	print("End")