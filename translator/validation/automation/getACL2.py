import argparse
import os
import sys
import sharedFunctions

# This script take in a path to one of the K-Framework single instruction tests as input
# and produces a Lisp script to be piped into ACL2, specialised with the start address
# and number of steps to be taken.

# Get the file path from the command line
parser = argparse.ArgumentParser(description='Generate Setup Files')
parser.add_argument('fpath', type=str, nargs=1,
                   help='path to the directory with .elfs/.diselfs etc')
args = parser.parse_args()
fpath = args.fpath[0]

# Get the start address
sa, _ = sharedFunctions.getAddresses(fpath)

# Name we will use for the output file
outName = os.path.join(fpath, 'runACL2.lisp')

def formatError(errString):
	# Return a formatted error
	if errString.startswith('Translation error: two-byte opcode '):
		return f"NT 2 {errString[35:38]}"
	if errString in ['Model state error: Opcode Unimplemented in x86isa!\n', 'Model state error: Unimplemented exception in x86isa!\n']:
		return "MS-unimpl"
	if errString.startswith('Model state error: '):
		return f'MS-error "{errString[19:-1]}"'
	if errString == 'Error: vex decoding not implemented\n':
		return "VEX-unimpl"
	if errString.startswith("['X86-ILLEGAL-INSTRUCTION'"):
		return f'MS-fault illegal-instruction'
	return errString

def printStatus(fpath, string):
	name = fpath.split('/')[-1]
	print(f"{name} {string}")
	# print("Last lines of Sail log read:")
	# print('\n\t'+ '\n\t'.join(ls[-5:]))

# Get the number of steps the Sail execution took
status = 'OK'
with open(os.path.join(fpath, 'sailOut.log'), 'r') as f:
	# The number of lines should be on the penultimate non-empty line
	# But an error might have occurred
	ls = f.readlines()
	if ls == []:
		printStatus(fpath, 'blank')
		sys.exit()
	if not ls[-1].startswith('Steps = '):
		printStatus(fpath, 'notSteps')
		sys.exit()
	if not ls[-2].startswith('rip'):
		status = formatError(ls[-2])
	numSteps = int(ls[-1].split(' ')[-1])

# Read the instrument template file for ACL2
with open('acl2RunTemplate.lisp', 'r') as templateFile:
	template = templateFile.read()

lisp = template.format(
			"a.out",
			sa,
			numSteps)

# print(f"Generated ACL2 for {fpath}")
printStatus(fpath, status)

with open(outName, 'w') as outfile:
	outfile.write(lisp)
