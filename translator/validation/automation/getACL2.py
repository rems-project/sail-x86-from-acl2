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

def printNiceError(errString):
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

def printError(fpath, string, ls):
	name = fpath.split('/')[-1]
	print(f"{name} {string}")
	# print("Last lines of Sail log read:")
	# print('\n\t'+ '\n\t'.join(ls[-5:]))

# Get the number of steps the Sail execution took
fault = True
with open(os.path.join(fpath, 'sailOut.log'), 'r') as f:
	# The number of lines should be on the penultimate non-empty line
	# But an error might have occurred
	ls = f.readlines()
	if ls == []:
		printError(fpath, 'blank', ls)
	elif not ls[-2].startswith('rip'):
		printError(fpath, printNiceError(ls[-2]), ls)
		# print(f"Error in Sail log file in: {fpath} - third last line starts with {ls[-3][:3]} not `rip`")
	elif not ls[-1].startswith('Steps = '):
		printError(fpath, 'notSteps', ls)
		# print(f"Error finding number of steps taken in: {fpath} - second last line not `steps`")
	else:
		numSteps = int(ls[-1].split(' ')[-1])
		fault = False

if fault:
	sys.exit()

# Read the instrument template file for ACL2
with open('acl2RunTemplate.lisp', 'r') as templateFile:
	template = templateFile.read()

lisp = template.format(
			"a.out",
			sa,
			numSteps)

# print(f"Generated ACL2 for {fpath}")
print(f"{fpath.split('/')[-1]} OK")

with open(outName, 'w') as outfile:
	outfile.write(lisp)