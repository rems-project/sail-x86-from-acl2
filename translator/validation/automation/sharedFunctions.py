import os

def getAddresses(fpath):
	# Open the diself file
	with open(os.path.join(fpath, 'a.diself'), 'r') as f:
		# Start address is on the sixth line so read five first
		[f.readline() for _ in range(5)]
		saLine = f.readline()

		# We have something like 'start address 0x0000000000400233'
		# Extract the start addr from this and convert to an integer
		sa = saLine[14:]
		sa = int(sa, 16)

		# The last instruction should be a nop and be represented on the 
		# penultimate line of the disassembly as:
		#   400c76:	90                   	nop
		rest = f.readlines()
		ha = int(rest[-1].split(':')[0], 16)

	return sa, ha