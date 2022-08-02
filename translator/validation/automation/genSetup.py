import argparse
import os
from shutil import copyfile
import sharedFunctions

# This script take in a path to one of the K-Framework single instruction
# tests and a path the the compiler C x86 eulator.  It  produces a shell
# script to invoke the emulator.  It passes in values for rsp, rflags, and
# specialised values for start and halt addresses which are read from the
# disassembled elf.

# Get the file path from the command line
parser = argparse.ArgumentParser(description='Generate script to invoke C emulator')
parser.add_argument('fpath', type=str, nargs=1,
                  	help='path to the directory with .elfs/.diselfs etc')
parser.add_argument('emupath', type=str, nargs=1,
					help='path to the compiled C x86 emulator')
args = parser.parse_args()
fpath = args.fpath[0]
emu_path = args.emupath[0]

# Get the start and halt addresses
sa, ha = sharedFunctions.getAddresses(fpath)

# Generate the shell command
command = "{} -e a.out -C rip={} -C rsp=35184372088832 -C rflags=2 -C ha={} -C set64bit=1 -C app_view=1  -C cr4=512".format(
	emu_path,
	sa,
	ha,)

print(f"Generated shell command for {fpath}")
with open(os.path.join(fpath, 'runSail.sh'), 'w') as outfile:
	outfile.write(command)
