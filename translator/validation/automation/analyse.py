import argparse
import os
import sys
import analyseOutput

# Get the file path from the command line
parser = argparse.ArgumentParser(description='Generate Setup Files')
parser.add_argument('fpath', type=str, nargs=1,
                   help='path to the directory with .elfs/.diselfs etc')
args = parser.parse_args()
fpath = args.fpath[0]
print(f"Path: {fpath}")

# Check the log files exist
sailPath = os.path.join(fpath, 'sailOut.log')
acl2Path = os.path.join(fpath, 'acl2-instrument.log')

if not os.path.exists(sailPath):
	sys.exit(f"The Sail log does not exist for: {fpath}")
if not os.path.exists(acl2Path):
	sys.exit(f"The ACl2 log does not exist for: {fpath}")

with open(acl2Path, 'r') as f:
	acl2Log = ''.join(f.readlines())

# Analyse
ACL2pss = analyseOutput.statesFromACL2log(acl2Log)
sailPss, _ = analyseOutput.statesAndMemoryFromSailLog(sailPath)
analyseOutput.comparePSs(ACL2pss, sailPss, os.path.join(fpath, "analyseOut.log"))
