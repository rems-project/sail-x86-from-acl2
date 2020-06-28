set -e

# Find instrument.sail files excluding some rep folders
folder=${KFSIT}
SFILES=$(find ${folder} -type f -iname "runSail.sh" ! -path '*rep_mov*')

# Parallel version
# parallel --group 'echo {#}; echo {//}; rm -f {//}/runACL2.lisp; python3 getACL2.py {//}' ::: ${SFILES}

# Sequential version
for i in ${SFILES}; do
	# Split directory and filename.  Parentheses create a subshell
	DIR=$(dirname "${i}");
	NAME=$(basename "${i}") # Should be `test.s`

	# Remove a previously generated file
	rm -f ${DIR}/runACL2.lisp

	# Run a Python script to generate the top level `instrument` file
	python3 getACL2.py ${DIR}

	# Make it executable
	chmod +x ${DIR}/runSail.sh
done;