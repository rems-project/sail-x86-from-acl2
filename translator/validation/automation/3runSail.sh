#!/bin/bash

if [ -z "${KFSIT}" ]; then
	echo "Error: Set KFSIT environment variable."
	exit 1
fi

KFSIT=$(realpath ${KFSIT})

# Find runSail.sh files excluding some rep folders
SFILES=$(find ${KFSIT} -type f -name "runSail.sh")

for i in ${SFILES}; do
	# Split directory and filename.  Parentheses create a subshell
	DIR=$(dirname "${i}");

	# Run a Python script to generate the top level `instrument` file
	echo "Running ${DIR}"
	cd ${DIR}
	./runSail.sh > sailOut.log
done
