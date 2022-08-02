#!/bin/bash

if [ -z "${KFSIT}" ]; then
	echo "Error: Set KFSIT environment variable."
	exit 1
fi

KFSIT=$(realpath ${KFSIT})

# Add translator source code directory to Python path
TRANSLATOR_DIR=$(realpath $(pwd)/../..)
PYTHONPATH=${TRANSLATOR_DIR}:${PYTHONPATH}
export PYTHONPATH

# Find folders containing .s but check if the necessary log files are actually there in Python
SFILES=$(find ${KFSIT} -type f -name "acl2-instrument.log")

for i in ${SFILES}; do
	DIR=$(dirname ${i})
	echo ${DIR}

	python3 analyse.py ${DIR}
done;
