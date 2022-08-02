#!/bin/bash

if [ -z "${KFSIT}" ]; then
	echo "Error: Set KFSIT environment variable."
	exit 1
fi

KFSIT=$(realpath ${KFSIT})

# Find the runACL2.lisp files, excluding some rep folders
SFILES=$(find ${KFSIT} -type f -name "runACL2.lisp")

if [ -z "${ACL2}" ]; then
	echo "Error: Set ACL2 environment variable."
	exit 1
fi

# parallel --lb 'echo {#}; echo {//}; cd {//}; ACL2=${ACL2}; cat runACL2.lisp | $ACL2 > acl2Out.log' ::: ${SFILES}

for i in ${SFILES}; do
	# Split directory and filename.  Parentheses create a subshell
	DIR=$(dirname "${i}");

	echo ${DIR}

	# Remove a previously generated file
	rm -f ${DIR}/acl2-instrument.log

	cd ${DIR}
	${ACL2} < ${i} > /dev/null
done;
