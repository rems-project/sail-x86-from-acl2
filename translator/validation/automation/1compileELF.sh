# Change into the single instruction tests directory
cd ${KFSIT}

# Find .s files and generate list of a.out paths
SFILES=$(find . -type f -iname "*.s")
aoutFILES=$(echo "${SFILES}" | sed 's/test\.s$/a.out/')

# Compile and disassemble the .s files
for i in ${SFILES}; do

	# Split directory and filename.
	DIR=$(dirname "${i}");
	NAME=$(basename "${i}") # Should be `test.s`

	# Compile with gcc
	echo "Compiling ${DIR}/${NAME}"
	gcc -nostartfiles ${DIR}/${NAME} -o ${DIR}/a.out

	# Disassemble with start address information
	objdump -d -x ${DIR}/a.out > ${DIR}/a.diself
done;
