# Find .s files
SFILES=$(find ${KFSIT} -type f -name "*.s")

for i in ${SFILES}; do
	# Split directory and filename.
	DIR=$(dirname "${i}");
	NAME=$(basename "${i}") # Should be `test.s`

	# Run a Python script to generate the top level `instrument` file
	python3 genSetup.py ${DIR} ${X86EMU}

	# Make it executable
	chmod +x ${DIR}/runSail.sh
done;
