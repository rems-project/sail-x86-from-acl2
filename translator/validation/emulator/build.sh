set -e

# Run Sail to create C file
${SAIL_DIR}/sail -c -c_coverage -c_include ${SAIL_DIR}/lib/sail_coverage.h ${x86SAIL_DIR}/instrument.sail -o emulator > all_branches

# Compile C file to emulator
gcc ${SAIL_DIR}/lib/*.c emulator.c ${SAIL_DIR}/lib/coverage/libsail_coverage.a -DHAVE_SETCONFIG -lgmp -lz -lpthread -ldl -I ${SAIL_DIR}/lib -o emulator
