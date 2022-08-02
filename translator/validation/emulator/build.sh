set -e

# Run Sail to create C file
sail -c ${x86SAIL_DIR}/instrument.sail -o emulator

# Compile C file to emulator
gcc -g ${SAIL_DIR}/lib/*.c emulator.c -DHAVE_SETCONFIG -lgmp -lz -lpthread -ldl -I ${SAIL_DIR}/lib -o emulator
