# Attempt to work with either sail from opam or built from repo in SAIL_DIR
ifneq ($(SAIL_DIR),)
# Use sail repo in SAIL_DIR
SAIL:=$(SAIL_DIR)/sail
export SAIL_DIR
else
# Use sail from opam package
SAIL_DIR:=$(shell opam config var sail:share)
SAIL:=sail
endif

PRELUDE_SAILS=prelude.sail register_types.sail registers.sail register_accessors.sail memory_accessors.sail opcode_ext.sail
MAIN_SAIL=main.sail

X86SAIL_DIR=../../../output

X86SAIL_FILES=$(addprefix $(X86SAIL_DIR)/,$(PRELUDE_SAILS) $(MAIN_SAIL))

CC=gcc

all: x86_emulator

x86_emulator.c: $(X86SAIL_FILES)
	$(SAIL) -c -memo_z3 $(SAIL_FLAGS) $^ > x86_emulator.c.temp
	mv x86_emulator.c.temp x86_emulator.c

x86_emulator: x86_emulator.c
	$(CC) -O2 -DHAVE_SETCONFIG x86_emulator.c $(SAIL_DIR)/lib/*.c -lgmp -lz -I $(SAIL_DIR)/lib/ -o x86_emulator
