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

ISLA_SAIL=isla-sail

PRELUDE_SAILS=prelude.sail register_types.sail registers.sail register_accessors.sail opcode_ext.sail memory_accessors.sail init.sail config.sail logging.sail
MODEL_SAILS=step.sail
MAIN_SAIL=main.sail
ALL_SAILS=$(PRELUDE_SAILS) $(MODEL_SAILS) $(MAIN_SAIL)

TRANSLATOR_OUTPUT=../output

CC=gcc

GMP_FLAGS = $(shell pkg-config --cflags gmp)
# N.B. GMP does not have pkg-config metadata on Ubuntu 18.04 so default to -lgmp
GMP_LIBS := $(shell pkg-config --libs gmp || echo -lgmp)

ZLIB_FLAGS = $(shell pkg-config --cflags zlib)
ZLIB_LIBS = $(shell pkg-config --libs zlib)

all: x86_emulator

.PHONY: all interactive update_snapshot

x86_emulator.c: $(ALL_SAILS)
	$(SAIL) -c -memo_z3 $(SAIL_FLAGS) $(ALL_SAILS) > x86_emulator.c.temp
	mv x86_emulator.c.temp x86_emulator.c

x86_emulator: x86_emulator.c
	$(CC) -O2 -DHAVE_SETCONFIG x86_emulator.c $(SAIL_DIR)/lib/*.c $(GMP_FLAGS) $(GMP_LIBS) $(ZLIB_FLAGS) $(ZLIB_LIBS) -I $(SAIL_DIR)/lib/ -o x86_emulator

x86.ir: $(ALL_SAILS)
	$(ISLA_SAIL) $(foreach file,$(wildcard ../test-generation-patches/*.sail),-splice $(file)) -verbose 1 -o x86 $(ALL_SAILS)

interactive:
	$(SAIL) -i -memo_z3 $(SAIL_FLAGS) $(ALL_SAILS)

update_snapshot:
	mkdir -p $(TRANSLATOR_OUTPUT)/format
	cd $(TRANSLATOR_OUTPUT); $(SAIL) -output_sail -output_sail_dir format $(ALL_SAILS)
	cp $(TRANSLATOR_OUTPUT)/format/*.sail .
	cp ../translator/handwrittenSupport/*.sail .
	# Add ifdefs to structures.sail to support multiple inclusions
	echo "\$$ifndef __X86_STRUCTURES" > structures.sail.header
	echo "\$$define __X86_STRUCTURES" >> structures.sail.header
	cat structures.sail.header structures.sail > structures.sail.tmp
	echo "\$$endif" >> structures.sail.tmp
	mv structures.sail.tmp structures.sail
	rm structures.sail.header
