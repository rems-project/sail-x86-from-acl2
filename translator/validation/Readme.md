# Readme

*Patrick Taylor, pjht2, 27.06.20*

This file provides instructions for compiling an emulator from the translated model, running and example on it and performing co-simulation with the K-Framework Single Instruction Tests.

[ToC]

## Environment Variables References

Many of scripts require various environment variables to be set.  Some of these are defined throughout these instructions, but this list serves as a reference.

* `SAIL_DIR` - directory to the Sail installation
* `X86EMU` - the executable emulator of the translated model
* `X86EMU_DIR` - the directory containing the executable emulator
* `x86SAIL_DIR` - the output directory of the translator where the Sail files can be found.
* `KFSIT` - directory of the K Framework Single Instruction Tests suite.
* `ACL2` - the ACL2 executable (normally called `saved_acl2`).

## Compiling the Model

1. Set the environment variables `SAIL_DIR` and `x86SAIL_DIR` then.

2. Connect to `emulator/`

3. Run `./build.sh` which runs Sail to produce the C code for the emulator before compiling it into an executable.  This will liekly take a few minutes.  The top level function is actually contained in `instrument.sail`, which was copied to the output folder when translation finished.  This file, and `instrumentFns.sail` instrument the model with function similar to those available in the ACL2 model.

4. Set `X86EMU` and `X86EMU_DIR`:

   ```
   export X86EMU=${PWD}/emulator
   export X86EMU_DIR=${PWD}
   ```

## Running an Example

This section uses the example in `add1example` where `add1.c` is a simple example which adds `1` to a number.

The instructions here a really just a condensed version of the instructions for simulating in the ACL2 model but translated for the Sail model.  Please do read the ACL2 instructions, found at: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____PROGRAM-EXECUTION

1. Compile the `.c` file using `gcc add1.c -o add1.elf`

2. Disassemble the result using `objdump -d -x add1.elf > add1.diself`

3. There are various parameters we need to be able to set for execution.  We will go through how to find there values now, then how to set them.

   1. When compiling with `gcc` there are many set up functions included in the ELF file.  We don't want to excute all of them (and trying to probably would not work).  Instead, let's execute just the `add1()` function.  In the disassembly we find the following line:

      ```
      40065d:	e8 94 ff ff ff       	callq  4005f6 <add1>
      ```

      This line calls the `add1()` function from `main()`, so we want to set the start address to 0x40065d.  However, we need to take into account x86 calling conventions, as described in the next points.

   2. The value passed to `add1()` is stored in `eax`.  Currently there is no way to set this as a model parameter (although we can add it in `instrument.sail`), so we'll content ourselves with adding `1` to `0`, the detaul value.

   3. Let's set the stack pointer to a reasonable value of `35184372088832`  ($2^{45}$) as recommended in the link above.

   4. Likewise, the link points out that the second bit of rflags should always be set, so we'll set rflags to `2`.

   5. Let's set the halt address to the one following the `callq` - thats `0x400662`.

   6. Finally, we need to tell the simulator the model should be in 64 bit mode.

4. To set all these parameters and run the simulation use:

   ```
   ${X86EMU} -e add1.elf -C rip=0x40065d -C rsp=35184372088832 -C rflags=2 -C ha=0x400662 -C set64bit=1
   ```

5. The state of each register at each step and a record of memory reads and writes should be printed.  The final line should read `Steps = 10`, indicating 10 cycles were simulated.  Also note the value in `rax` for the last step: `rax = 0x0000000000000001`.  This is the value returned from `add1()` and represents the calculation `0+1`, i.e. `1`.

6. This output was saved to `OutputSampleSail.log`.

Simulation in the ACL2 model can be performed by following instructions at hte link above.  A template for commands to submit to an ACL2 instance can be found in `automation/acl2RunTemplate.lisp`, where `{}` represent values for you to fill in.  For reference, the commands used to produce `OutputSampleACL2.log` in the ACL2 interpreter where:

```lisp
(include-book "projects/x86isa/tools/execution/top" :ttags :all :dir :system)
(in-package "X86ISA")
(binary-file-load "add1.elf")
(init-x86-state-64 
 ;; Status (MS and fault field) 
 nil 
 ;; Start Address --- set the RIP to this address 
 #x40065d
 ;; Initial values of General-Purpose Registers 
 '((#.*rsp* . #.*2^45*)) 
 ;; Control Registers: a value of nil will not nullify existing 
 ;; values. 
 nil 
 ;; Model-Specific Registers: a value of nil will not nullify existing 
 ;; values. 
 nil 
 ;; Segment Registers: a value of nil will not nullify existing values. 
 nil ; visible portion 
 nil nil nil ; hidden portion 
 ;; Rflags Register 
 2 
 ;; Memory image: a value of nil will not nullify existing values. 
 nil 
 ;; x86 state 
 x86)
(trace-all-reads)
(trace-all-writes)
(log-instr-fn 10 x86 state)
```

Note that the output was copied manually to `OutputSampleACL2.log`.

The results of the Sail and ACLA2 runs can be compared (performing co-simulation) by running the `analyseOutput.py` script.  Differences (non in this example) will be displayed.

## Co-Simulation

This section describes how to use the scripts included in `automation` for co-simulation of the K-Framework Single Instruction Tests.

**Setup**

1. Compile the model and set the relevent environment variables as described in "Compiling the Model" above.

2. Download the K-Framework project and set the environment variable to the 
   
   ```
   git clone https://github.com/kframework/X86-64-semantics.git
   export KFSIT=${PWD}/X86-64-semantics/tests/single-instruction-tests
   ```
   
3. Set the ACL2 environment variable.

4. Connect to `automation/` folder.

**Run Scripts**

1. `./1compileElf.sh` searches for `.s` files in `KFSIT` and, for each one, compiles it into and ELF files, `a.out` and then disassembles the result using `objdump` into `a.diself`.  Using `objdump` is important because some of the scripts us the contentes of the disassembly later.
2. `./2genSetup.sh` runs the Python script `genSetup.py` and, for each `.s` file in `KFSIT` generates a shell script, `runSail.sh`, which the next script uses.  This script uses the compiled emulator to run the `a.out` ELF compiled in step 1 and sets appropriate command line options (start address, halt address, stack pointer, contents of register rflags, 64 bit mode).
3. `./3runSail.sh` uses the scripts generated in step 2 to run the Sail simulations.  For each `.s` file a standard `sail_coverage` files is produced as well as `sailOut.log`.  This latter file records the register state at each step and memory reads/writes (implemented in `instrumentation.sail` and `instrumentationFns.sail`).
   1. Note that folders in `KFSIT` containing the string `rep_mov` are not run because of an error in the original ACL2 model which causes an infinite loop.
4. `./4genACl2.sh` is similar to step 2 but instead of creating scripts for the Sail simulations, creates scripts for the ACL2 simulations, `runSail.lisp`.  Scripts are not created for `rep_mov` instructions or for instructions which failed the Sail simulation in some way (e.g. encountering an unimplemented or untranslated instruction).  It uses `genACL2.py` to do this.
5. `./5runACL2.sh ` uses the scripts generated in step 4 to perform the ACL2 simulations, again, in parallel if possible.  A new ACL2 instance is started for each simulation, which takes some time.  This helps ensure no state leaks from one run to the next - some careful implementation could allow all simulations to be run in one instance (at the expense of parallelism).  `acl2Out.log` files are produced which track the register state state for each instruction for each simulation.  `acl2-instrument.log` files are also produced.
6. `./6analyse.sh` runs `analyse.py` which runs `../analyseOutput.py`.  Thisparses and compares the states in the log fiels generated from each simulation (in steps 3 and 5) and, for each pair, produces a file `analyseOut.log`.
   * If this file is empty it indicates that the register states at each step matched (co-simulation suceeded).  To find such files run `find . -iname "analyseOut.log" -size 0` from `KFSIT`.
   * If the file is non-empty the contents indicate the differences in co-cimulation.  To find such files run `find . -iname "analyseOut.log" -size +0` from `KFSIT`.
   * Memory reads/writes and not compared due to Lisp not reliably saving this data.
7. `./7concatCoverage.sh` collects all the generated `sail_coverage` files and concatenates them into `total_sail_coverage`.  It then runs the Sail coverage tool and saves the resulting html in `coverage_html/`.  It saves the reports text to `coverage_html/report.txt` and prints it to the terminal.

As mentioned in 'Running an Example', the above steps can be performed manually on specific examples.  In this case the functions in `analyseOutput.py` can be used to compare outputs.

Finally, `./clean.sh` removes all of the generated files in the steps above.
