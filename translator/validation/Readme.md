# Readme

*Patrick Taylor, pjht2, 27.06.20*

This file provides instructions for compiling an emulator from the translated model, running and example on it and performing co-simulation with the K-Framework Single Instruction Tests.

[ToC]

## Environment Variables References

Many of scripts require various environment variables to be set.  Some of these are defined throughout these instructions, but this list serves as a reference.

* `SAIL_DIR` - directory to the Sail installation
* `X86EMU` - the executable emulator of the translated model
* `X86EMU_DIR` - the directory containing the executable emulator
* `X86SAIL_DIR` - the output directory of the translator where the Sail files can be found.
* `KFSIT` - directory of the K Framework Single Instruction Tests suite.
* `ACL2` - the ACL2 executable (normally called `saved_acl2`).

## Compiling the Model

Install [Sail](https://github.com/rems-project/sail), if necessary, as described [here](https://github.com/rems-project/sail/blob/sail2/INSTALL.md).  These instructions were last tested with Sail git revision `4f8532ae`, built from source.

Run `make x86_emulator` in the `model/` directory of this repository to build an emulator from the model snapshot, or if you want to build from a different version of the Sail sources, point the `X86SAIL_DIR` variable to it and run `make` in the `emulator` directory.

Set the `X86EMU` environment variable to the path of the generated emulator, e.g. `$THIS_REPO/model/x86_emulator`.

## Running an Example

This section uses the example in `add1example/` where `add1.c` is a simple example which adds `1` to a number.

The instructions here are really just a condensed version of the instructions for simulating in the ACL2 model but translated for the Sail model.  Please do read the ACL2 instructions, found at: <http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____PROGRAM-EXECUTION>

1. Compile the `.c` file using `gcc add1.c -o add1.elf`

2. Disassemble the result using `objdump -d -x add1.elf > add1.diself`

3. There are various parameters we need to be able to set for execution.  We will go through how to find there values now, then how to set them in the next step.

   1. When compiling with `gcc` there are many setup functions included in the ELF file.  We don't want to execute all of them (and trying to probably would not work).  Instead, let's execute just the `add1()` function.  In the disassembly we find the following line:

      ```
      40065d:	e8 94 ff ff ff       	callq  4005f6 <add1>
      ```

      This line calls the `add1()` function from `main()`, so we want to set the start address to 0x40065d.  However, we need to take into account x86 calling conventions, as described in the next points.

   2. The value passed to `add1()` is stored in `eax`.  Currently there is no way to set this as a model parameter (although we can add it in `instrument.sail`), so we'll content ourselves with adding `1` to `0`, the default value.

   3. Let's set the stack pointer to a reasonable value of `35184372088832`  ($2^{45}$) as recommended in the link above.

   4. Likewise, the link points out that the second bit of rflags should always be set, so we'll set rflags to `2`.

   5. Let's set the halt address to the address following the `callq` - i.e. `0x400662`.

   6. Some of the tests in the K framework test suite also need the control register bit `cr4.osfxsr` to be set;  otherwise, some instructions used in those tests fail.

   7. We need to tell the simulator the model should be in 64 bit mode.

   8. Finally, we need to either initialise the address translation tables, or tell the simulator to use the application-level view of the model;  we currently choose the latter.

4. To set all these parameters and run the simulation use:

   ```
   ${X86EMU} -e add1.elf -C rip=0x40065d -C rsp=35184372088832 -C rflags=2 -C ha=0x400662 -C cr4=512 -C set64bit=1 -C app_view=1
   ```

5. The state of each register at each step and a record of memory reads and writes should be printed.  The final line should read `Steps = 10`, indicating 10 cycles were simulated.  Also note the value in `rax` for the last step: `rax = 0x0000000000000001`.  This is the value returned from `add1()` and represents the calculation `0+1`, i.e. `1`.

6. A sample output is saved in `OutputSampleSail.log`.

Simulation in the ACL2 model can be performed by following the instructions at the link above.  A template for commands to submit to an ACL2 instance can be found in `automation/acl2RunTemplate.lisp`, where `{}` represent values for you to fill in.  For reference, the commands submited to the ACL2 interpreter to produce the output shown in `OutputSampleACL2.log` were:

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

The results of the Sail and ACL2 runs can be compared by running the `analyseOutput.py` script.  Differences (none in this example) will be displayed.

## Co-Simulation

This section describes how to use the scripts included in `automation/` for co-simulation of the K-Framework Single Instruction Tests.

**Setup**

1. Compile the model and set the relevant environment variables as described in "Compiling the Model" above.

2. Download the K-Framework project and set the `KSIT` environment variable. 
   
   ```
   git clone https://github.com/kframework/X86-64-semantics.git
   
   export KFSIT=${PWD}/X86-64-semantics/tests/single-instruction-tests
   ```
   
3. Set the `ACL2` environment variable.

4. Connect to `automation/` folder.

**Run Scripts**

1. `./1compileElf.sh` searches for `.s` files in `KFSIT`.  It compiles each one into an ELF, `a.out`, then disassembles the result into `a.diself` using `objdump`.  Using `objdump` is important because some of the scripts use the contents of the disassembly later.
2. `./2genSetup.sh` runs the Python script `genSetup.py` and, for each `.s` file in `KFSIT` generates a shell script, `runSail.sh`, which the next script uses.  This script uses the compiled emulator to run the `a.out` ELF compiled in step 1 and sets appropriate command line options (start address, halt address, stack pointer, contents of register rflags, 64 bit mode) derived from the `a.diself`.
3. `./3runSail.sh` uses the scripts generated in step 2 to run the Sail simulations in parallel.  For each `.s` file a standard `sail_coverage` file is produced as well as `sailOut.log`.  This latter file records the register state at each step and memory reads/writes (implemented in `instrument.sail` and `instrumentFns.sail`).
   1. Note that folders in `KFSIT` containing the string `rep_mov` are not run because of an error in the original ACL2 model which causes an infinite loop.
4. `./4genACl2.sh` is similar to step 2 but instead of creating scripts for the Sail simulations, creates scripts for the ACL2 simulations, `runSail.lisp`.  Scripts are not created for `rep_mov` instructions or for instructions which failed the Sail simulation in some way (e.g. encountering an unimplemented or untranslated instruction).  It uses `genACL2.py` to do this.
5. `./5runACL2.sh ` uses the scripts generated in step 4 to perform the ACL2 simulations, again, in parallel if possible.  A new ACL2 instance is started for each simulation, which takes some time.  This helps ensure no state leaks from one run to the next - some careful implementation could allow all simulations to be run in one instance (at the expense of parallelism).  `acl2Out.log` files are produced which track the register state state for each instruction for each simulation.  `acl2-instrument.log` files are also produced.
6. `./6analyse.sh` runs `analyse.py` which runs `../analyseOutput.py`.  This parses and compares the states in the log files generated from each simulation (in steps 3 and 5) and, for each pair, produces a file `analyseOut.log`.
   * If this file is empty it indicates that the register states at each step matched (co-simulation succeeded).  To find such files run `find ${KFSIT} -iname "analyseOut.log" -size 0`.
   * If the file is non-empty the contents indicate the differences in co-simulation.  To find such files run `find ${KFSIT} -iname "analyseOut.log" -size +0`.
   * Memory reads/writes and not compared due to Lisp not reliably saving this data.
7. If the tests were run with an emulator that has support for generating model coverage information (not tested with current versions of Sail), then `./7concatCoverage.sh` collects all the generated `sail_coverage` files and concatenates them into `total_sail_coverage`.  It then runs the Sail coverage tool and saves the resulting html in `coverage_html/`.  It saves the reports text to `coverage_html/report.txt` and prints it to the terminal.

As mentioned in 'Running an Example', the above steps can be performed manually on specific examples.  In this case the functions in `analyseOutput.py` can be used to compare outputs.

Finally, `./clean.sh` removes all of the generated files in the steps above.
