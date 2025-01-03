# ACL2-to-Sail translator and the resulting Sail x86 ISA model

[ToC]

## Introduction

This project translates the [X86isa](https://www.cs.utexas.edu/~moore/acl2/v8-5/combined-manual/index.html?topic=ACL2____X86ISA) formal model of the x86 ISA, written in ACL2 by Shilpi Goel, Warren A. Hunt, Jr., and Matt Kaufmann, into [Sail](https://www.cl.cam.ac.uk/~pes20/sail/popl2019.html), a language designed for modelling ISA semantics.

The translator was originally developed by Patrick Taylor as a student project in 2020, with supervision by Alasdair Armstrong and Peter Sewell.  Since then, Thomas Bauereiss has updated and extended it, in particular extending the scope of the translation, making the data type annotations more precise, and making the generated Sail code more idiomatic.

This README provides a quick start guide for getting the translator working, before detailing the implementation.

Files and folders:

* `model/` contains a snapshot of the Sail model generated by the translator.
* `translator/` contains the translator code as well as validation scripts in `translator/validation/`.  The `Readme.md` file in the latter directory describes how to use the translated model.
* `doc/` contains documentation of the original prototype of the translator, in particular Patrick's dissertation in `doc/diss/latex/diss.pdf`.  Much of this still applies to the current version of the translator, but some aspects have changed;  for example, the type inference now generates fixed-width bitvector type annotations where possible, rather than using unbounded integers.

## Quick Start Guide - Running the Translator

These first steps give us a current version of the ACL2 x86 model.  The certification steps are necessary because the translator must be able to call a running instance of ACL2 with the X86ISA project loaded in order to perform macro expansion.

1. Download and install ACL2 and certify the community books as detailed here: <http://www.cs.utexas.edu/users/moore/acl2/v8-5/HTML/installation/installation.html>.  SBCL is the recommended underlying Common Lisp - CCL seems to cause problems.
2. Certify the `x86ISA` book as explained here: <https://www.cs.utexas.edu/users/moore/acl2/v8-5/combined-manual/index.html?topic=X86ISA____X86ISA-BUILD-INSTRUCTIONS>.  I ran `cert.pl top` from `<x86-project-folder>/tools/execution/` where `<x86-project-folder` is probably `<acl2-install-folder>/books/projects/x86isa/`.   `cert.pl` should be in `<acl2-install-folder>/books/build/`.
3. Go to the `translator` directory, and copy the configuration file template `config.toml.in` to `config.toml`.  Set the configuration varialbe `acl2_process` to the path to the ACL2 executable (probably called `saved_acl2`).
4. Run `python3 callACL2.py` to start a running instance of ACL2 with which the translator can interact.  You can change its port number by setting `acl2_port` in `config.toml`.  Wait until it prints `Ready` before proceeding.  Leave this running and execute the next steps in a separate terminal.

These steps run the translator.

1. Set other options in `translator/config.toml`.  Most should be fine as default except:
   1. `x86_project_folder` location of the x86 ISA project folder (probably `<acl2-install-folder>/books/projects/x86isa/`).
   2. `outputFolder` - the Sail output files will be saved here.  Make sure that this directory exists before running the translation.
   3. `unresolvedTypesFile` - location of a log file for the type resolution algorithm.
2. Run `python3 ./transform.py` to perform the translation.
3. The translation could take a few minutes - once it has completed the program will exit.

## Code Structure and File Guide

The following diagram shows the general structure of the translator.  Each file is also briefly described below.

![Diagrams](doc/Diagrams.svg)

**General**

* `transform.py` is the top level file.  It defines the environment which is threaded throughout the translator and dispatches control of translation to individual 'translation functions' (see General Comments below).
* `lex_parse.py` contains functions for lexing and parsing Lisp code and for pretty printing Lisp ASTs.

**Translation Functions**

* `specialTokens.py` contains translation functions for 'special tokens' (i.e. built-in Lisp forms or forms which are convenient to translate manually).
* `handwritten_tokens.py` contains the names and types of the handwritten functions contained in `handwritten.sail`.
* `manualInterventions.py` contains the code for Lisp forms which do not conform to the assumptions made in the translation functions and where a special case is required.

**Configuration Files**

* `config.toml.in` is a configuration file template that should be copied to `config.toml` to enable it.  It contains file paths, most notably for locating the ACL2 model and setting output folder.  There are some global options in here as well.
* `config_patterns.py` defines the slice of the model to translate by either specifying Lisp forms to exclude from files otherwise translated, or by specifying Lisp forms to translate from files otherwise excluded.
* `config_function_maps.py` contains mappings of Lisp tokens to Python translation functions (drawn from the translation function files above).  These tables are used each time a token is encountered during translation.

**Misc.**

* `SailASTelems.py` and `SailTypes.py` contain Python representations of Sail syntactic elements and Sail types, as well as pretty printing methods.  Some computation is performed here: the class `SailApp` contains some type resolution code; and other AST classes contain special case code (see `doc/SpecialCases.md` for details).
* `callACL2.py` and `socketFuncs.py` contain code for starting an ACL2 instance which can be queried for macro expansions etc.
* `utils.py` contains miscellaneous utilities, most notably a function for transforming Lisp names (which are very permissive) into Sail names.

## Licence

The files in this repository are distributed under the BSD 2-clause licence in `LICENCE`, except for the model snapshot in `model`, which is derived from the original ACL2 model, distributed under the BSD 3-clause licence.  A copy of the latter can be found in `model/LICENCE`.
