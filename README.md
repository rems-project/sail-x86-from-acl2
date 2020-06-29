# Readme

*Patrick Taylor, pjht2, 25.06.20*

[ToC]

## Introduction

The program implemented translates a formal model of the x86 ISA written in ACL2 (a subset of Lisp) to Sail, a language designed for modelling ISA semantics.

This file provides a quick start guide for getting the translator before detailing the code structure for future work.

Files and folders:

* `doc/` - contains other documentation (see below).
* `modelSample` - contains an output of a run of the translator.
* `translator` contains the translation Python code and validation scripts.

Other documentation:

* A separate Readme in `translator/validation` describes how to use the translated model.
* `doc/FutureWork.md` - contains a list of obvious improvements which could be made.
* `doc/comparison/` - contains a side by side comparison of an ACL2 call trace and its Sail translation along with expanatory prose.
* `doc/diss/latex/diss.pdf` - contains a copy of the dissertation .  This goes into depth on the project as a whole and the algorithms used to infer unknown types and disambiguate the translation of Lisp tokens such as `nil`.
* Code comments at the start of each Python file and function.
* `doc/specialCases.md` - contains a list of places which might cause confusion.

## Quick Start Guide - Running the Translator

These steps give us a current version of the ACL2 x86 model.  The certification steps are necessary because the translator must be able to call a running instance of ACL2 with the X86ISA project loaded in order to perform macro expansion.

1. Download and install ACL2 and certify the community books as detailed here: http://www.cs.utexas.edu/users/moore/acl2/v8-3/HTML/installation/installation.html.  SBCL is the recommended underlying Common Lisp - CCL seems to cause problems.
2. Certify the `x86ISA` book as explained here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____X86ISA-BUILD-INSTRUCTIONS.  I ran `cert.pl top` from `<x86-project-folder>/tools/execution/`, where `cert.pl` should be in `<alc2-install-folder>/books/build/`.
3. Set the path to the ACL2 executable (probably called `saved_acl2`) by setting the configuration variable `acl2Process` in `translator/config_files.py`.
4. Run `python3 translator/callACL2.py` to start a running instance of ACL2 with which the translator can interact.  Wait until it prints `Ready` before proceeding.  Leave this running and execute the next steps in a separate terminal.

These steps run the translator.

1. Set other options at the top of `translator/config_files`.  Most should be fine as default except:
   1. `x86_project_folder` and `translateFile` - location of the model to translate.
   2. `outputFolder` - the Sail output files will be saved here.
   3. `unresolvedTypesFile` - location of a log file for the type resolution algorithm.
2. Connect to the `translator/` directory.
3. Run `python3 ./transform.py` to perform the translation.
4. The translation could take a few minutes - once it has completed the program will exit.

## Code Structure and File Guide

The following diagram shows the general structure of the translator.  Each file is also briefly described below.

![Diagrams](doc/Diagrams.svg)

**General**

* `transform.py` is the top level file which defines the environment threaded througout the translator and dispatches control of translation to individual 'translation functions' (see General Comments below).
* `lex_parse.py` contains functions for lexing and parsing Lisp code and for pretty printing Lisp ASTs.

**Translation Functions**

* `specialTokens.py` contains translation functions for 'special tokens' (i.e. built-in Lisp forms or forms which are convenient to translate manually).
* `handwritten_tokens.py` contains the names and types of the handwritten functions contained in `handwritten.sail`.
* `generateUtils.py` provides a custom translation for `utilities.lisp`.
* `manualInterventions.py` contains the code for Lisp forms which do not conform to the assumptions made in the translation functions and where a special case is required.

**Configuration Scripts**

* `config_files.py` contains file paths, most notably for locating the ACL2 model and output folder.  There are some global options in here as well.
* `config_patterns.py` defines the slice of the model to translate by either specifying Lisp forms to exclude from files otherwise translated, or by specifying Lisp forms to translate from files otherwise excluded.
* `config_function_maps.py` contains mappings of Lisp tokens to Python translation functions.  These tables are used each time a token is encountered during translation.

**Misc.**

* `SailASTelems.py` and `SailTypes.py` contain Python representations of Sail syntactic elements and Sail types, as well as pretty printing methods.  Some computation is performed here: the class `SailApp` contains some type resolution code; and other AST classes contain special case code.
* `callACL2.py` and `socketFuncs.py` contain code for starting an ACL2 instance which can be queried for macro expansions etc.
* `utils.py` contains miscellaneous utilities, most notably a function for transforming Lisp names (which are very permissive) into Sail names.

## Extending the Translator

Think about where the extension should go:

### Special Token

1. Implement the translation function in `specialTokens.py`.  Ensure it has the correct type as detailed in that file.
2. Register the Lisp token to translation function mapping in `specialTokens()` in `config_function_maps.py`.

### Handwritten Function

1. Write the handwritten Sail function in `handwritten.sail`.
2. Register its name and type in `handwritten_tokens.py`.
3. Register the lisp token to handwritten function mapping in `handwritten()` in `config_function_maps.py`.

### Manual Intervention

1. Implement the manual intervention as a function in `manualInterventions.py`.  Ensure it has the correct type as detailed in that file.
2. Add the function name to `interventionsList` at the bottom of`manualInterventions.py`.

### Custom Translation

For example, `generateUtils.py` is a custom translation of `utilities.lisp`.  A custom translation should still return Python translation functions of the type descried in `specialTokens.py`.  Follow the example of `utils()` in `config_function_maps.py`.

### Post Processing of whole Sail AST

An example of using the `getChildrenByPred()` method implement by all Sail AST classes can be found in `transformACL2FiletoSail()` in `transform.py`.   This example collects instaces of function applications and let bindings where the bound variable(s) have an unknown type.

## General Comments

This section contains useful comments which don't really belong anywhere else.

* A 'translation function' is a Python function which translates a single Lisp token of Lisp list.  For example, consider the Lisp form `(if t (+ 2 5) (- 6 3))` (the general form of a conditional is `(if <cond> <then-branch> <else-branch>)`).  The first token of this list is `if` so the translation function `tr_if` if called and given the whole list and the current environment as arguments.  `tr_if` then recursively asks the top-level translation function to translation its sub-expressions (`t`, `(+ 2 5)`, and `(- 6 3)`).  Once it has these translations it places them into the correct Sail expression for `if`.

  Translation functions return a Python *list* of Sail AST elements because some forms produce more than one translated funtion definition (most notably bitstructs, for which accessors and updaters are generated).

* ACL2 has an `mbe` form which allows different code for logical reasoning vs. execution.  `config_files.py` has a parameter which can be changed to choose between the `:logic` and `:exec` branches.  Throughout development the `:logic` branch was used.

* I've mostly avoided translating homogeneous data lists.  This means that some fairly basic Lisp forms such as `car` and `cdr` (effectively `head` and `tail`) do not have translation functions.  So far this has not been an issue.  One place where it might be useful is the translation of errors.  See `FutureWork.md` for details.

* The Python code is implemented in a broadly functional style which should be fairly amenable to translation to OCaml.  For instance, the environment is threaded through most functions.  Many occurences of `isinstance` could be replaced with pattern matching.

* The ACL2 model makes extensive use of `b*` bindings - it's worth understanding these (see here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____B_A2).

* The pretty printers for each syntactic element are farily basic and, as described in `FutureWork.md` often require many type annotations.  Hopefully sending the translator output on a round trip though Sail's pretty printer might improve matters as a first pass.