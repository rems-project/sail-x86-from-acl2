# Implementation Notes

This file contains notes written by Patrick Taylor on implementation details of the original prototype of the translator.

## Extending the Translator

Thinking about where the extension should go:

### Special Token

1. Implement the translation function in `specialTokens.py`.  Ensure it has the correct type as detailed in that file.
2. Register the mapping of Lisp token to translation function in `specialTokens()` in `config_function_maps.py`.

### Handwritten Function

1. Write the handwritten Sail function in `handwritten.sail`.
2. Register its name and type in `handwritten_tokens.py`.
3. Register the mapping of Lisp token to handwritten function in `handwritten()` in `config_function_maps.py`.

### Manual Intervention

1. Implement the manual intervention as a function in `manualInterventions.py`.  Ensure it has the correct type as detailed in that file.
2. Add the function name to `interventionsList` at the bottom of`manualInterventions.py`.

### Custom Translation

For example, `generateUtils.py` is a custom translation of `utilities.lisp`.  A custom translation should still return Python translation functions of the type descried in `specialTokens.py`.  Follow the example of `utils()` in `config_function_maps.py`.

### Post Processing of Whole Sail AST

An example of using the `getChildrenByPred()` method implement by all Sail AST classes can be found in `transformACL2FiletoSail()` in `transform.py`.   This example collects instances of function applications and let bindings where the bound variable(s) have an unknown type.

## General Comments

This section contains useful comments which don't really belong anywhere else.

* A 'translation function' is a Python function which translates a single Lisp token or Lisp list.  For example, consider the Lisp form `(if t (+ 2 5) (- 6 3))` (the general form of a conditional is `(if <cond> <then-branch> <else-branch>)`).  The first token of this list is `if` so the translation function `tr_if` if called and given the whole list and the current environment as arguments.  `tr_if` then recursively asks the top-level translation function to translation its sub-expressions (`t`, `(+ 2 5)`, and `(- 6 3)`).  Once it has these translations it places them into the correct Sail expression for `if`.

  Translation functions return a Python *list* of Sail AST elements because some forms produce more than one translated function definition (most notably bitstructs, for which accessors and updaters are generated).

* ACL2 has an `mbe` form which allows different code for logical reasoning vs. execution.  `config_files.py` has a parameter which can be changed to choose between the `:logic` and `:exec` branches.  Throughout development the `:logic` branch was used.

* I've mostly avoided translating homogeneous data lists.  This means that some fairly basic Lisp forms such as `car` and `cdr` (effectively `head` and `tail`) do not have translation functions.  So far this has not been an issue.  One place where it might be useful is the translation of errors.  See `FutureWork.md` for details.

* The Python code is implemented in a broadly functional style which should be fairly amenable to translation to OCaml.  For instance, the environment is threaded through most functions.  Many occurrences of `isinstance()` could be replaced with pattern matching.

* The ACL2 model makes extensive use of `b*` bindings - it's worth understanding these (see here: http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/ACL2____B_A2).

* The pretty printers for each syntactic element are fairly basic and, as described in `FutureWork.md` often require many type annotations.  Hopefully sending the translator output on a round trip though Sail's pretty printer might improve matters as a first pass.
