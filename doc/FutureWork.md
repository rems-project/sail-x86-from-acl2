# Future Work

*Patrick Taylor, pjht2, 25.06.20*

Although the translator provides *a* type-checkable and executable translation (which, under testing, seems to be correct), there are many areas for improvement.

There is a certain trade off between the idiomaticity of the Sail output and the level of translation automation.  Nonetheless, there are areas in which both may be improved without detriment to the other.  The suggestions are organise under headings along these lines.

[ToC]

## Idiomaticity of Output

### Inferring bitvectors Instead of integers

The ACL2 model represents many values with integers and implements bitvector operations on them using integer operations.  Currently the translation follows this precedent, translating many of the operations as handwritten functions.  The most pressing improvement is to translate these values as Sail bitvectors, which would require inferring their width.

* In ACL2 the integers are checked to be within the correct range for their particular context with inline `the` expressions (`(the <type-spec> <val>)` statically proves `<val>` conforms to `<type-spec>`).  These are translated as dynamic type checks in Sail.  Translating as bitvectors would allow types to be checked statically - a far better solution.
* One relatively easy part of this will be to translate bitstructs to use Sail `structs` and bit vectors rather than ints.
* Using subfields of bitvectors to access parts of overlapping registers would be good but would likely be a large change.

### Use Sail's Dependent Types

We often find snippets of the following form:

```lisp
(make-event (sal/shl-spec-gen  8))
(make-event (sal/shl-spec-gen 16))
(make-event (sal/shl-spec-gen 32))
(make-event (sal/shl-spec-gen 64))
```

In ACL2 this creates four functions, one for each of the four different data widths.  It would be better to leverage Sail's dependent types to create a single function.

* Unfortunately this is not as simple as evaluating `(sal/shl-spec-gen size)`  using the running ACL2 instance because the input must be either 8, 16, 32, 64.

### Merging `let` Bindings

The model makes extensive use of `b*` bindings which allow lists of consecutive binding interspeced with control flow (allowing control to escape to another function when a condition is met).  The consecutive bindings currently translate as a series of nested `let` bindings in Sail.  It would be better, where possible, to collapse these consecutive bindings into a single `let` block delimited with `{ ... }`.

* One issue it name-shadowing.  Sail does not allow a name to be re-defined in a `let` block but names are re-defined in in this style in ACL2.
* Another is the control flow parts (`when`, `if` and `unless` binders).  These are perfectly translatable but the code isn't particularly readable.

### Reduce Type Annotations

The translation makes liberal use of type annotations to force all numeric types to be integers.  To some extent, this because we don't represent numerics as bitvectors (as suggested above), but there are other issues where the ACL2 style interacts poorly with Sail.  It would be good to reduce the reliance on annotations.  Some specific examples:

* Consider the following example

  ```
  val id : int -> int
  function id x = x
  
  val matchProblem : int -> int
  function matchProblem i =
  	id(
  		match i {
  			0 => 5,
  			_ => 6
  		}
  	)
  ```

  This produces the error message:

  ```
  Cannot infer type of: match i { 0 -> 5, _ -> 6 }
  ```

  Which is solved by wrapping the `match` expression in an `( ... ) : int` annotation.

* Consider the following example:

  ```
  val letProblem : unit -> int
  function letProblem () =
  	let v1 =
  		(let v2 = id(5)
  			in
  				v2)
  	in
  		v1
  ```

  This produces the error message:

  ```
  The type variable '_v2 would leak into an outer scope.
  ```

  because the lifetime of `v1`'s type variable is greater than `v2`'s.  The current solution is to make `v1` not depend on `v2`' type by adding an annotation round the inner excpression.  A more idiomatic re-write would be:

  ```
  {
  	let y = ...;
  	let x = f(y);
  	...
  }
  ```

* Consider the following example:

  ```
  val letProblem2 : bool -> int
  function letProblem2 b =
  	let (x,y) =
  		if b
  		then (0,0)
  		else (id(1), id(2))
  	in
  		x
  ```

  This produces the error message:

  ```
  Tried performing type coercion from int to int(0) on id(1)
  Coercion failed because:
  int is not a subtype of int(0)
  ```

  Sail expects `id(0)` to return an `int(0)` rather than just an `int` because of the very specific `then` branch.  The chosen solution was the annotate literal values when they appear in tuples (the problem does not occur when tuples are not involved).  I.e. `then (0:int, 0:int)`.

  * A specific example of this is handled in `SailApp`: we annotate quality tests when one of the arguments involves a call to `vex3_byte1_get_m_mmmm`.

### Make Macro Constants More Readable

Various macros beginning with `#` or `*` are translated as numeric constants.  These effectively for magic numbers in the Sail code and are unreadable.  It would be better to translate their names somehow.

### Better Comments

Only long-form comments are translated.  It would be better to translate all comments (most notably inline comments beginning with `;`).

### Better Exceptions

Exceptions:

* All errors are currently thrown in an `EMsg` exception.  It would be better to implement different types for different errors (`ms` and `fault` errors from the ACL2 model and transation errors).
* The ACL2 model represents errors as lists where the first value is a descriptive string or `nil` and any remaining values represent additional data.  Currently only the descriptive string is translated - this makes debugging issues difficult.  Perhaps one option would be to infer the type of the remaining elements and wrap them in a function which casts them to a string, forming a heterogenous list.
* It would also be good to print file and line number (either of the original ACL2 or the translated Sail) when an excpetion occurs.
* Technically the ACL2 model has the option of continuing execution when an error occurs - we may be able to catch errors in Sail and give the user this option as well.

### Remove Dummy `x86` Variable

* In the ACL2 model many functions take a parameter called `x86` as the last parameter.  `x86` carried around the model state (mostly registers and memory).  This state is global in Sail and we translate the `x86` variable as a dummy integer.  It would be nice to remove it entirely.

## Automaticity

### Registers

Currently register definitions and their getters and setters are written by hand in `handwritten.sail`.  It would be better to automatically generate these using information in `concrete-state.lisp` and/or `abstract-state.lisp`.  It would probably be prudent to do this using some custom code, similar to how `utilities.lisp` is partially translated in `generateUtils.py`.

### Linking Handwritten Functions

Every time a new handwritten function is written, its name and type must also be added, by hand, to `handwritten_tokens.py` and regisered in `config_function_maps.py`.  Again, parsing the handwritten Sail file for this information would be better.

### Automatic Identification of Translatable Functions

Currently the translator fails when it encounters a function or macro it does not recognise (i.e. one which has not been manually implement or has not been encountered as a `define`).  One possibility would be to find the definition of such functions by sending `:pe <function-name>` to the running ACL2 instance.  Although this may increase automation but there are a couple of potential issues:

1. It would greatly expand the scope of translation.  Currently this is under tight control in `config_patterns.py`.
2. It may not be very reliable, especially if unimplemented Lisp functions are encountered (such as `car` and `cdr`).

### `generateUtils.py` Automation

In `utilities.lisp` there is a list of sizes for which to generate the various utility functions.  This list has been copied manually to `generateUtils.py` - finding it automatically would be better.  Care should be taken that this is done robustly though.

## Proportion of Model

### System View

Currently only `app-view` is translated (implemented in the translation function `tr_if` and and the handwritten function `app_view`).  One large part of system view which would require translation is paging.

### Undefined Behaviour

The most notable untranslated instructions involving undefined behaviour are system calls and RDRAND.  Information about undefined behaviour can be found in Goel's thesis in Chapter 7 (particularly Section 7.1).

### Opcodes

Most one-byte and some two-byte opcodes implemented in the ACL2 model ahve been translated.  This leaves most two-byte opcodes, most notably floating point operations (for which the correspondence between ACL2 'rationals' and rationals in Sail will have to be checked).  It seems that no three-byte opcodes are implement by the ACL2 model (see http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____IMPLEMENTED-OPCODES).

### Theorems

Goel and her colleagues but a lot of effort into proving theorems about the ACL2 model.  These are ignored by the translator becasue there is no direct way of representing them in Sail.  It might be useful to translate them as support for some of the theorme provers Sail an target.  This would likely require implementing representations of theorem prover syntax in the translator.

## Code Quality / Misc.

### File and Line Information

Carrying and and line information from the classes defined in `lex_parse.py` would be sensible.  It would, at least, allow us to be more informative in error translation.

### Special Cases

As detailed in `SpecialCases.md` some Sail AST classes implement some special case code which may be better placed in either `manualInterventions.py` or in an AST post-processing step.

### Lexing

The lexing algorithm, although it works, is rather simplistic.  A more refined lexer would be more robust.

### Translation Function Return Type

Translation functions return three items:

1. The translated AST;
2. The updated enviornment;
3. The number of ACL2 tokens which were consumed.

This final item was originally intended for catching translation errors but is now rarely used.  It should be removed.

### Better Calling of ACL2

Interaction with the ACL2 instance is currently done by passing the request Lisp forms into its `stdin` then reading a byte at a time from `stdout` until the string `!>` is found, which indicates the ACL2 prompt.

Although this works, it could do with being more robust - perhaps ACL2 has an offical way to interact with it programatically.

Here is one promising book: http://www.cs.utexas.edu/users/moore/acl2/current/combined-manual/index.html?topic=ACL2____BRIDGE