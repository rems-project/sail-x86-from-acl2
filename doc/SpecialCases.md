# Special Cases

*Patrick Taylor, pjht2, 26.06.20*

Although much of the code is well structured, there do exist certain special cases and hacks which may at first cause confusion.  This files is intended as an index - more information can be found in the comments in the relevant code files.

Translation functions begin with `tr_`.

**Misc**

* The translations of the files `instructions/segmentation.lisp` and `instructions/fp/top.lisp` are called `segmentationInst.sail` and `topFP.sail` respectively to avoid name collisions.
* Translation of `utilities.lisp` is done manually with `generateUtils.py`.
* Lisp functions `feature-flag` and `feature-flags` are translated manually in `handwritten2.sail `.  They assume all 'features' are available.  See comment there for information.

**In Translation Functions**

* In `tr_if` if the conditional is `app-view` then just translate the 'then' branch because we do not translate system view.
* Translation function `tr_mv` implements recognition and translation of error lists.
* `tr_def_inst` is quite fragile and not robust to change.
* `tr_xr` contains a list of registers whose getters/setters have been implemented in `handwritten2.sail` - this list should be found automatically.
* Inspection of the result of a `:pe` command in `tr_pe` is likely to be fragile.

**In AST Elements (`SailASTelems.py`)**

* In `SailLet.__init__()`, when binding `let <var> = <expr> in <body`: if `<expr>` is either `check-alignment?` or `inst-ac?` and `<expr>` is `nil` we force the `nil` to a boolean `False`.

* In `SailBoundVar`:

  * In `setType()`: force type of variables called `x86` to `int` - this variable represents model state and in Sail is global, so we ignore use a dummy integer in the translation.
  * In `pp()`: if the variable is called `les_lds_distinguishing_byte` or `max_offset` then provide a type annotation.

* In `SailApp.pp()`: if we have a test of the form ``vex3_byte1_get_m_mmmm(...) == ...` then provide a type annotation.

* In `SailIf`:

  * In `__init_()`: convert a string to a maybe string in error messages
  * In `getType()`: 

* In `SailTuple.pp()`: provide a type annotation for number literals.

* In `SailMatch.pp()`: some constant folding to remove instances like the following (the product of macro expansions):

  ```
  match ":CF" {
  ":CF" => set_rflagsbits_get_cf(sailVal, rflags_var),
  ":PF" => set_rflagsbits_get_pf(sailVal, rflags_var),
  ":AF" => set_rflagsbits_get_af(sailVal, rflags_var),
  ":ZF" => set_rflagsbits_get_zf(sailVal, rflags_var),
  ":SF" => set_rflagsbits_get_sf(sailVal, rflags_var),
  ":TF" => set_rflagsbits_get_tf(sailVal, rflags_var),
  ":IF" => set_rflagsbits_get_intf(sailVal, rflags_var),
  ":DF" => set_rflagsbits_get_df(sailVal, rflags_var),
  ":OF" => set_rflagsbits_get_of(sailVal, rflags_var),
  ":IOPL" => set_rflagsbits_get_iopl(sailVal, rflags_var),
  ":NT" => set_rflagsbits_get_nt(sailVal, rflags_var),
  ":RF" => set_rflagsbits_get_rf(sailVal, rflags_var),
  ":VM" => set_rflagsbits_get_vm(sailVal, rflags_var),
  ":AC" => set_rflagsbits_get_ac(sailVal, rflags_var),
  ":VIF" => set_rflagsbits_get_vif(sailVal, rflags_var),
  ":VIP" => set_rflagsbits_get_vip(sailVal, rflags_var),
  ":ID" => set_rflagsbits_get_id(sailVal, rflags_var),
  _ => throw(Emsg("not hello"))
  }
  ```
  
* In `SailVectorProject.pp()`: there is some inline Sail.

* In `Sail_t_member`: only supports ints and strings.

