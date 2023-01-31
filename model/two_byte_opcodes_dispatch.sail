$include "./three_byte_opcodes_dispatch.sail"

/*! Two-byte opcode dispatch function.
<p>@ ( ' two-byte-opcode-execute ' ) is the doorway to the two-byte 
 opcode map, and will lead to the three-byte opcode map if @ ( ' opcode ' ) is 
 either @ ( ' #x38 ' ) or @ ( ' #x3A ' ) .</p> */
val two_byte_opcode_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), bits(8), modr_m, sib) -> unit

function two_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib) = {
    if ext_two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib) then {
        return()
    } else {
        (match opcode {
          0 => {
              if modr_m[reg] == 0b000 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b010 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_lldt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[reg] == 0b011 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b101 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          1 => {
              if modr_m[reg] == 0b000 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b010 then {
                  let fault_var : option(string) = {
                      if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b011 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b100 then {
                  let fault_var : option(string) = {
                      if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MONITOR"|]) == 0 | not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MONITOR"|]) == 0 | not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b010 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":SMAP"|]) == 0 | 0xf0 == prefixes[lck] | not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b011 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":SMAP"|]) == 0 | 0xf0 == prefixes[lck] | not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b111 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b010 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_lgdt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[reg] == 0b011 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_lidt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":XSAVE"|]) == 0 | 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[osxsave] == 0b0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":XSAVE"|]) == 0 | 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[osxsave] == 0b0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b100 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b101 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] | 0x66 == prefixes[opr] | 0xf3 == prefixes[rep] | 0xf2 == prefixes[rep] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b110 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":HLE", ":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b111 then {
                  let fault_var : option(string) = {
                      if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 then {
                          Some(":NM")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b111 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if in_64bit_mode(proc_mode) & modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":RDTSCP"|]) == 0 | 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          2 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          3 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          5 => {
              if in_64bit_mode(proc_mode) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] | Mk_ia32_eferbits(truncate(read_msr(0), 12))[sce] == 0b0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_syscall_both_views(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          6 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          7 => {
              if in_64bit_mode(proc_mode) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] | Mk_ia32_eferbits(truncate(read_msr(0), 12))[sce] == 0b0 then {
                          Some(":UD")
                      } else if not_bool(unsigned({
                          ();
                          Mk_segment_selectorbits(seg_visibles[1])[rpl]
                      }) == 0) then {
                          Some(":GP")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_sysret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          8 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          9 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          11 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"UD2 encountered!\" START-RIP TEMP-RIP X86)")),
          13 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          16 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movss_movsd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movss_movsd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          17 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movss_movsd_op_en_mr(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movss_movsd_op_en_mr(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          18 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          19 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          20 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_unpck?ps_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_unpck?pd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          21 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_unpck?ps_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_unpck?pd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          22 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          23 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          24 => {
              if modr_m[reg] == 0b000 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b010 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & not_bool(modr_m[mod] == 0b11) then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b101 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b110 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[reg] == 0b111 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if modr_m[mod] == 0b11 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          25 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          26 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                          modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                      } else if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          27 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                          modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                      } else if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                          modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                      } else if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                      } else {
                          (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                            Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                      }) then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          28 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          29 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          30 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          31 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_nop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          32 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | (let reg : bits(3) = modr_m[reg] in
                    if in_64bit_mode(proc_mode) & logbitp(2, rex_byte) then {
                        not_bool(reg == 0b000)
                    } else {
                        reg == 0b001 | reg == 0b101 | reg == 0b110 | reg == 0b111
                    }) then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_mov_control_regs_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          33 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[de] == 0b1 & (modr_m[reg] == 0b100 | modr_m[reg] == 0b101) then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          34 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | (let reg : bits(3) = modr_m[reg] in
                    if in_64bit_mode(proc_mode) & logbitp(2, rex_byte) then {
                        not_bool(reg == 0b000)
                    } else {
                        reg == 0b001 | reg == 0b101 | reg == 0b110 | reg == 0b111
                    }) then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          35 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[de] == 0b1 & (modr_m[reg] == 0b100 | modr_m[reg] == 0b101) then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          40 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          41 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          43 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          48 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          49 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          50 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          51 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[pce] == 0b0 then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          52 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else if Mk_cr0bits(loghead(32, ctrs[0]))[pe] == 0b0 then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          53 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) | Mk_cr0bits(loghead(32, ctrs[0]))[pe] == 0b0 then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          55 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          64 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          65 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          66 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          67 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          68 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          69 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          70 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          71 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          72 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          73 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          74 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          75 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          76 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          77 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          78 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          79 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          80 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          82 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          83 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          91 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          96 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          97 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          98 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          99 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          100 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          101 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          102 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          103 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          104 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          105 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          106 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          107 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          108 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          109 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          110 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movd_movq_to_xmm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          111 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          112 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          113 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          114 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          115 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          117 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          118 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          119 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          120 => {
              let fault_var : option(string) = {
                  if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          121 => {
              let fault_var : option(string) = {
                  if not_bool(unsigned({
                      ();
                      Mk_segment_selectorbits(seg_visibles[1])[rpl]
                  }) == 0) then {
                      Some(":GP")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          124 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          125 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          126 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movd_movq_from_xmm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          127 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          128 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          129 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          130 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          131 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          132 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          133 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          134 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          135 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          136 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          137 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          138 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          139 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          140 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          141 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          142 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          143 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          144 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          145 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          146 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          147 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          148 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          149 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          150 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          151 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          152 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          153 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          154 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          155 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          156 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          157 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          158 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          159 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          160 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          161 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          162 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          163 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_bt_0f_a3(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          164 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          165 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          168 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          169 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          170 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
          171 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          172 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          173 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          175 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_imul_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          176 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          177 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          178 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          179 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          180 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          181 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          182 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          183 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          184 => {
              if mandatory_prefix == 0x00 then {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":POPCNT"|]) == 0 | 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          185 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"UD1 encountered!\" START-RIP TEMP-RIP X86)")),
          186 => {
              if modr_m[reg] == 0b100 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_bt_0f_ba(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if modr_m[reg] == 0b101 then {
                  let fault_var : option(string) = {
                      if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b110 then {
                  let fault_var : option(string) = {
                      if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[reg] == 0b111 then {
                  let fault_var : option(string) = {
                      if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          187 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          189 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          190 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          191 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          192 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          193 => {
              let fault_var : option(string) = {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          195 => {
              let fault_var : option(string) = {
                  if feature_flags_fn([|":SSE2"|]) == 0 | 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          196 => {
              if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          197 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          198 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_shufps_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      x86_shufpd_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          200 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          201 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          202 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          203 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          204 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          205 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          206 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          207 => {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          },
          208 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          209 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          210 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          211 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          212 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          213 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          214 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":SSE2"|]) == 0 | Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 | Mk_cr4bits(loghead(22, ctrs[4]))[osfxsr] == 0b0 | 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      if feature_flags_fn([|":SSE2"|]) == 0 | Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 | Mk_cr4bits(loghead(22, ctrs[4]))[osfxsr] == 0b0 | 0xf0 == prefixes[lck] then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          216 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          217 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          218 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          220 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          221 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          222 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          224 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          225 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          226 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          227 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          228 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          229 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          230 => {
              if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf3 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          231 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          232 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          233 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          234 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          236 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          237 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          238 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          240 => {
              if mandatory_prefix == 0xf2 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE3"|]) == 0 | modr_m[mod] == 0b11 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          241 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          242 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          243 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          244 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          245 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          246 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          247 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          248 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          249 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          250 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          251 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          252 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          253 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          254 => {
              if mandatory_prefix == 0x00 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":MMX"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else if mandatory_prefix == 0x66 then {
                  let fault_var : option(string) = {
                      let chk_ex : option(string) = chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                      if is_some(chk_ex) then {
                          chk_ex
                      } else if feature_flags_fn([|":SSE2"|]) == 0 then {
                          Some(":UD")
                      } else {
                          None() : option(string)
                      }
                  };
                  if is_some(fault_var) then {
                      (match fault_var {
                        Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-GENERAL-PROTECTION \"#GP Encountered!\" START-RIP TEMP-RIP X86)")),
                        Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                        _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                      }) : unit
                  } else {
                      throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
                  }
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
              }
          },
          42 => throw(Emsg("Translation error: two-byte opcode 42 not translated")),
          44 => throw(Emsg("Translation error: two-byte opcode 44 not translated")),
          45 => throw(Emsg("Translation error: two-byte opcode 45 not translated")),
          46 => throw(Emsg("Translation error: two-byte opcode 46 not translated")),
          47 => throw(Emsg("Translation error: two-byte opcode 47 not translated")),
          56 => throw(Emsg("Translation error: two-byte opcode 56 not translated")),
          58 => throw(Emsg("Translation error: two-byte opcode 58 not translated")),
          81 => throw(Emsg("Translation error: two-byte opcode 81 not translated")),
          84 => throw(Emsg("Translation error: two-byte opcode 84 not translated")),
          85 => throw(Emsg("Translation error: two-byte opcode 85 not translated")),
          86 => throw(Emsg("Translation error: two-byte opcode 86 not translated")),
          87 => throw(Emsg("Translation error: two-byte opcode 87 not translated")),
          88 => throw(Emsg("Translation error: two-byte opcode 88 not translated")),
          89 => throw(Emsg("Translation error: two-byte opcode 89 not translated")),
          90 => throw(Emsg("Translation error: two-byte opcode 90 not translated")),
          92 => throw(Emsg("Translation error: two-byte opcode 92 not translated")),
          93 => throw(Emsg("Translation error: two-byte opcode 93 not translated")),
          94 => throw(Emsg("Translation error: two-byte opcode 94 not translated")),
          95 => throw(Emsg("Translation error: two-byte opcode 95 not translated")),
          116 => throw(Emsg("Translation error: two-byte opcode 116 not translated")),
          174 => throw(Emsg("Translation error: two-byte opcode 174 not translated")),
          188 => throw(Emsg("Translation error: two-byte opcode 188 not translated")),
          194 => throw(Emsg("Translation error: two-byte opcode 194 not translated")),
          199 => throw(Emsg("Translation error: two-byte opcode 199 not translated")),
          215 => throw(Emsg("Translation error: two-byte opcode 215 not translated")),
          219 => throw(Emsg("Translation error: two-byte opcode 219 not translated")),
          223 => throw(Emsg("Translation error: two-byte opcode 223 not translated")),
          235 => throw(Emsg("Translation error: two-byte opcode 235 not translated")),
          239 => throw(Emsg("Translation error: two-byte opcode 239 not translated")),
          _ => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
        }) : unit
    }
}

/*! Decoder and dispatch function for two-byte opcodes
<p>Source: Intel Manual, Volume 2, Appendix A-2</p> */
val two_byte_opcode_decode_and_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8)) -> unit

function two_byte_opcode_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, escape_byte) = {
    let ctx : string = "two-byte-opcode-decode-and-execute";
    let (flg0, opcode) : (option(string), bits(8)) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X");
    if is_some(flg0) then throw(Emsg("Model state error: :OPCODE-BYTE-ACCESS-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
    let mandatory_prefix : bits(8) = compute_mandatory_prefix_for_two_byte_opcode(proc_mode, opcode, prefixes);
    let modr_m? : bool = two_byte_opcode_modr_m_p(proc_mode, mandatory_prefix, opcode);
    let (flg1, modr_m) : (option(string), bits(8)) = {
        if modr_m? then {
            rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
        } else {
            (None() : option(string), 0x00)
        }
    };
    if is_some(flg1) then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = {
        if modr_m? then {
            add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
        } else {
            (None() : option(string), temp_rip)
        }
    };
    if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
    let sib? : bool = modr_m? & (let p4? : bool = 0x67 == prefixes[adr] in
    let n16_bit_addressp : bool = 2 == select_address_size(proc_mode, Some(prefixes)) in
      x86_decode_sib_p(Mk_modr_m(modr_m), n16_bit_addressp));
    let (flg2, sib) : (option(string), bits(8)) = {
        if sib? then {
            rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
        } else {
            (None() : option(string), 0x00)
        }
    };
    if is_some(flg2) then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"));
    let (flg, temp_rip) : (option(string), sbits(49)) = {
        if sib? then {
            add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
        } else {
            (None() : option(string), temp_rip)
        }
    };
    if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
    two_byte_opcode_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, mandatory_prefix, rex_byte, opcode, Mk_modr_m(modr_m), Mk_sib(sib))
}
