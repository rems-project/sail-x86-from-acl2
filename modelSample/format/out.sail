
default Order dec
$include <prelude.sail>
$include "./handwritten.sail"
$include "./top.sail"
$include "./two_byte_opcodes_dispatch.sail"

val get_prefixes : (range(0, 4), sbits(48), bits(52), bits(8), range(0, 15)) -> (option(string), int, int)

function get_prefixes (proc_mode, start_rip, prefixes, rex_byte, cnt) = {
    if cnt == 0 then {
        (Some("This error was 't' in ACL2 but a Some(string) in Sail"), unsigned(prefixes), unsigned(rex_byte))
    } else {
        let ctx : string = "get-prefixes";
        let (flg, byte) : (option(string), int) = rme08(proc_mode, sail_sign_extend(start_rip, 64), 1, ":X");
        let byte : bits(8) = bits_of_int(byte, 8);
        if is_some(flg) then {
            (Some(ctx), unsigned(byte), unsigned(rex_byte))
        } else {
            let prefix_byte_group_code : {|0, 1, 2, 3, 4|} = get_one_byte_prefix_array_code(byte);
            (match prefix_byte_group_code {
              0 => {
                  let rex? : bool = proc_mode == 0 & bits_of_int(ash(unsigned(byte), -4), 4) == 0x4;
                  if rex? then {
                      let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                      if is_some(flg) then {
                          (flg, unsigned(prefixes), unsigned(rex_byte))
                      } else {
                          get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, byte, check_range(0, 15, cnt - 1))
                      }
                  } else {
                      let (elem0, elem1, elem2) : (option(string), bits(52), bits(8)) = {
                          let prefixes : bits(52) = set_prefixes_get_nxt(byte, prefixes);
                          (None() : option(string), set_prefixes_get_num(bits_of_int(15 - cnt, 4), prefixes), rex_byte)
                      };
                      (elem0, unsigned(elem1), unsigned(elem2))
                  }
              },
              1 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, unsigned(prefixes), unsigned(rex_byte))
                  } else {
                      let prefixes : bits(52) = {
                          if byte == 0xf0 then {
                              set_prefixes_get_lck(byte, prefixes)
                          } else {
                              set_prefixes_get_rep(byte, prefixes)
                          }
                      };
                      get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              2 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, unsigned(prefixes), unsigned(rex_byte))
                  } else if proc_mode == 0 & (byte == 0x64 | byte == 0x65) | not_bool(proc_mode == 0) then {
                      get_prefixes(proc_mode, truncate(next_rip, 48), set_prefixes_get_seg(byte, prefixes), 0x00, check_range(0, 15, cnt - 1))
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              3 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, unsigned(prefixes), unsigned(rex_byte))
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), set_prefixes_get_opr(byte, prefixes), 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              4 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, unsigned(prefixes), unsigned(rex_byte))
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), set_prefixes_get_adr(byte, prefixes), 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              _ => (Some("This error was 't' in ACL2 but a Some(string) in Sail"), unsigned(prefixes), unsigned(rex_byte))
            }) : (option(string), int, int)
        }
    }
}

val one_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), bits(52), bits(8), bits(8), bits(8), bits(8)) -> unit

function one_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    (match opcode {
      0 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      1 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      2 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      3 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      4 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      5 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      6 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: PUSH ES is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      7 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: POP ES is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      8 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      9 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      10 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      11 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      12 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      13 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      14 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: PUSH CS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      15 => two_byte_opcode_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode),
      16 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      17 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      18 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      19 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      20 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      21 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      22 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: PUSH SS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      23 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: POP SS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      24 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      25 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      26 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      27 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      28 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      29 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      30 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: PUSH DS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      31 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: POP DS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      32 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      33 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      34 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      35 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      36 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      37 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      38 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      39 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: DAA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      40 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      41 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      42 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      43 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      44 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      45 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      46 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      47 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: DAS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      48 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      49 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      50 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      51 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      52 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      53 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      54 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      55 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: AAA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      56 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      57 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      58 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      59 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      60 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      61 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      62 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      63 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: AAS is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      64 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      65 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      66 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      67 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      68 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      69 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      70 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      71 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      72 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      73 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      74 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      75 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      76 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      77 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      78 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      79 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      80 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      81 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      82 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      83 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      84 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      85 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      86 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      87 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      88 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      89 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      90 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      91 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      92 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      93 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      94 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      95 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      96 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_pusha(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: PUSHA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      97 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_popa(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: POPA is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      98 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) | modr_m_get_mod(modr_m) == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      99 => {
          if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_movsx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      100 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      101 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      102 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      103 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      104 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      105 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      106 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      107 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      108 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      109 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      110 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      111 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      112 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      113 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      114 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      115 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      116 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      117 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      118 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      119 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      120 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      121 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      122 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      123 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      124 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      125 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      126 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      127 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      128 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      129 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      130 => {
          if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: Opcode 0x82 is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      131 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      132 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      133 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      134 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      135 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      136 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      137 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      138 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      139 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      140 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      141 => {
          let fault_var : option(string) = {
              if modr_m_get_mod(modr_m) == 0b11 | 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_lea(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      142 => {
          let fault_var : option(string) = {
              if modr_m_get_reg(modr_m) == 0b001 | 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      143 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_pop_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      144 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      145 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      146 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      147 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      148 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      149 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      150 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      151 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      152 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cbw_cwd_cdqe(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      153 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cwd_cdq_cqo(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      154 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: far CALL is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      155 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      156 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_pushf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      157 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_popf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      158 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) | proc_mode == 0 & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_sahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      159 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) | proc_mode == 0 & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_lahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      160 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      161 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      162 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      163 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      164 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      165 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      166 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      167 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      168 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      169 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      170 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      171 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      172 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      173 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      174 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      175 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      176 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      177 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      178 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      179 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      180 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      181 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      182 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      183 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      184 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      185 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      186 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      187 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      188 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      189 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      190 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      191 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      192 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      193 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      194 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      195 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      196 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) | modr_m_get_mod(modr_m) == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      197 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          } else if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) | modr_m_get_mod(modr_m) == 0b11 then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      198 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 & modr_m_get_mod(modr_m) == 0b11 & modr_m_get_r_m(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      199 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 & modr_m_get_mod(modr_m) == 0b11 & modr_m_get_r_m(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      200 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      201 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_leave(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      202 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
      203 => throw(Emsg("Model state error: Opcode Unimplemented in x86isa!")),
      204 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      205 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      206 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: INTO is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      207 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      208 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      209 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      210 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      211 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      212 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: AAM is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      213 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: AAD is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      215 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      216 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      217 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      218 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      219 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      220 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      221 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      222 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      223 => {
          let fault_var : option(string) = {
              if cr0bits_get_ts(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 | cr0bits_get_em(bits_of_int(loghead(32, unsigned(ctrs[0])), 32)) == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-DEVICE-NOT-AVAILABLE', STRING: #NM Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      224 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      225 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      226 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      227 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_jrcxz(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      228 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      229 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      230 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      231 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      232 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_call_e8_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      233 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      234 => {
          if not_bool(proc_mode == 0) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if proc_mode == 0 then {
              let fault_var : option(string) = {
                  if true then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: JMP is illegal in the 64-bit mode!, 'START-RIP', 'TEMP-RIP', 'X86']"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      235 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      236 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      237 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      238 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      239 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      240 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      241 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      242 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      243 => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
      244 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else if not_bool(unsigned(let x86 : unit = () in segment_selectorbits_get_rpl(seg_visibles[1])) == 0) then {
                  Some(":GP")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                Some(":GP") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-GENERAL-PROTECTION', STRING: #GP Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_hlt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      245 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      246 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      247 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b111 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      248 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      249 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      250 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      251 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
          }
      },
      252 => x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib),
      253 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes_get_lck(prefixes) then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      254 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      255 => {
          if modr_m_get_reg(modr_m) == 0b000 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b001 then {
              let fault_var : option(string) = {
                  if modr_m_get_mod(modr_m) == 0b11 & 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b010 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_call_ff_2_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b011 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  throw(Emsg("Model state error: Opcode Unimplemented in x86isa!"))
              }
          } else if modr_m_get_reg(modr_m) == 0b100 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_near_jmp_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b101 & not_bool(modr_m_get_mod(modr_m) == 0b11) then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_far_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m_get_reg(modr_m) == 0b110 then {
              let fault_var : option(string) = {
                  if 0xf0 == prefixes_get_lck(prefixes) then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']")),
                    _ => throw(Emsg("Model state error: Unimplemented exception in x86isa!"))
                  }) : unit
              } else {
                  x86_push_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST:\n['X86-ILLEGAL-INSTRUCTION', STRING: #UD Encountered!, 'START-RIP', 'TEMP-RIP', 'X86']"))
          }
      },
      214 => throw(Emsg("Translation error: one-byte opcode 214 not translated")),
      _ => throw(Emsg("Translation error: invalid one-byte opcode"))
    }) : unit
}

val x86_fetch_decode_execute : unit -> unit

function x86_fetch_decode_execute () = {
    let ctx : string = "x86-fetch-decode-execute";
    if ms() | fault() then () else {
        let proc_mode : {|0, 1|} = x86_operation_mode();
        let n64_bit_modep_var : bool = proc_mode == 0;
        let start_rip : sbits(48) = read_iptr(proc_mode);
        let (flg, prefixes, rex_byte) : (option(string), int, int) = get_prefixes(proc_mode, start_rip, 0x0000000000000, 0x00, 15);
        let prefixes : bits(52) = bits_of_int(prefixes, 52);
        let rex_byte : bits(8) = bits_of_int(rex_byte, 8);
        if is_some(flg) then throw(Emsg("Model state error: :ERROR-IN-READING-PREFIXES"));
        let opcode_vex_evex_byte : bits(8) = prefixes_get_nxt(prefixes);
        let prefix_length : bits(4) = prefixes_get_num(prefixes);
        let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, bits_of_int(1 + unsigned(prefix_length), 48));
        if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
        let vex_byte0? : bool = opcode_vex_evex_byte == 0xc5 | opcode_vex_evex_byte == 0xc4;
        let (flg, les_lds_distinguishing_byte : int) : (option(string), int) = {
            if vex_byte0? then {
                rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
            } else {
                (None() : option(string), 0)
            }
        };
        if is_some(flg) then throw(Emsg("Model state error: :LES/LDS-DISTINGUISHING-BYTE-READ-ERROR"));
        if vex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & get_slice_int(2, les_lds_distinguishing_byte : int, 6) == 0b11) then {
            let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001);
            if is_some(flg) then throw(Emsg("Model state error: :VEX-BYTE1-INCREMENT-ERROR"));
            let vex_prefixes : bits(24) = set_vex_prefixes_get_byte0(opcode_vex_evex_byte, 0x000000);
            let vex_prefixes : bits(24) = set_vex_prefixes_get_byte1(bits_of_int(les_lds_distinguishing_byte : int, 8), vex_prefixes);
            unimplemented_vex_decode_and_execute(proc_mode, signed(start_rip), signed(temp_rip), unsigned(prefixes), unsigned(rex_byte), unsigned(vex_prefixes))
        } else {
            let opcode_evex_byte : bits(8) = opcode_vex_evex_byte;
            let evex_byte0? : bool = opcode_evex_byte == 0x62;
            let (flg, bound_distinguishing_byte) : (option(string), int) = {
                if evex_byte0? then {
                    rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
                } else {
                    (None() : option(string), 0)
                }
            };
            if is_some(flg) then throw(Emsg("Model state error: :BOUND-DISTINGUISHING-BYTE-READ-ERROR"));
            if evex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & get_slice_int(2, bound_distinguishing_byte, 6) == 0b11) then {
                let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001);
                if is_some(flg) then throw(Emsg("Model state error: :EVEX-BYTE1-INCREMENT-ERROR"));
                let evex_prefixes : bits(32) = set_evex_prefixes_get_byte0(opcode_evex_byte, 0x00000000);
                let evex_prefixes : bits(32) = set_evex_prefixes_get_byte1(bits_of_int(bound_distinguishing_byte, 8), evex_prefixes);
                unimplemented_evex_decode_and_execute(proc_mode, signed(start_rip), signed(temp_rip), unsigned(prefixes), unsigned(rex_byte), unsigned(evex_prefixes))
            } else {
                let opcode_byte : bits(8) = opcode_evex_byte;
                let modr_m? : bool = one_byte_opcode_modr_m_p(proc_mode, opcode_byte);
                let (flg, modr_m) : (option(string), int) = {
                    if modr_m? then {
                        if vex_byte0? | evex_byte0? then {
                            (None() : option(string), les_lds_distinguishing_byte : int)
                        } else {
                            rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
                        }
                    } else {
                        (None() : option(string), 0)
                    }
                };
                let modr_m : bits(8) = bits_of_int(modr_m, 8);
                if is_some(flg) then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"));
                let (flg, temp_rip) : (option(string), sbits(49)) = {
                    if modr_m? then {
                        add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
                    } else {
                        (None() : option(string), temp_rip)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
                let sib? : bool = modr_m? & (let p4? : bool = 0x67 == prefixes_get_adr(prefixes) in
                let n16_bit_addressp : bool = 2 == select_address_size(proc_mode, p4?) in
                  x86_decode_sib_p(modr_m, n16_bit_addressp));
                let (flg, sib) : (option(string), int) = {
                    if sib? then {
                        rme08(proc_mode, sail_sign_extend(temp_rip, 64), 1, ":X")
                    } else {
                        (None() : option(string), 0)
                    }
                };
                let sib : bits(8) = bits_of_int(sib, 8);
                if is_some(flg) then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"));
                let (flg, temp_rip) : (option(string), sbits(49)) = {
                    if sib? then {
                        add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
                    } else {
                        (None() : option(string), temp_rip)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
                one_byte_opcode_execute(proc_mode, start_rip, truncate(temp_rip, 48), prefixes, rex_byte, opcode_byte, modr_m, sib)
            }
        }
    }
}
