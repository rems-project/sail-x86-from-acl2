$include "./top.sail"
$include "./two_byte_opcodes_dispatch.sail"
$include "./vex_opcodes_dispatch.sail"
$include "./evex_opcodes_dispatch.sail"

/*! Fetch and store legacy and REX prefixes, if any, of an instruction
<p>The function @ ( ' get-prefixes ' ) fetches the legacy and REX prefixes 
 of an instruction and also returns the first byte following the last such 
 prefix. The input @ ( ' start-rip ' ) points to the first byte of an instruction, 
 which may potentially be a legacy prefix. The initial value of @ ( ' cnt ' ) 
 should be @ ( ' 15 ' ) so that the result @ ( ' ( - 15 cnt ) ' ) returned at the end of 
 the recursion is the correct number of legacy and/or REX bytes parsed by this 
 function.</p> 
 
 <h3>Legacy Prefixes</h3> 
 
 <p>From Intel Manual, Vol. 2, May 2018, Section 2.1.1 ( Instruction 
 Prefixes ) :</p> 
 
 <p><em>Instruction prefixes are divided into four groups, each with a set of 
 allowable prefix codes. For each instruction, it is only useful to include 
 up to one prefix code from each of the four groups ( Groups 1, 2, 3, 
 4 ) . Groups 1 through 4 may be placed in any order relative to each 
 other.</em></p> 
 
 <p>Despite the quote from the Intel Manual above, the order of the legacy 
 prefixes does matter when there is more than one prefix from the same group 
 --- <b>all but the last prefix from a single prefix group are ignored</b>. 
 The only <b>exception</b> in this case is for <b>Group 1</b> prefixes --- see 
 below for details.</p> 
 
 <ul> 
 <li>@ ( ' 0x64_88_00 ' ) is @ ( ' mov byte ptr fs:[rax], al ' ) </li> 
 <li>@ ( ' 0x65_88_00 ' ) is @ ( ' mov byte ptr gs:[rax], al ' ) </li> 
 <li>@ ( ' 0x64_65_88_00 ' ) is @ ( ' mov byte ptr gs:[rax], al ' ) </li> 
 <li>@ ( ' 0x65_64_88_00 ' ) is @ ( ' mov byte ptr fs:[rax], al ' ) </li> 
 </ul> 
 
 <ul> 
 <li>@ ( ' 0xf2_a4 ' ) is @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf3_a4 ' ) is @ ( ' repe movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf2_f3_a4 ' ) is @ ( ' repe movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 <li>@ ( ' 0xf3_f2_a4 ' ) is @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) </li> 
 </ul> 
 
 <p>We now discuss the Group 1 exception below.</p> 
 
 <p>@ ( ' 0xf0_f2_a4 ' ) is <b>NOT</b> <br/> 
 @ ( ' repne movsb byte ptr [rdi], byte ptr [rsi] ' ) <br/> 
 It is: <br/> 
 @ ( ' lock repne movsb byte ptr [rdi], byte ptr [rsi] ' ) <br/> 
 
 Note that lock and rep/repne are Group 1 prefixes. It is important to record 
 the lock prefix, even if it is overshadowed by a rep/repne prefix, because 
 the former instruction will not @ ( ' #UD ' ) , but the latter instruction will. 
 This is akin to the lock prefix being in a separate group than the rep/repne 
 prefixes; in fact, AMD manuals ( Section 1.2.1: Summary of Legacy Prefixes, 
 Vol. 3 May 2018 Edition ) treat them as such.</p> 
 
 <p>For details about how mandatory prefixes are picked from legacy prefixes, 
 see @ ( see mandatory-prefixes-computation ) .</p> 
 
 <h3>REX Prefixes</h3> 
 
 <p>A REX prefix ( applicable only to 64-bit mode ) is treated as a null prefix 
 if it is followed by a legacy prefix. Here is an illustrative example ( using 
 Intel ' s XED, x86 Encoder Decoder --- see 
 @ ( ' https://intelxed.github.io/ ' ) ) :</p> 
 
 <ul> 
 
 <li>@ ( ' xed -64 -d 48670100 ' ) is @ ( ' add dword ptr [eax], eax ' ) ; the REX.W 
 prefix does not have any effect on the operand size, which remains 32 ( i.e., 
 the default operand size in the 64-bit mode ) .</li> 
 
 <li>@ ( ' xed -64 -d 67480100 ' ) is @ ( ' add qword ptr [eax], rax ' ) ; the REX prefix 
 has the intended effect of promoting the operand size to 64 bits.</li> 
 
 </ul> 
 
 <p>Note that the prefixes structure output of this function does not include 
 the REX byte ( which is a separate return value of this function ) , but its 
 @ ( ' :num-prefixes ' ) field includes a count of the REX prefixes encountered. 
 This is because adding an 8-bit field to the prefixes structure to store a 
 REX byte will make it a bignum, thereby impacting execution efficiency.</p> */
val get_prefixes : (range(0, 4), sbits(48), prefixes, bits(8), range(0, 15)) -> (option(string), prefixes, bits(8))

function get_prefixes (proc_mode, start_rip, prefixes, rex_byte, cnt) = {
    if cnt == 0 then {
        (Some("get_prefixes: too many bytes"), prefixes, rex_byte)
    } else {
        let (flg, byte) : (option(string), bits(8)) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(start_rip, 64), None(), 1, ":X");
        if is_some(flg) then {
            (flg, prefixes, rex_byte)
        } else if is_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte) then {
            let (flg, prefixes, rex_byte, continue?) = process_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte);
            if is_some(flg) | not_bool(continue?) then {
                (flg, prefixes, rex_byte)
            } else {
                let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                if is_some(flg) then {
                    (flg, prefixes, rex_byte)
                } else {
                    get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, rex_byte, check_range(0, 15, cnt - 1))
                }
            }
        } else {
            let prefix_byte_group_code : {|0, 1, 2, 3, 4|} = get_one_byte_prefix_array_code(byte);
            match prefix_byte_group_code {
              0 => {
                  let rex? : bool = proc_mode == 0 & bits_of_int(ash(unsigned(byte), -4), 4) == 0x4;
                  if rex? then {
                      let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                      if is_some(flg) then {
                          (flg, prefixes, rex_byte)
                      } else {
                          get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, byte, check_range(0, 15, cnt - 1))
                      }
                  } else {
                      let prefixes : prefixes = [prefixes with nxt = byte];
                      (None() : option(string), [prefixes with num = bits_of_int(15 - cnt, 4)], rex_byte)
                  }
              },
              1 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, prefixes, rex_byte)
                  } else {
                      let prefixes : prefixes = {
                          if byte == 0xf0 then {
                              [prefixes with lck = byte]
                          } else {
                              [prefixes with rep = byte]
                          }
                      };
                      get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              2 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, prefixes, rex_byte)
                  } else if proc_mode == 0 & (byte == 0x64 | byte == 0x65) | not_bool(proc_mode == 0) then {
                      get_prefixes(proc_mode, truncate(next_rip, 48), [prefixes with seg = byte], 0x00, check_range(0, 15, cnt - 1))
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              3 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, prefixes, rex_byte)
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), [prefixes with opr = byte], 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              4 => {
                  let (flg, next_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if is_some(flg) then {
                      (flg, prefixes, rex_byte)
                  } else {
                      get_prefixes(proc_mode, truncate(next_rip, 48), [prefixes with adr = byte], 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              _ => (Some("get_prefixes: unrecognised prefix byte group"), prefixes, rex_byte)
            }
        }
    }
}

/*! Top-level dispatch function.
<p>@ ( ' one-byte-opcode-execute ' ) is the doorway to all the opcode 
 maps ( for non-AVX/AVX512 instructions ) .</p> */
val one_byte_opcode_execute : (range(0, 4), sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function one_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    (match opcode {
      0 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      1 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      4 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      5 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      6 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"PUSH ES is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      7 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"POP ES is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      8 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      9 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      10 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      11 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      12 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      14 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"PUSH CS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      15 => two_byte_opcode_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode),
      16 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      17 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      18 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      19 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      20 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      21 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      22 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"PUSH SS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      23 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"POP SS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      24 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      25 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      26 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      27 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      28 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      29 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      30 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"PUSH DS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      31 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"POP DS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      32 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      33 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      34 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      35 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      36 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      37 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      38 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      39 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"DAA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      40 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      41 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      42 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      43 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      44 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      45 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      46 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      47 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"DAS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      48 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      49 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      51 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      52 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      53 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      54 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      55 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"AAA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      56 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      57 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      58 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      59 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      60 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      61 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      62 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      63 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"AAS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      64 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      65 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      66 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      67 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      68 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      69 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      70 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      71 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      72 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      73 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      74 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      75 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      76 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      77 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      78 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      79 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      80 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      81 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      82 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      83 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      84 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      85 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      86 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      87 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      88 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      89 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      90 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      91 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      92 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      93 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      94 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      95 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      96 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"PUSHA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      97 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"POPA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      98 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      99 => {
          if proc_mode == 0 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_movsx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      100 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      101 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      102 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      103 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      104 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      105 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      106 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      107 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      108 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      109 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      110 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      111 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      112 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      113 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      114 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      115 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      116 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      117 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      118 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      119 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      120 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      121 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      122 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      123 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      124 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      125 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      126 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      127 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      128 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b100 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      129 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b100 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      130 => {
          if not_bool(proc_mode == 0) & modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b100 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b101 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b110 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if not_bool(proc_mode == 0) & modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"Opcode 0x82 is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      131 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b100 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      134 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      135 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      141 => {
          let fault_var : option(string) = {
              if modr_m[mod] == 0b11 | 0xf0 == prefixes[lck] then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_lea(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      142 => {
          let fault_var : option(string) = {
              if modr_m[reg] == 0b001 | 0xf0 == prefixes[lck] then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      143 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_pop_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cbw_cwd_cdqe(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cwd_cdq_cqo(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      154 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"far CALL is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_pushf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_popf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      158 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes[lck] | proc_mode == 0 & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_sahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      159 => {
          let fault_var : option(string) = {
              if 0xf0 == prefixes[lck] | proc_mode == 0 & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  Some(":UD")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_lahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      166 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      167 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      170 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      171 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      174 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      176 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      177 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      178 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      179 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      180 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      181 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      184 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      185 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      186 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      187 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      188 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      189 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      192 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      193 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      194 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      195 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      196 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      197 => {
          if proc_mode == 0 then {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          } else if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      198 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
              let fault_var : option(string) = {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      199 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
              let fault_var : option(string) = {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                      Some(":UD")
                  } else {
                      None() : option(string)
                  }
              };
              if is_some(fault_var) then {
                  (match fault_var {
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_leave(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      202 => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib),
      203 => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib),
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      206 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"INTO is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      208 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      209 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      210 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      211 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      212 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"AAM is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      213 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"AAD is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      215 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      216 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      217 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      218 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      219 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      220 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      221 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      222 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      223 => {
          let fault_var : option(string) = {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  Some(":NM")
              } else {
                  None() : option(string)
              }
          };
          if is_some(fault_var) then {
              (match fault_var {
                Some(":NM") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-DEVICE-NOT-AVAILABLE \"#NM Encountered!\" START-RIP TEMP-RIP X86)")),
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      224 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      225 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      226 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      227 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_jrcxz(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      228 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      229 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      230 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      231 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      232 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_call_e8_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      233 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      234 => {
          if not_bool(proc_mode == 0) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    Some(":UD") => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"JMP is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)"))
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      235 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      236 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      237 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      238 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      239 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      240 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      241 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      242 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      243 => throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")),
      244 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_hlt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      245 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      246 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b110 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      247 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b110 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b111 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      248 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      249 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      250 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      251 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      252 => x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib),
      253 => {
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
                _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }) : unit
          } else {
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          }
      },
      254 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      255 => {
          if modr_m[reg] == 0b000 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b001 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b010 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_call_ff_2_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b011 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_near_jmp_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b101 & not_bool(modr_m[mod] == 0b11) then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_far_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else if modr_m[reg] == 0b110 then {
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
                    _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
                  }) : unit
              } else {
                  x86_push_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          } else {
              throw(Emsg("A fault occurred.  Original ACL2 AST: (X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"))
          }
      },
      _ => ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
    }) : unit
}

/*! Top-level step function
<p>@ ( ' x86-fetch-decode-execute ' ) is the step function of our x86 
 interpreter. It fetches one instruction by looking up the memory address 
 indicated by the instruction pointer @ ( ' rip ' ) , decodes that instruction, and 
 dispatches control to the appropriate instruction semantic function.</p> */
val x86_fetch_decode_execute : unit -> unit

function x86_fetch_decode_execute () = {
    let ctx : string = "x86-fetch-decode-execute";
    if ms() | fault() then () else {
        let proc_mode : {|0, 1|} = x86_operation_mode();
        let n64_bit_modep_var : bool = proc_mode == 0;
        let start_rip : sbits(48) = read_iptr(proc_mode);
        let (flg, prefixes, rex_byte) : (option(string), prefixes, bits(8)) = get_prefixes(proc_mode, start_rip, Mk_prefixes(0x0000000000000), 0x00, 15);
        if is_some(flg) then throw(Emsg("Model state error: :ERROR-IN-READING-PREFIXES"));
        let opcode_vex_evex_byte : bits(8) = Mk_prefixes(prefixes.bits)[nxt];
        let prefix_length : bits(4) = Mk_prefixes(prefixes.bits)[num];
        let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, start_rip, bits_of_int(1 + unsigned(prefix_length), 48));
        if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
        let vex_byte0? : bool = opcode_vex_evex_byte == 0xc5 | opcode_vex_evex_byte == 0xc4;
        let (flg, les_lds_distinguishing_byte : bits(8)) : (option(string), bits(8)) = {
            if vex_byte0? then {
                rme08(proc_mode, select_address_size(proc_mode, Some(Mk_prefixes(prefixes.bits))), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
            } else {
                (None() : option(string), 0x00)
            }
        };
        if is_some(flg) then throw(Emsg("Model state error: :LES/LDS-DISTINGUISHING-BYTE-READ-ERROR"));
        if vex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & les_lds_distinguishing_byte : bits(8)[7 .. 6] == 0b11) then {
            let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001);
            if is_some(flg) then throw(Emsg("Model state error: :VEX-BYTE1-INCREMENT-ERROR"));
            let vex_prefixes : vex_prefixes = [Mk_vex_prefixes(0x000000) with byte0 = opcode_vex_evex_byte];
            let vex_prefixes : vex_prefixes = [vex_prefixes with byte1 = les_lds_distinguishing_byte : bits(8)];
            vex_decode_and_execute(proc_mode, start_rip, truncate(temp_rip, 48), Mk_prefixes(prefixes.bits), rex_byte, vex_prefixes)
        } else {
            let opcode_evex_byte : bits(8) = opcode_vex_evex_byte;
            let evex_byte0? : bool = opcode_evex_byte == 0x62;
            let (flg, bound_distinguishing_byte) : (option(string), bits(8)) = {
                if evex_byte0? then {
                    rme08(proc_mode, select_address_size(proc_mode, Some(Mk_prefixes(prefixes.bits))), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
                } else {
                    (None() : option(string), 0x00)
                }
            };
            if is_some(flg) then throw(Emsg("Model state error: :BOUND-DISTINGUISHING-BYTE-READ-ERROR"));
            if evex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & bound_distinguishing_byte[7 .. 6] == 0b11) then {
                let (flg, temp_rip) : (option(string), sbits(49)) = add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001);
                if is_some(flg) then throw(Emsg("Model state error: :EVEX-BYTE1-INCREMENT-ERROR"));
                let evex_prefixes : evex_prefixes = [Mk_evex_prefixes(0x00000000) with byte0 = opcode_evex_byte];
                let evex_prefixes : evex_prefixes = [evex_prefixes with byte1 = bound_distinguishing_byte];
                evex_decode_and_execute(proc_mode, start_rip, truncate(temp_rip, 48), Mk_prefixes(prefixes.bits), rex_byte, evex_prefixes)
            } else {
                let opcode_byte : bits(8) = opcode_evex_byte;
                let modr_m? : bool = one_byte_opcode_modr_m_p(proc_mode, opcode_byte);
                let (flg, modr_m) : (option(string), bits(8)) = {
                    if modr_m? then {
                        if vex_byte0? | evex_byte0? then {
                            (None() : option(string), les_lds_distinguishing_byte : bits(8))
                        } else {
                            rme08(proc_mode, select_address_size(proc_mode, Some(Mk_prefixes(prefixes.bits))), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
                        }
                    } else {
                        (None() : option(string), 0x00)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :MODR/M-BYTE-READ-ERROR"));
                let (flg, temp_rip) : (option(string), sbits(49)) = {
                    if modr_m? then {
                        add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
                    } else {
                        (None() : option(string), temp_rip)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
                let sib? : bool = modr_m? & (let p4? : bool = 0x67 == Mk_prefixes(prefixes.bits)[adr] in
                let n16_bit_addressp : bool = 2 == select_address_size(proc_mode, Some(Mk_prefixes(prefixes.bits))) in
                  x86_decode_sib_p(Mk_modr_m(modr_m), n16_bit_addressp));
                let (flg, sib) : (option(string), bits(8)) = {
                    if sib? then {
                        rme08(proc_mode, select_address_size(proc_mode, Some(Mk_prefixes(prefixes.bits))), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
                    } else {
                        (None() : option(string), 0x00)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :SIB-BYTE-READ-ERROR"));
                let (flg, temp_rip) : (option(string), sbits(49)) = {
                    if sib? then {
                        add_to_iptr(proc_mode, truncate(temp_rip, 48), 0x000000000001)
                    } else {
                        (None() : option(string), temp_rip)
                    }
                };
                if is_some(flg) then throw(Emsg("Model state error: :INCREMENT-ERROR"));
                one_byte_opcode_execute(proc_mode, start_rip, truncate(temp_rip, 48), Mk_prefixes(prefixes.bits), rex_byte, opcode_byte, Mk_modr_m(modr_m), Mk_sib(sib))
            }
        }
    }
}
