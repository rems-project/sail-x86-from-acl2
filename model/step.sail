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
val get_prefixes : (proc_mode, sbits(48), prefixes, bits(8), range(0, 15)) -> (prefixes, bits(8))

function get_prefixes (proc_mode, start_rip, prefixes, rex_byte, cnt) = {
    if cnt == 0 then {
        (prefixes, rex_byte)
    } else {
        let byte : bits(8) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(start_rip, 64), None(), 1, ":X");
        if is_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte) then {
            let (prefixes, rex_byte, continue?) = process_ext_prefix_byte(proc_mode, prefixes, rex_byte, cnt, byte);
            if not_bool(continue?) then {
                (prefixes, rex_byte)
            } else {
                let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                get_prefixes(proc_mode, next_rip, prefixes, rex_byte, check_range(0, 15, cnt - 1))
            }
        } else {
            let prefix_byte_group_code : {|0, 1, 2, 3, 4|} = get_one_byte_prefix_array_code(byte);
            match prefix_byte_group_code {
              0 => {
                  let rex? : bool = in_64bit_mode(proc_mode) & bits_of_int(ash(unsigned(byte), -4), 4) == 0x4;
                  if rex? then {
                      let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                      get_prefixes(proc_mode, next_rip, prefixes, byte, check_range(0, 15, cnt - 1))
                  } else {
                      let prefixes : prefixes = [prefixes with nxt = byte];
                      ([prefixes with num = bits_of_int(15 - cnt, 4)], rex_byte)
                  }
              },
              1 => {
                  let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  let prefixes : prefixes = {
                      if byte == 0xf0 then {
                          [prefixes with lck = byte]
                      } else {
                          [prefixes with rep = byte]
                      }
                  };
                  get_prefixes(proc_mode, next_rip, prefixes, 0x00, check_range(0, 15, cnt - 1))
              },
              2 => {
                  let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  if in_64bit_mode(proc_mode) & (byte == 0x64 | byte == 0x65) | not_bool(in_64bit_mode(proc_mode)) then {
                      get_prefixes(proc_mode, next_rip, [prefixes with seg = byte], 0x00, check_range(0, 15, cnt - 1))
                  } else {
                      get_prefixes(proc_mode, next_rip, prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              3 => {
                  let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  get_prefixes(proc_mode, next_rip, [prefixes with opr = byte], 0x00, check_range(0, 15, cnt - 1))
              },
              4 => {
                  let next_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, 0x000000000001);
                  get_prefixes(proc_mode, next_rip, [prefixes with adr = byte], 0x00, check_range(0, 15, cnt - 1))
              },
              _ => (prefixes, rex_byte)
            }
        }
    }
}

/*! Top-level dispatch function.
<p>@ ( ' one-byte-opcode-execute ' ) is the doorway to all the opcode 
 maps ( for non-AVX/AVX512 instructions ) .</p> */
val one_byte_opcode_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8), modr_m, sib) -> unit

function one_byte_opcode_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) = {
    if ext_one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib) then {
        return()
    } else {
        (match opcode {
          0 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          1 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          2 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          3 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          4 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          5 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          6 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"PUSH ES is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          7 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"POP ES is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          8 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          9 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          10 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          11 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          12 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          13 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          14 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"PUSH CS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          15 => two_byte_opcode_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode),
          16 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          17 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          18 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          19 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          20 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          21 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          22 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"PUSH SS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          23 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"POP SS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          24 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          25 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          26 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          27 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          28 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          29 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          30 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"PUSH DS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          31 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"POP DS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          32 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          33 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          34 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          35 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          36 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          37 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          38 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          39 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"DAA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          40 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          41 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          42 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          43 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          44 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          45 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          46 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          47 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"DAS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          48 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          49 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          50 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          51 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          52 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          53 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          54 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          55 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"AAA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          56 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          57 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          58 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          59 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_g_e(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          60 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          61 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          62 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          63 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"AAS is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          64 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          65 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          66 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          67 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          68 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          69 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          70 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          71 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          72 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          73 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          74 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          75 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          76 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          77 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          78 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          79 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_4x(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          80 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          81 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          82 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          83 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          84 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          85 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          86 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          87 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          88 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          89 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          90 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          91 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          92 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          93 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          94 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          95 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pop_general_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          96 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_pusha(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"PUSHA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          97 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_popa(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"POPA is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          98 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          99 => {
              if in_64bit_mode(proc_mode) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_movsx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          100 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          101 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          102 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          103 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          104 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          105 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          106 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_i(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          107 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_imul_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          108 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          109 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          110 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          111 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          112 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          113 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          114 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          115 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          116 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          117 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          118 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          119 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          120 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          121 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          122 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          123 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          124 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          125 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          126 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          127 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_one_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          128 => {
              if modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          129 => {
              if modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          130 => {
              if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b100 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b101 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b110 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if not_bool(in_64bit_mode(proc_mode)) & modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"Opcode 0x82 is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          131 => {
              if modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(2, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(6, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(3, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(4, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(5, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(8, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          132 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          133 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_g(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          134 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          135 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          136 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          137 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          138 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          139 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          140 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          141 => {
              if modr_m[mod] == 0b11 | 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_lea(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          142 => {
              if modr_m[reg] == 0b001 | 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          143 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_pop_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          144 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              if logbitp(0, rex_byte) then {
                  x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_one_byte_nop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              }
          },
          145 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          146 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          147 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          148 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          149 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          150 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          151 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_xchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          152 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cbw_cwd_cdqe(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          153 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cwd_cdq_cqo(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          154 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"far CALL is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          155 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          156 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_pushf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          157 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_popf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          158 => {
              if 0xf0 == prefixes[lck] | in_64bit_mode(proc_mode) & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  x86_fault("(:UD)")
              };
              x86_sahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          159 => {
              if 0xf0 == prefixes[lck] | in_64bit_mode(proc_mode) & feature_flag_fn(":LAHF-SAHF") == 0 then {
                  x86_fault("(:UD)")
              };
              x86_lahf(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          160 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          161 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_fd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          162 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          163 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_td(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          164 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          165 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movs(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          166 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          167 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmps(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          168 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          169 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_add_adc_sub_sbb_or_and_xor_cmp_test_rax_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          170 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          171 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_stos(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          172 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          173 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          174 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          175 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          176 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          177 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          178 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          179 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          180 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          181 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          182 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          183 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          184 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          185 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          186 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          187 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          188 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          189 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          190 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          191 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_mov_op_en_oi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          192 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          193 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          194 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          195 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_ret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          196 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          197 => {
              if in_64bit_mode(proc_mode) then {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              } else if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          198 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          199 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_mov_op_en_mi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          200 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          201 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_leave(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          202 => x86_model_error("Opcode Unimplemented in x86isa!"),
          203 => x86_model_error("Opcode Unimplemented in x86isa!"),
          204 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          205 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          206 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"INTO is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          207 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          208 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          209 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          210 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          211 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_sal_sar_shl_shr_rcl_rcr_rol_ror(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          212 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"AAM is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          213 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"AAD is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          215 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          216 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          217 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          218 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          219 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          220 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          221 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          222 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          223 => {
              if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                  x86_fault("(:NM)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          224 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          225 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          226 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_loop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          227 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_jrcxz(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          228 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          229 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          230 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          231 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          232 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_call_e8_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          233 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          234 => {
              if not_bool(in_64bit_mode(proc_mode)) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) then {
                  if true then {
                      x86_fault("(:UD)")
                  };
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"JMP is illegal in the 64-bit mode!\" START-RIP TEMP-RIP X86)")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          235 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_near_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          236 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          237 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          238 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          239 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          240 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          241 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          242 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          243 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          244 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_hlt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          245 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          246 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          247 => {
              if modr_m[reg] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_add_adc_sub_sbb_or_and_xor_cmp_test_e_i(7, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_not_neg_f6_f7(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_mul(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_imul_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_div(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b111 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_idiv(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          248 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          249 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          250 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          251 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          252 => x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib),
          253 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmc_clc_stc_cld_std(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          254 => {
              if modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          255 => {
              if modr_m[reg] == 0b000 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b001 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_inc_dec_fe_ff(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_call_ff_2_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_near_jmp_op_en_m(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_far_jmp_op_en_d(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b110 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_push_ev(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          _ => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
        }) : unit
    }
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
        let proc_mode : proc_mode = x86_operation_mode();
        let n64_bit_modep_var : bool = in_64bit_mode(proc_mode);
        let start_rip : sbits(48) = read_iptr(proc_mode);
        let (prefixes, rex_byte) : (prefixes, bits(8)) = get_prefixes(proc_mode, start_rip, Mk_prefixes(0x0000000000000), 0x00, 15);
        let prefixes : prefixes = Mk_prefixes(prefixes.bits);
        let opcode_vex_evex_byte : bits(8) = prefixes[nxt];
        let prefix_length : bits(4) = prefixes[num];
        let temp_rip : sbits(48) = add_to_iptr(proc_mode, start_rip, bits_of_int(1 + unsigned(prefix_length), 48));
        let vex_byte0? : bool = opcode_vex_evex_byte == 0xc5 | opcode_vex_evex_byte == 0xc4;
        let les_lds_distinguishing_byte : bits(8) : bits(8) = {
            if vex_byte0? then {
                rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
            } else 0x00
        };
        if vex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & les_lds_distinguishing_byte : bits(8)[7 .. 6] == 0b11) then {
            let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
            let vex_prefixes : vex_prefixes = [Mk_vex_prefixes(0x000000) with byte0 = opcode_vex_evex_byte];
            let vex_prefixes : vex_prefixes = [vex_prefixes with byte1 = les_lds_distinguishing_byte : bits(8)];
            vex_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, vex_prefixes)
        } else {
            let opcode_evex_byte : bits(8) = opcode_vex_evex_byte;
            let evex_byte0? : bool = opcode_evex_byte == 0x62;
            let bound_distinguishing_byte : bits(8) = {
                if evex_byte0? then {
                    rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X")
                } else 0x00
            };
            if evex_byte0? & (n64_bit_modep_var | not_bool(n64_bit_modep_var) & bound_distinguishing_byte[7 .. 6] == 0b11) then {
                let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
                let evex_prefixes : evex_prefixes = [Mk_evex_prefixes(0x00000000) with byte0 = opcode_evex_byte];
                let evex_prefixes : evex_prefixes = [evex_prefixes with byte1 = bound_distinguishing_byte];
                evex_decode_and_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes)
            } else {
                let opcode_byte : bits(8) = opcode_evex_byte;
                let modr_m? : bool = one_byte_opcode_modr_m_p(proc_mode, opcode_byte);
                let modr_m : modr_m = {
                    if modr_m? then {
                        if vex_byte0? | evex_byte0? then {
                            Mk_modr_m(les_lds_distinguishing_byte : bits(8))
                        } else {
                            Mk_modr_m(rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X"))
                        }
                    } else {
                        Mk_modr_m(0x00)
                    }
                };
                let temp_rip : sbits(48) = {
                    if modr_m? then {
                        add_to_iptr(proc_mode, temp_rip, 0x000000000001)
                    } else {
                        temp_rip
                    }
                };
                let sib? : bool = modr_m? & (let p4? : bool = 0x67 == prefixes[adr] in
                let n16_bit_addressp : bool = 2 == select_address_size(proc_mode, Some(prefixes)) in
                  x86_decode_sib_p(modr_m, n16_bit_addressp));
                let sib : sib = {
                    if sib? then {
                        Mk_sib(rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X"))
                    } else {
                        Mk_sib(0x00)
                    }
                };
                let temp_rip : sbits(48) = {
                    if sib? then {
                        add_to_iptr(proc_mode, temp_rip, 0x000000000001)
                    } else {
                        temp_rip
                    }
                };
                one_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode_byte, modr_m, sib)
            }
        }
    }
}
