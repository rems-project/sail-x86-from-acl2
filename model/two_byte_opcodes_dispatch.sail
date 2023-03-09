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
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_lldt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b101 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          1 => {
              if modr_m[reg] == 0b000 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b010 then {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b011 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b000 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b100 then {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  if feature_flags_fn([|":MONITOR"|]) == 0 | not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  if feature_flags_fn([|":MONITOR"|]) == 0 | not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b010 then {
                  if feature_flags_fn([|":SMAP"|]) == 0 | 0xf0 == prefixes[lck] | not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b011 then {
                  if feature_flags_fn([|":SMAP"|]) == 0 | 0xf0 == prefixes[lck] | not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b111 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_lgdt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_lidt(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  if feature_flags_fn([|":XSAVE"|]) == 0 | 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[osxsave] == 0b0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  if feature_flags_fn([|":XSAVE"|]) == 0 | 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[osxsave] == 0b0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b100 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b101 then {
                  if feature_flags_fn([|":RTM"|]) == 0 | 0xf0 == prefixes[lck] | 0x66 == prefixes[opr] | 0xf3 == prefixes[rep] | 0xf2 == prefixes[rep] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b110 then {
                  if feature_flags_fn([|":HLE", ":RTM"|]) == 0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b111 then {
                  if Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 then {
                      x86_fault("(:NM)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[umip] == 0b0 then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b111 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if in_64bit_mode(proc_mode) & modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b000 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & modr_m[r_m] == 0b001 then {
                  if feature_flags_fn([|":RDTSCP"|]) == 0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          2 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          3 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          5 => {
              if in_64bit_mode(proc_mode) then {
                  if 0xf0 == prefixes[lck] | Mk_ia32_eferbits(truncate(read_msr(0), 12))[sce] == 0b0 then {
                      x86_fault("(:UD)")
                  };
                  x86_syscall_both_views(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          6 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          7 => {
              if in_64bit_mode(proc_mode) then {
                  if 0xf0 == prefixes[lck] | Mk_ia32_eferbits(truncate(read_msr(0), 12))[sce] == 0b0 then {
                      x86_fault("(:UD)")
                  } else {
                      if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                          x86_fault("(:GP)")
                      };
                      ()
                  };
                  x86_sysret(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          8 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          9 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          11 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"UD2 encountered!\" START-RIP TEMP-RIP X86)"),
          13 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          16 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movss_movsd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movss_movsd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          17 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movss_movsd_op_en_mr(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movss_movsd_op_en_mr(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          18 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movlps_movlpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          19 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movlps_movlpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          20 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_unpck?ps_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_unpck?pd_op_en_rm(0, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          21 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_unpck?ps_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_unpck?pd_op_en_rm(1, proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          22 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movhps_movhpd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          23 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_movhps_movhpd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          24 => {
              if modr_m[reg] == 0b000 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b001 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & not_bool(modr_m[mod] == 0b11) then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b101 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b111 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          25 => x86_model_error("Opcode Unimplemented in x86isa!"),
          26 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                      modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                  } else if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  if feature_flags_fn([|":MPX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          27 => {
              if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                      modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                  } else if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  if feature_flags_fn([|":MPX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[r_m], rex_byte, 0b00)) | (if not_bool(in_64bit_mode(proc_mode)) & prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0xf3 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | (if in_64bit_mode(proc_mode) then {
                      modr_m[mod] == 0b00 & (modr_m[r_m] == 0b101 | modr_m[r_m] == 0b100 & sib[base] == 0b101 & sib[index] == 0b100)
                  } else if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0xf3 then {
                  if feature_flags_fn([|":MPX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  if feature_flags_fn([|":MPX"|]) == 0 | 0xf0 == prefixes[lck] | 4 <= unsigned(reg_index(modr_m[reg], rex_byte, 0b10)) | not_bool(in_64bit_mode(proc_mode)) & (if prefixes[adr] == 0x67 then {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b1
                  } else {
                      (let cs_attr : bits(16) = seg_hidden_attrs[1] in
                        Mk_code_segment_descriptor_attributesbits(cs_attr)[d]) == 0b0
                  }) then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          28 => x86_model_error("Opcode Unimplemented in x86isa!"),
          29 => x86_model_error("Opcode Unimplemented in x86isa!"),
          30 => x86_model_error("Opcode Unimplemented in x86isa!"),
          31 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_nop(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          32 => {
              if 0xf0 == prefixes[lck] | (let reg : bits(3) = modr_m[reg] in
                if in_64bit_mode(proc_mode) & logbitp(2, rex_byte) then {
                    not_bool(reg == 0b000)
                } else {
                    reg == 0b001 | reg == 0b101 | reg == 0b110 | reg == 0b111
                }) then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_mov_control_regs_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          33 => {
              if 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[de] == 0b1 & (modr_m[reg] == 0b100 | modr_m[reg] == 0b101) then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          34 => {
              if 0xf0 == prefixes[lck] | (let reg : bits(3) = modr_m[reg] in
                if in_64bit_mode(proc_mode) & logbitp(2, rex_byte) then {
                    not_bool(reg == 0b000)
                } else {
                    reg == 0b001 | reg == 0b101 | reg == 0b110 | reg == 0b111
                }) then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          35 => {
              if 0xf0 == prefixes[lck] | Mk_cr4bits(loghead(22, ctrs[4]))[de] == 0b1 & (modr_m[reg] == 0b100 | modr_m[reg] == 0b101) then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          40 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movaps_movapd_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          41 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movaps_movapd_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          43 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          48 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          49 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          50 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          51 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) & Mk_cr4bits(loghead(22, ctrs[4]))[pce] == 0b0 then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          52 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              } else {
                  if Mk_cr0bits(loghead(32, ctrs[0]))[pe] == 0b0 then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          53 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              } else {
                  if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) | Mk_cr0bits(loghead(32, ctrs[0]))[pe] == 0b0 then {
                      x86_fault("(:GP)")
                  };
                  ()
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          55 => x86_model_error("Opcode Unimplemented in x86isa!"),
          64 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          65 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          66 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          67 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          68 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          69 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          70 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          71 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          72 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          73 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          74 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          75 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          76 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          77 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          78 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          79 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmovcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          80 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          82 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          83 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          91 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          96 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          97 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          98 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          99 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          100 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          101 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          102 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          103 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          104 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          105 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          106 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          107 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          108 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          109 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          110 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movd_movq_to_xmm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          111 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          112 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          113 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          114 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b100 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          115 => {
              if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b010 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b011 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b111 & modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-7", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          117 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          118 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          119 => {
              if mandatory_prefix == 0x00 then {
                  if 0xf0 == prefixes[lck] | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          120 => {
              if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                  x86_fault("(:GP)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          121 => {
              if not_bool(unsigned(Mk_segment_selectorbits(seg_visibles[1])[rpl]) == 0) then {
                  x86_fault("(:GP)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          124 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          125 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          126 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movd_movq_from_xmm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          127 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_movups_movupd_movdqu_op_en_mr(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          128 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          129 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          130 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          131 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          132 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          133 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          134 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          135 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          136 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          137 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          138 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          139 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          140 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          141 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          142 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          143 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_two_byte_jcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          144 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          145 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          146 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          147 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          148 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          149 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          150 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          151 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          152 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          153 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          154 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          155 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          156 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          157 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          158 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          159 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_setcc(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          160 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          161 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          162 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          163 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_bt_0f_a3(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          164 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          165 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          168 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_push_segment_register(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          169 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          170 => x86_model_error("Opcode Unimplemented in x86isa!"),
          171 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          172 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          173 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_shld_shrd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          175 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_imul_op_en_rm(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          176 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          177 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_cmpxchg(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          178 => {
              if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          179 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          180 => {
              if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          181 => {
              if 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          182 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          183 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movzx(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          184 => {
              if mandatory_prefix == 0x00 then {
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  if feature_flags_fn([|":POPCNT"|]) == 0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          185 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"UD1 encountered!\" START-RIP TEMP-RIP X86)"),
          186 => {
              if modr_m[reg] == 0b100 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_bt_0f_ba(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if modr_m[reg] == 0b101 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b110 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[reg] == 0b111 then {
                  if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          187 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          189 => {
              if mandatory_prefix == 0x00 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  if 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          190 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          191 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_movsxd(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
          },
          192 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          193 => {
              if modr_m[mod] == 0b11 & 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          195 => {
              if feature_flags_fn([|":SSE2"|]) == 0 | 0xf0 == prefixes[lck] | modr_m[mod] == 0b11 then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          196 => {
              if modr_m[mod] == 0b11 & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if modr_m[mod] == 0b11 & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(modr_m[mod] == 0b11) & mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          197 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          198 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_shufps_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_shufpd_op_en_rmi(proc_mode, start_rip, temp_rip, prefixes, rex_byte, opcode, modr_m, sib)
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
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          202 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          203 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
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
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          207 => {
              if 0xf0 == prefixes[lck] then {
                  x86_fault("(:UD)")
              };
              x86_model_error("Opcode Unimplemented in x86isa!")
          },
          208 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          209 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          210 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          211 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          212 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          213 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          214 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  if feature_flags_fn([|":SSE2"|]) == 0 | Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 | Mk_cr4bits(loghead(22, ctrs[4]))[osfxsr] == 0b0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  if feature_flags_fn([|":SSE2"|]) == 0 | Mk_cr0bits(loghead(32, ctrs[0]))[ts] == 0b1 | Mk_cr0bits(loghead(32, ctrs[0]))[em] == 0b1 | Mk_cr4bits(loghead(22, ctrs[4]))[osfxsr] == 0b0 | 0xf0 == prefixes[lck] then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          216 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          217 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          218 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          220 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          221 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          222 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          224 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          225 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          226 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          227 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          228 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          229 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          230 => {
              if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf3 then {
                  chk_exc_fn(":LEGACY", ":TYPE-5", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-2", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          231 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-1", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          232 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          233 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          234 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          236 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          237 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          238 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          240 => {
              if mandatory_prefix == 0xf2 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE3"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE3"|]) == 0 | modr_m[mod] == 0b11 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          241 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          242 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          243 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          244 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          245 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          246 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":SSE"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          247 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-8", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          248 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          249 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          250 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          251 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          252 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          253 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          254 => {
              if mandatory_prefix == 0x00 then {
                  chk_exc_fn(":LEGACY", ":TYPE-22-7", [|":MMX"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":MMX"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if mandatory_prefix == 0x66 then {
                  chk_exc_fn(":LEGACY", ":TYPE-4", [|":SSE2"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":SSE2"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
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
          _ => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
        }) : unit
    }
}

/*! Decoder and dispatch function for two-byte opcodes
<p>Source: Intel Manual, Volume 2, Appendix A-2</p> */
val two_byte_opcode_decode_and_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), bits(8)) -> unit

function two_byte_opcode_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, escape_byte) = {
    let ctx : string = "two-byte-opcode-decode-and-execute";
    let opcode : bits(8) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X");
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let mandatory_prefix : bits(8) = compute_mandatory_prefix_for_two_byte_opcode(proc_mode, opcode, prefixes);
    let modr_m? : bool = two_byte_opcode_modr_m_p(proc_mode, mandatory_prefix, opcode);
    let modr_m : modr_m = {
        if modr_m? then {
            Mk_modr_m(rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X"))
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
    two_byte_opcode_execute(proc_mode, start_rip, temp_rip, prefixes, mandatory_prefix, rex_byte, opcode, modr_m, sib)
}
