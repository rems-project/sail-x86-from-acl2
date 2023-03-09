
/*! Dispatch function for EVEX-encoded instructions in the two-byte opcode map */
val evex_0f_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function evex_0f_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    if ext_evex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) then {
        return()
    } else {
        (match opcode {
          16 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          17 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          18 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & modr_m[mod] == 0b11 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & not_bool(modr_m[mod] == 0b11) then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          19 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          20 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          21 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          22 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & not_bool(modr_m[mod] == 0b11) then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & modr_m[mod] == 0b11 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          23 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          40 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          41 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          42 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          43 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          44 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          45 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          46 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          47 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          65 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          66 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          68 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          69 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          70 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          71 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          74 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          75 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          80 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          81 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          82 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          83 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          84 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          85 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          86 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          87 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          88 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          89 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          90 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          91 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          92 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          93 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          94 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          95 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          96 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          97 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          98 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          99 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          100 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          101 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          102 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          103 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          104 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          105 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          106 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          107 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          108 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          109 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          110 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          111 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          112 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          113 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          114 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b000 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b100 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          115 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b011 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b011 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b011 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b111 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b111 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & modr_m[reg] == 0b111 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          116 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          117 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          118 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          119 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          120 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          121 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b00 & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          122 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          123 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          124 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          125 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          126 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          127 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          144 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          145 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          146 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          147 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          152 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          153 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          174 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          194 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          196 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          197 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          198 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if evex_get_pp(evex_prefixes) == 0b00 & not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          208 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          209 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          210 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          211 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          212 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          213 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          214 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          215 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          216 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          217 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          218 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          219 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          220 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          221 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          222 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          223 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          224 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          225 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          226 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          227 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          228 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          229 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          230 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          231 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          232 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          233 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          234 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          235 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          236 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          237 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          238 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          239 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          240 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          241 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          242 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          243 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          244 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          245 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          246 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          247 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          248 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          249 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          250 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          251 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          252 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          253 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          254 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          _ => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
        }) : unit
    }
}

/*! Dispatch function for EVEX-encoded instructions in the first 
 three-byte opcode map */
val evex_0f38_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function evex_0f38_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    if ext_evex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) then {
        return()
    } else {
        (match opcode {
          0 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          1 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          2 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          3 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          4 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          5 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          6 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          7 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          8 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          9 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          10 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          11 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          12 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          13 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          14 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          15 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          16 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          17 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          18 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          19 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          20 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          21 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          22 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          23 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          24 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          25 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          26 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          27 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          28 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          29 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          30 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          31 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          32 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          33 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          34 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          35 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          36 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          37 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          38 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          39 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          40 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          41 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          42 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E1NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          43 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          44 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          45 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          46 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          47 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          48 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          49 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          50 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          51 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          52 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          53 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E5", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          54 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          55 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          56 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          57 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E7NM", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          58 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b10 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          59 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          60 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          61 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          62 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          63 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          64 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          65 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          66 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          67 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          68 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          69 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          70 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          71 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          76 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          77 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          78 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          79 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E10", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          82 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_4VNNIW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4VNNIW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          83 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_4VNNIW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4VNNIW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          88 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          89 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          90 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          91 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          100 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          101 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          102 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          117 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          118 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          119 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          120 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          121 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          122 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          123 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          124 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          125 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          126 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          127 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          131 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512_VBMI", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512_VBMI", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          136 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          137 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          138 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          139 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          140 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          141 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512_VBMI"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_VBMI"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          142 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          144 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          145 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          146 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          147 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          150 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          151 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          152 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          153 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          154 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512_4FMAPS"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4FMAPS"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          155 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512_4FMAPS"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4FMAPS"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          156 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          157 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          158 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          159 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          160 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          161 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          162 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          163 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          166 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          167 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          168 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          169 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          170 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512_4FMAPS"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4FMAPS"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          171 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b11 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512_4FMAPS"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_4FMAPS"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          172 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          173 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          174 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          175 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          180 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          181 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA", ":AVX512VL"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA", ":AVX512VL"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512_IFMA"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512_IFMA"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          182 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          183 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          184 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          185 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          186 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          187 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          188 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          189 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          190 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          191 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          196 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512CD"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512CD"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          198 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b101 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b101 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          199 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b001 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b010 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b101 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b101 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & modr_m[reg] == 0b110 then {
                  chk_exc_fn(":LEGACY", ":TYPE-E12NP", [|":AVX512PF"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512PF"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          200 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          202 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          203 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          204 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          205 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512ER"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512ER"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          219 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          220 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          221 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          222 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          223 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          242 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          243 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          245 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          246 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          247 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          _ => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
        }) : unit
    }
}

/*! Dispatch function for EVEX-encoded instructions in the second 
 three-byte opcode map */
val evex_0f3a_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), evex_prefixes, bits(8), modr_m, sib) -> unit

function evex_0f3a_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) = {
    if ext_evex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib) then {
        return()
    } else {
        (match opcode {
          0 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          1 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          2 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          3 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          4 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          5 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          6 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          8 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          9 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          10 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          11 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          12 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          13 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          14 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          15 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          20 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          21 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          22 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          23 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          24 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          25 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          26 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          27 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          29 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E11", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          30 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          31 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          32 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          33 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          34 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E9NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          35 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          37 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          38 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          39 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          48 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          49 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          50 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          51 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          56 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          57 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          58 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          59 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          62 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          63 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          64 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          65 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          66 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512VL", ":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF.NB", [|":AVX512BW"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512BW"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          67 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4NF", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          68 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          70 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          74 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          75 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          76 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          80 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          81 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          84 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          85 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512F"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512F"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          86 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E2", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          87 => {
              if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if not_bool(evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E3", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          96 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          97 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          98 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          99 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          102 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b00 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b01 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512VL", ":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512VL", ":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_vl_rc(evex_prefixes) == 0b10 & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E4", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          103 => {
              if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b1 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else if (evex_get_vvvv(evex_prefixes) == 0xf & evex_get_v_prime(evex_prefixes) == 0b1) & evex_get_pp(evex_prefixes) == 0b01 & evex_get_w(evex_prefixes) == 0b0 & false then {
                  chk_exc_fn(":LEGACY", ":TYPE-E6", [|":AVX512DQ"|], proc_mode, prefixes, rex_byte, opcode, modr_m, sib);
                  if feature_flags_fn([|":AVX512DQ"|]) == 0 then {
                      x86_fault("(:UD)")
                  };
                  x86_model_error("Opcode Unimplemented in x86isa!")
              } else {
                  x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
              }
          },
          223 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          240 => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)"),
          _ => x86_fault("(X86-ILLEGAL-INSTRUCTION \"#UD Encountered!\" START-RIP TEMP-RIP X86)")
        }) : unit
    }
}

/*! <p>@ ( ' evex-decode-and-execute ' ) dispatches control to EVEX-encoded 
 instructions.</p> 
 
 <p><i>Reference: Intel Vol. 2A, Section 2.6: Intel ( R ) AVX-512 
 Encoding</i></p> */
val evex_decode_and_execute : (proc_mode, sbits(48), sbits(48), prefixes, bits(8), evex_prefixes) -> unit

function evex_decode_and_execute (proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes) = {
    let ctx : string = "evex-decode-and-execute";
    if not_bool(rex_byte == 0x00) then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :rex rex-byte)");
    if prefixes[lck] == 0xf0 then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :lock-prefix)");
    if prefixes[rep] == 0xf2 then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :F2-prefix)");
    if prefixes[rep] == 0xf3 then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :F3-prefix)");
    if prefixes[opr] == 0x66 then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :66-prefix)");
    let evex_byte1 : bits(8) = evex_prefixes[byte1];
    if not_bool(Mk_evex_byte1(evex_byte1)[res] == 0b00) then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :byte1-reserved-bits)");
    let (evex_0f_map?, evex_0f38_map?, evex_0f3a_map?) : (bool, bool, bool) = (Mk_evex_byte1(evex_byte1)[mm] == 0b01, Mk_evex_byte1(evex_byte1)[mm] == 0b10, Mk_evex_byte1(evex_byte1)[mm] == 0b11);
    if not_bool(evex_0f_map? | evex_0f38_map? | evex_0f3a_map?) then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :mm evex-byte1)");
    let evex_byte2 : bits(8) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X");
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let evex_prefixes : evex_prefixes = [evex_prefixes with byte2 = evex_byte2];
    if not_bool(Mk_evex_byte2(evex_byte2)[res] == 0b1) then x86_fault("(!!fault-fresh :ud :evex-prefixes evex-prefixes :byte2-reserved-bit)");
    let evex_byte3 : bits(8) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X");
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let opcode : bits(8) = rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X");
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let modr_m : modr_m = Mk_modr_m(rme08(proc_mode, select_address_size(proc_mode, Some(prefixes)), sail_sign_extend(temp_rip, 64), None(), 1, ":X"));
    let temp_rip : sbits(48) = add_to_iptr(proc_mode, temp_rip, 0x000000000001);
    let sib? : bool = {
        let p4? : bool = 0x67 == prefixes[adr];
        let n16_bit_addressp : bool = 2 == select_address_size(proc_mode, Some(prefixes));
        x86_decode_sib_p(modr_m, n16_bit_addressp)
    };
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
    if evex_0f_map? then {
        evex_0f_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib)
    } else if evex_0f38_map? then {
        evex_0f38_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib)
    } else if evex_0f3a_map? then {
        evex_0f3a_execute(proc_mode, start_rip, temp_rip, prefixes, rex_byte, evex_prefixes, opcode, modr_m, sib)
    } else {
        x86_model_error(":ILLEGAL-VALUE-OF-EVEX-MM")
    }
}
