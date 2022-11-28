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
