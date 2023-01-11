val get_prefixes : (proc_mode, sbits(64), prefixes, bits(8), range(0, 15)) -> (prefixes, bits(8))

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
                let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
                get_prefixes(proc_mode, next_rip, prefixes, rex_byte, check_range(0, 15, cnt - 1))
            }
        } else {
            let prefix_byte_group_code : {|0, 1, 2, 3, 4|} = get_one_byte_prefix_array_code(byte);
            match prefix_byte_group_code {
              0 => {
                  let rex? : bool = in_64bit_mode(proc_mode) & bits_of_int(ash(unsigned(byte), -4), 4) == 0x4;
                  if rex? then {
                      let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
                      get_prefixes(proc_mode, next_rip, prefixes, byte, check_range(0, 15, cnt - 1))
                  } else {
                      let prefixes : prefixes = [prefixes with nxt = byte];
                      ([prefixes with num = bits_of_int(15 - cnt, 4)], rex_byte)
                  }
              },
              1 => {
                  let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
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
                  let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
                  if in_64bit_mode(proc_mode) & (byte == 0x64 | byte == 0x65) | not_bool(in_64bit_mode(proc_mode)) then {
                      get_prefixes(proc_mode, next_rip, [prefixes with seg = byte], 0x00, check_range(0, 15, cnt - 1))
                  } else {
                      get_prefixes(proc_mode, next_rip, prefixes, 0x00, check_range(0, 15, cnt - 1))
                  }
              },
              3 => {
                  let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
                  get_prefixes(proc_mode, next_rip, [prefixes with opr = byte], 0x00, check_range(0, 15, cnt - 1))
              },
              4 => {
                  let next_rip : sbits(64) = add_to_iptr(proc_mode, start_rip, 0x0000000000000001);
                  get_prefixes(proc_mode, next_rip, [prefixes with adr = byte], 0x00, check_range(0, 15, cnt - 1))
              },
              _ => (prefixes, rex_byte)
            }
        }
    }
}
