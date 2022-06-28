
default Order dec
$include <prelude.sail>
$include "./instrumentFns.sail"

register haltAddrReg : sbits(48)

val initialise_64_bit_mode : unit -> unit

function initialise_64_bit_mode () = {
    msrs[0] = sail_zero_extend(set_ia32_eferbits_get_lma(0b1, truncate(msrs[0], 12)), 64);
    seg_hidden_attrs[1] = set_code_segment_descriptor_attributesbits_get_l(0b1, seg_hidden_attrs[1])
}

val initialiseState : unit -> unit

function initialiseState () = {
    rip = sail_zero_extend(0x40065D, 48);
    write_rgfi(7, sail_zero_extend(0b1, 64));
    write_rgfi(4, sail_shiftleft(sail_zero_extend(0b1, 64), 45));
    rflags = sail_zero_extend(0x2, 32);
    initialise_64_bit_mode();
    haltAddrReg = sail_zero_extend(0x400662, 48)
}

val initialiseState2 : unit -> unit

function initialiseState2 () = {
    rip = bits_of_int(4194867, 48);
    write_rgfi(4, sail_shiftleft(sail_zero_extend(0b1, 64), 45));
    rflags = sail_zero_extend(0x2, 32);
    initialise_64_bit_mode()
}

val __SetConfig : (string, int) -> unit

function __SetConfig (arg, value) = {
    match () {
      () if arg == "rip" => {
          rip = get_slice_int(48, value, 0)
      },
      () if arg == "rdi" => {
          write_rgfi(7, get_slice_int(64, value, 0))
      },
      () if arg == "rsp" => {
          write_rgfi(4, get_slice_int(64, value, 0))
      },
      () if arg == "rflags" => {
          rflags = get_slice_int(32, value, 0)
      },
      () if arg == "ha" => {
          haltAddrReg = get_slice_int(48, value, 0)
      },
      () if arg == "set64bit" => {
          initialise_64_bit_mode()
      },
      _ => {
          throw(Emsg("Unrecognised Config"))
      }
    }
}

val __ListConfig : unit -> unit

function __ListConfig () = ()

val doEverything : unit -> unit

function doEverything () = {
    steps = x86_run_halt(haltAddrReg, 10000, true, false, 0);
    print_int("Steps = ", steps)
}

val main : unit -> unit

function main () = {
    doEverything()
}
