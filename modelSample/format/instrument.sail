
default Order dec
$include <prelude.sail>
$include "./instrumentFns.sail"

register haltAddrReg : sbits(48)

val initialise_64_bit_mode : unit -> unit

function initialise_64_bit_mode () = {
    msrs[0][11 .. 0] = [Mk_ia32_eferbits(msrs[0][11 .. 0]) with lma = 0b1].bits;
    seg_hidden_attrs[1][15 .. 0] = [Mk_code_segment_descriptor_attributesbits(seg_hidden_attrs[1][15 .. 0]) with l = 0b1].bits
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
    match arg {
      "rip" => {
          rip = get_slice_int(48, value, 0)
      },
      "rdi" => {
          write_rgfi(7, get_slice_int(64, value, 0))
      },
      "rsp" => {
          write_rgfi(4, get_slice_int(64, value, 0))
      },
      "rflags" => {
          rflags = get_slice_int(32, value, 0)
      },
      "cr4" => {
          ctrs[4] = get_slice_int(64, value, 0)
      },
      "ha" => {
          haltAddrReg = get_slice_int(48, value, 0)
      },
      "set64bit" => {
          initialise_64_bit_mode()
      },
      "app_view" => {
          app_view = value != 0
      },
      _ => throw(Emsg("Unrecognised Config"))
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
