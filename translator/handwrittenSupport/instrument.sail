$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <string.sail>
$include "instrumentFns.sail"

register haltAddrReg : sbits(48)

// Set 64 bit mode
val initialise_64_bit_mode : unit -> unit effect {escape, rreg, wreg}
function initialise_64_bit_mode () = {
	msrs[0][11 .. 0] = [Mk_ia32_eferbits(msrs[0][11 ..0]) with lma = 0b1].bits;
	seg_hidden_attrs[1][15 .. 0] = [Mk_code_segment_descriptor_attributesbits(seg_hidden_attrs[1][15 .. 0]) with l = 0b1].bits;
}

// Initialise state for the add1 example
val initialiseState : unit -> unit effect {escape, wreg, rreg}
function initialiseState () = {
	// Set rip
	rip = sail_zero_extend(0x40065D, 48);

	// rdi = 1
	write_rgfi(7, sail_zero_extend(0b1, 64));

	// rsp = 2 ^ 45
	write_rgfi(4, sail_shiftleft(sail_zero_extend(0b1, 64), 45));

	// rflags = 2
	rflags = Mk_rflagsbits(sail_zero_extend(0x2, 32));

	// Set 64 bit mode
	initialise_64_bit_mode();

	// Set halt address
	haltAddrReg = sail_zero_extend(0x400662, 48)
}

// Initialise state for the K-framework example
val initialiseState2 : unit -> unit effect {escape, wreg, rreg}
function initialiseState2 () = {
	// Set rip
	rip = bits_of_int(4194867, 48); // Original start to this file: 4194867; the instruction which fails in the interpreter: 4196698

	// rdi = 1
	//x = write_rgfi(7, 1, 0);

	// rsp = 2 ^ 45
	write_rgfi(4, sail_shiftleft(sail_zero_extend(0b1, 64), 45));

	// rflags = 2
	rflags = Mk_rflagsbits(sail_zero_extend(0x2, 32));

	// Set 64 bit mode
	initialise_64_bit_mode()
}

// Using the __SetConfig method.  See aarch32 example for example.
val __SetConfig : (string, int) -> unit effect {escape, rreg, wreg}
function __SetConfig (arg, value) =
	match arg {
		"rip" => { rip = get_slice_int(48, value, 0) },
		"rdi" => { write_rgfi(7, get_slice_int(64, value, 0)) },
		"rsp" => { write_rgfi(4, get_slice_int(64, value, 0)) },
		"rflags" => { rflags = Mk_rflagsbits(get_slice_int(32, value, 0)) },
		"cr4" => { ctrs[4] = get_slice_int(64, value, 0) },
		"ha" => { haltAddrReg = get_slice_int(48, value, 0) },
		"set64bit" => {
			// Ignore the value
			initialise_64_bit_mode()
		},
		"app_view" => { app_view = (value != 0) },
		_ => throw(Emsg("Unrecognised Config"))
	}

// This should print information about __SetConfig to stderr.
val __ListConfig : unit -> unit
function __ListConfig () = ()

// This function is so we can compile to a C emulator which does pretty does everything
val doEverything : unit -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function doEverything () = {
	//initialiseState();
	//steps = x86_run_halt(4195938, 10000, 0, true, false, 0); // 4195938 for original example; 4197494 for test.s; 4194880 for debug.s
	steps = x86_run_halt(haltAddrReg, 10000, true, false, 0);
	print_int("Steps = ", steps);
}

// A main function for C compilation
val main : unit -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function main () =
	doEverything()
	//x86_step_print_halt(haltAddrReg, true, false, 0)
