/*
To use this file in the interpreter you must first load the sail file contain the x86 step function
You also need to load an ELF file to do anything useful.
*/

$ifndef _DEFAULT_DEC
	default Order dec
$endif

$include <prelude.sail>
$include <string.sail>
$include "out.sail"
$include "instrumentFns.sail"

register haltAddrReg : int

// Initialise state for the add1 example
val initialiseState : unit -> unit effect {escape, wreg}
function initialiseState () = {
	// Set rip
	x = write_rip(4195933, 0);

	// rdi = 1
	x = write_rgfi(7, 1, 0);

	// rsp = 2 ^ 45
	x = write_rgfi(4, pow2(45), 0);

	// rflags = 2
	x = write_rflags(2, 0);
}

// Initialise state for the K-framework example
val initialiseState2 : unit -> unit effect {escape, wreg}
function initialiseState2 () = {
	// Set rip
	x = write_rip(4194867, 0); // Original start to this file: 4194867; the instruction which fails in the interpreter: 4196698

	// rdi = 1
	//x = write_rgfi(7, 1, 0);

	// rsp = 2 ^ 45
	x = write_rgfi(4, pow2(45), 0);

	// rflags = 2
	x = write_rflags(2, 0);
}

// Set 64 bit mode
val initialise_64_bit_mode : unit -> unit effect {escape, rreg, wreg}
function initialise_64_bit_mode () = {
	let x = write_msri(0, set_ia32_eferbits_get_lma(1,  msri(0,0)), 0) in
	let x = write_seg_hidden_attri(1, set_code_segment_descriptor_attributesbits_get_l(1, seg_hidden_attri(1,0)), 0) in
	()
}

// Using the __SetConfig method.  See aarch32 example for example.
val __SetConfig : (string, int) -> unit effect {escape, rreg, wreg}
function __SetConfig (arg, value) =
	match () {
		() if arg == "rip" => {
			x = write_rip(value, 0)
		},
		() if arg == "rdi" => {
			x = write_rgfi(7, value, 0)
		},
		() if arg == "rsp" => {
			x = write_rgfi(4, value, 0)
		},
		() if arg == "rflags" => {
			x = write_rflags(value, 0)
		},
		() if arg == "ha" => {
			haltAddrReg = value
		},
		() if arg == "set64bit" => {
			// Ignore the value
			initialise_64_bit_mode()
		},
		_ => {
			throw(Emsg("Unrecognised Config"))
		}
	}

// This should print information about __SetConfig to stderr.
val __ListConfig : unit -> unit
function __ListConfig () = ()

// This function is so we can compile to a C emulator which does pretty does everything
val doEverything : unit -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function doEverything () = {
	//initialiseState();
	//steps = x86_run_halt(4195938, 10000, 0, true, false, 0); // 4195938 for original example; 4197494 for test.s; 4194880 for debug.s
	steps = x86_run_halt(haltAddrReg, 10000, 0, true, false, 0);
	print_int("Steps = ", steps);
}

// A main function for C compilation
val main : unit -> unit effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function main () =
	doEverything()
	//x86_step_print_halt(haltAddrReg, true, false, 0)
