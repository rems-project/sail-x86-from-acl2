register haltAddrReg : sbits(48)

function clause __SetConfig("ha", value) = { haltAddrReg = get_slice_int(48, value, 0) }

// Set 64 bit mode
val initialise_64_bit_mode : unit -> unit
function initialise_64_bit_mode () = {
	let msr0 = read_msr(0);
	let efer = Mk_ia32_eferbits(msr0[11 .. 0]);
	let new_efer = [efer with lma = 0b1];
	write_msr(0, [msr0 with 11 .. 0 = new_efer.bits]);
	seg_hidden_attrs[1] = [Mk_code_segment_descriptor_attributesbits(seg_hidden_attrs[1]) with l = 0b1].bits;
}

function clause __SetConfig("set64bit", value) = {
  // Ignore the value
  initialise_64_bit_mode()
}
function clause __SetConfig(_, _) = throw(Emsg("Unrecognised Config"))
end __SetConfig

val x86_step_print_halt : (sbits(48), bool, bool, int) -> bool
function x86_step_print_halt(haltAddr, printEachStep, printCount, count) = {
	if read_rip() == haltAddr then {
		return true
	} else {
		x86_fetch_decode_execute();
		if printEachStep then printState();
		if printCount then print_int("Steps = ", count + 1);
		return false
	}
}

// Simulates the same ACL2 function
val x86_run_halt : (sbits(48), int, bool, bool, int) -> int
function x86_run_halt (haltAddr, maxSteps, printEachStep, printCount, count) = {
	if maxSteps == 0 then {
		return count
	} else {
		try {
			let halt? : bool = x86_step_print_halt(haltAddr, printEachStep, printCount, count + 1);
			if halt?  then {
				return count
			} else {
				return x86_run_halt(haltAddr, maxSteps - 1, printEachStep, printCount, count + 1)
			}
		}
		catch {
			Emsg(msg) => { print_endline(msg); return count }
		}
	}
}

// A main function for C compilation
val main : unit -> unit
function main () = {
	initialize_model();
	steps = x86_run_halt(haltAddrReg, 10000, not_bool(log_register_writes), false, 0);
	print_int("Steps = ", steps);
}
