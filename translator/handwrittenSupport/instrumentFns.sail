val printState : unit -> unit effect {rreg, escape}
function printState () = {
    x86_dummy = 0;
	print_bits("rax = ", get_slice_int(64, rgfi(0, x86_dummy), 0));
	print_bits("rbx = ", get_slice_int(64, rgfi(3, x86_dummy), 0));
	print_bits("rcx = ", get_slice_int(64, rgfi(1, x86_dummy), 0));
	print_bits("rdx = ", get_slice_int(64, rgfi(2, x86_dummy), 0));
	print_bits("rsi = ", get_slice_int(64, rgfi(6, x86_dummy), 0));
	print_bits("rdi = ", get_slice_int(64, rgfi(7, x86_dummy), 0));
	print_bits("rbp = ", get_slice_int(64, rgfi(5, x86_dummy), 0));
	print_bits("rsp = ", get_slice_int(64, rgfi(4, x86_dummy), 0));
	print_bits("r8 = ", get_slice_int(64, rgfi(8, x86_dummy), 0));
	print_bits("r9 = ", get_slice_int(64, rgfi(9, x86_dummy), 0));
	print_bits("r10 = ", get_slice_int(64, rgfi(10, x86_dummy), 0));
	print_bits("r11 = ", get_slice_int(64, rgfi(11, x86_dummy), 0));
	print_bits("r12 = ", get_slice_int(64, rgfi(12, x86_dummy), 0));
	print_bits("r13 = ", get_slice_int(64, rgfi(13, x86_dummy), 0));
	print_bits("r14 = ", get_slice_int(64, rgfi(14, x86_dummy), 0));
	print_bits("r15 = ", get_slice_int(64, rgfi(15, x86_dummy), 0));

	print_bits("rflags = ", get_slice_int(64, r_rflags(0), 0));

	print_bits("rip = ", get_slice_int(64, ripi(0), 0));
}

val x86_step_print_halt : (int, bool, bool, int) -> bool effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_step_print_halt(haltAddr, printEachStep, printCount, count) =
	if ripi(0) == haltAddr
	then true
	else
		let _ = x86_fetch_decode_execute(0) in
		let _ =
			if printEachStep
			then printState()
			else () in
		let _ =
			if printCount
			then print_int("Steps = ", count + 1)
			else ()
		in false

// Simulates the same ACL2 function
val x86_run_halt : (int, int, int, bool, bool, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
function x86_run_halt (haltAddr, maxSteps, x86_dummy, printEachStep, printCount, count) =
	//if ripi(0) == haltAddr then count else
	if maxSteps == 0 then count
	else
		try {
			halt? = x86_step_print_halt(haltAddr, printEachStep, printCount, count + 1);
			if halt?
			then count
			else x86_run_halt(haltAddr, maxSteps - 1, 0, printEachStep, printCount, count + 1)
		}
		catch {
				Emsg(msg) => {print_endline(msg); count}
		}
