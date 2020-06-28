val printState : unit -> unit effect {rreg}
function printState () = {
	print_bits("rax = ", gprs[0]);
	print_bits("rbx = ", gprs[3]);
	print_bits("rcx = ", gprs[1]);
	print_bits("rdx = ", gprs[2]);
	print_bits("rsi = ", gprs[6]);
	print_bits("rdi = ", gprs[7]);
	print_bits("rbp = ", gprs[5]);
	print_bits("rsp = ", gprs[4]);
	print_bits("r8 = ", gprs[8]);
	print_bits("r9 = ", gprs[9]);
	print_bits("r10 = ", gprs[10]);
	print_bits("r11 = ", gprs[11]);
	print_bits("r12 = ", gprs[12]);
	print_bits("r13 = ", gprs[13]);
	print_bits("r14 = ", gprs[14]);
	print_bits("r15 = ", gprs[15]);

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
