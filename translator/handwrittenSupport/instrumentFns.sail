$include "out.sail"

val printState : unit -> unit effect {rreg, escape}
function printState () = {
	print_bits("rax = ", rgfi(0));
	print_bits("rbx = ", rgfi(3));
	print_bits("rcx = ", rgfi(1));
	print_bits("rdx = ", rgfi(2));
	print_bits("rsi = ", rgfi(6));
	print_bits("rdi = ", rgfi(7));
	print_bits("rbp = ", rgfi(5));
	print_bits("rsp = ", rgfi(4));
	print_bits("r8 = ", rgfi(8));
	print_bits("r9 = ", rgfi(9));
	print_bits("r10 = ", rgfi(10));
	print_bits("r11 = ", rgfi(11));
	print_bits("r12 = ", rgfi(12));
	print_bits("r13 = ", rgfi(13));
	print_bits("r14 = ", rgfi(14));
	print_bits("r15 = ", rgfi(15));

	print_bits("rflags = ", rflags.bits);

	print_bits("rip = ", read_rip());
}

function log_memory_read(addr, value) = {
	print_endline(bits_str(read_rip()) + ": R " + bits_str(addr) + " " + dec_str(1) + " " + bits_str(value))
}

function log_memory_write(addr, value) = {
	print_endline(bits_str(read_rip()) + ": W " + bits_str(addr) + " " + dec_str(1) + " " + bits_str(value))
}

val x86_step_print_halt : (sbits(48), bool, bool, int) -> bool effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
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
val x86_run_halt : (sbits(48), int, bool, bool, int) -> int effect {eamem, escape, rmem, rreg, undef, wmv, wreg}
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
