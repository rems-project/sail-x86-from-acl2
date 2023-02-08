$ifndef __X86_LOGGING
$define __X86_LOGGING

val printState : unit -> unit
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

function log_gpr_write(idx) = {
    match idx {
        0 => print_bits("rax = ", rgfi(0)),
        1 => print_bits("rcx = ", rgfi(1)),
        2 => print_bits("rdx = ", rgfi(2)),
        3 => print_bits("rbx = ", rgfi(3)),
        4 => print_bits("rsp = ", rgfi(4)),
        5 => print_bits("rbp = ", rgfi(5)),
        6 => print_bits("rsi = ", rgfi(6)),
        7 => print_bits("rdi = ", rgfi(7)),
        8 => print_bits("r8 = ", rgfi(8)),
        9 => print_bits("r9 = ", rgfi(9)),
        10 => print_bits("r10 = ", rgfi(10)),
        11 => print_bits("r11 = ", rgfi(11)),
        12 => print_bits("r12 = ", rgfi(12)),
        13 => print_bits("r13 = ", rgfi(13)),
        14 => print_bits("r14 = ", rgfi(14)),
        15 => print_bits("r15 = ", rgfi(15))
    }
}

function log_rflags_write() = {
    print_bits("rflags = ", rflags.bits);
}

function log_rip_write() = {
    print_bits("rip = ", read_rip());
}

function log_memory_read(addr, len, value) = {
	print_endline(bits_str(read_rip()) + ": R " + bits_str(addr) + " " + dec_str(len) + " " + bits_str(value))
}

function log_memory_write(addr, len, value) = {
	print_endline(bits_str(read_rip()) + ": W " + bits_str(addr) + " " + dec_str(len) + " " + bits_str(value))
}

$endif
