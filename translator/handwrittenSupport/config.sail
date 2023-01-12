$ifndef __X86_CONFIG
$define __X86_CONFIG

function clause __SetConfig("rip", value) = write_rip(get_slice_int(48, value, 0))
function clause __SetConfig("rax", value) = write_rgfi(0, get_slice_int(64, value, 0))
function clause __SetConfig("rbx", value) = write_rgfi(3, get_slice_int(64, value, 0))
function clause __SetConfig("rcx", value) = write_rgfi(1, get_slice_int(64, value, 0))
function clause __SetConfig("rdx", value) = write_rgfi(2, get_slice_int(64, value, 0))
function clause __SetConfig("rsi", value) = write_rgfi(6, get_slice_int(64, value, 0))
function clause __SetConfig("rdi", value) = write_rgfi(7, get_slice_int(64, value, 0))
function clause __SetConfig("rsp", value) = write_rgfi(4, get_slice_int(64, value, 0))
function clause __SetConfig("rbp", value) = write_rgfi(5, get_slice_int(64, value, 0))
function clause __SetConfig("r8", value) = write_rgfi(8, get_slice_int(64, value, 0))
function clause __SetConfig("r9", value) = write_rgfi(9, get_slice_int(64, value, 0))
function clause __SetConfig("r10", value) = write_rgfi(10, get_slice_int(64, value, 0))
function clause __SetConfig("r11", value) = write_rgfi(11, get_slice_int(64, value, 0))
function clause __SetConfig("r12", value) = write_rgfi(12, get_slice_int(64, value, 0))
function clause __SetConfig("r13", value) = write_rgfi(13, get_slice_int(64, value, 0))
function clause __SetConfig("r14", value) = write_rgfi(14, get_slice_int(64, value, 0))
function clause __SetConfig("r15", value) = write_rgfi(15, get_slice_int(64, value, 0))
function clause __SetConfig("rflags", value) = { rflags = Mk_rflagsbits(get_slice_int(32, value, 0)) }
function clause __SetConfig("cr4", value) = { ctrs[4] = get_slice_int(64, value, 0) }
function clause __SetConfig("app_view", value) = { app_view = (value != 0) }

// TODO: This should print information about __SetConfig to stderr.
val __ListConfig : unit -> unit
function __ListConfig () = ()

$endif
