// Dummy registers which really ought to be mapped to the model properly

register eflags : bits(32)
register cs : bits(32)
register ss : bits(32)
register ds : bits(32)
register es : bits(32)
register fs : bits(32)
register gs : bits(32)

// Fake floating point registers to make gdb happy

register st0 : bits(80)
register st1 : bits(80)
register st2 : bits(80)
register st3 : bits(80)
register st4 : bits(80)
register st5 : bits(80)
register st6 : bits(80)
register st7 : bits(80)

register fctrl : bits(16)
register fstat : bits(16)
register ftag : bits(16)

// sizes from gdbserver, which I don't really believe...
register fiseg : bits(32)
register fioff : bits(32)
register foseg : bits(32)
register fooff : bits(32)
register fop : bits(32)

let gdb_architecture = "i386:x86-64"
let gdb_features : list((string, list(string), list((string,string)))) =
  [| ("org.gnu.gdb.i386.core",
      [| "eflags", "rip",
         "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rsp", "rbp",
         "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
         "cs", "ss", "ds", "es", "fs", "gs",
         "fctrl", "fstat", "ftag", "fiseg", "fioff", "foseg", "fooff", "fop" |],
    [| ("st0", "i387_ext"),
       ("st1", "i387_ext"),
       ("st2", "i387_ext"),
       ("st3", "i387_ext"),
       ("st4", "i387_ext"),
       ("st5", "i387_ext"),
       ("st6", "i387_ext"),
       ("st7", "i387_ext") |]) |]
