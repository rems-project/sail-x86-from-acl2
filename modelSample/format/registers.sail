
register rflags : bits(32)

register rip : sbits(48)

register rax : regtype

register rbx : regtype

register rcx : regtype

register rdx : regtype

register rsi : regtype

register rdi : regtype

register rsp : regtype

register rbp : regtype

register r8 : regtype

register r9 : regtype

register r10 : regtype

register r11 : regtype

register r12 : regtype

register r13 : regtype

register r14 : regtype

register r15 : regtype

val read_gpr : range(0, 15) -> regtype

function read_gpr idx = {
    match idx {
      0 => rax,
      1 => rcx,
      2 => rdx,
      3 => rbx,
      4 => rsp,
      5 => rbp,
      6 => rsi,
      7 => rdi,
      8 => r8,
      9 => r9,
      10 => r10,
      11 => r11,
      12 => r12,
      13 => r13,
      14 => r14,
      15 => r15
    }
}

val write_gpr : (range(0, 15), regtype) -> unit

function write_gpr (idx, value) = {
    match idx {
      0 => rax = value,
      1 => rcx = value,
      2 => rdx = value,
      3 => rbx = value,
      4 => rsp = value,
      5 => rbp = value,
      6 => rsi = value,
      7 => rdi = value,
      8 => r8 = value,
      9 => r9 = value,
      10 => r10 = value,
      11 => r11 = value,
      12 => r12 = value,
      13 => r13 = value,
      14 => r14 = value,
      15 => r15 = value
    }
}

register msrs : vector(7, dec, bits(64))

register seg_visibles : vector(6, dec, bits(16))

register seg_hidden_attrs : vector(6, dec, bits(16))

register seg_hidden_bases : vector(6, dec, bits(64))

register seg_hidden_limits : vector(6, dec, bits(32))

register zmms : vector(32, dec, bits(512))

register ctrs : vector(17, dec, bits(64))

register strs : vector(2, dec, bits(80))

register ssr_visibles : vector(2, dec, bits(16))

register ssr_hidden_bases : vector(2, dec, bits(64))

register ssr_hidden_limits : vector(2, dec, bits(32))

register ssr_hidden_attrs : vector(2, dec, bits(16))
