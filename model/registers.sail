$ifndef __X86_REGISTERS
$define __X86_REGISTERS

$include "structures.sail"

// Although one would expect rflags to be 64 bits, the ACL2 models
// it as 32 bits
register rflags : rflagsbits

register rip : regtype

// In ACL2 rgfi() is often called with a `defconst` argument.  E.g.
//      ` (rgfi *rax*) `
// Unfortunately the translator currently translates these as number literals.
// So (ignoring the dummy x86 variable) the above will become:
//      ` rgfi(0)`
// To find the number *rax* maps to, one can simply type `*rax*` into and ACL2
// terminal with the x86 ISA project loaded.
//
// Each of the gprs is defined separately below, as opposed to a vector of
// bitvectors as other registers are.  This is so they play nicely with
// concurrency tools.  It also means rgfi() and write_rgfi() must work out
// which reg to access themselves, instead of indexing an array.
//
// See here for some hints on rgfi:
// http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____GPRS-READS-AND-WRITES?path=3532/29063/10660/6710/25183

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
function read_gpr(idx) = {
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
function write_gpr(idx, value) = {
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
    };
    if log_register_writes then log_gpr_write(idx);
}

/*
Model Specific Registers

Footnote b in section 5.1 of Goel thesis says only 6 MSR registers
(I think they mean 7) are thus far defined in the model.  For now,
we enumerate some of them explicitely here.  Fortuantely, the indexes
range from 0..6, and we can lump them into a vector of registers

TODO: make this enumeration automatic
*/
register msrs : vector(7, dec, bits(64)) // 64 bits as per table 5.1 in thesis

/*
seg-visible registers etc..  From `concrete-state.lisp` we find
there are 6 x 16bit registers

TODO: make this automatic (including the number 6, which is a global
constant but subject to change)

TODO: in `xr` in ACL2 we have things like seg_hidden_attri* (i.e.
with a trailing asterisk). Why is this?
*/
register seg_visibles : vector(6, dec, bits(16))

register seg_hidden_attrs : vector(6, dec, bits(16))

register seg_hidden_bases : vector(6, dec, bits(64))

register seg_hidden_limits : vector(6, dec, bits(32))

/*
zmm registers.  From `concrete-state.lisp` we find there are
32 x 512bit registers

TODO: make this automatic (including the number 32, which is a global
constant but subject to change)
*/
register zmms : vector (32, dec, bits(512))

/*
Control registers.  From `concrete-state.lisp` we find there are
17 x 64bit registers

TODO: make this automatic (including the number 17, which is a global
constant but subject to change)
*/
register ctrs : vector (17, dec, bits(64))

/*
We can use `:pe ...` from `stri` and find we need the following.
*/
register strs : vector(2, dec, bits(80))

/*
ssr
*/
register ssr_visibles : vector(2, dec, bits(16))

register ssr_hidden_bases : vector(2, dec, bits(64))

register ssr_hidden_limits : vector(2, dec, bits(32))

register ssr_hidden_attrs : vector(2, dec, bits(16))

$endif
