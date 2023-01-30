$ifndef __X86_REGISTER_ACCESSORS
$define __X86_REGISTER_ACCESSORS

val rgfi : (int) -> sbits(64)
function rgfi (regNum) = {
    assert(0 <= regNum & regNum < 16);
    regval_from_reg(read_gpr(regNum))
}

val write_rgfi : (int, sbits(64)) -> unit
function write_rgfi (regNum, regval_to_write) = {
    assert(0 <= regNum & regNum < 16);
    write_gpr(regNum, regval_into_reg(regval_to_write))
}

val read_rip : unit -> sbits(48)
function read_rip() = truncate(regval_from_reg(rip), 48)

val write_rip : sbits(48) -> unit
function write_rip(value) = {
    rip = regval_into_reg(sail_sign_extend(value, 64))
}

type msr_index = range(0, 6)

val read_msr : msr_index -> bits(64)
function read_msr(idx) = msrs[idx]

val write_msr : (msr_index, bits(64)) -> unit
function write_msr(idx, value) = {
    msrs[idx] = value
}

$endif
