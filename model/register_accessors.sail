$ifndef __X86_REGISTER_ACCESSORS
$define __X86_REGISTER_ACCESSORS

val rgfi : (int) -> sbits(64) effect {escape, rreg}
function rgfi (regNum) = {
    assert(0 <= regNum & regNum < 16);
    regval_from_reg(read_gpr(regNum))
}

val write_rgfi : (int, sbits(64)) -> unit effect {escape, wreg}
function write_rgfi (regNum, regval_to_write) = {
    assert(0 <= regNum & regNum < 16);
    write_gpr(regNum, regval_into_reg(regval_to_write))
}

val read_rip : unit -> sbits(48)
function read_rip() = truncate(regval_from_reg(rip), 48)

val write_rip : sbits(48) -> unit
function write_rip(value) = {
    rip = regval_into_reg(sail_zero_extend(value, 64))
}

$endif
