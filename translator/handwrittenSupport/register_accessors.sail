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

val read_rip : unit -> sbits(64)
function read_rip() = regval_from_reg(rip)

val write_rip : sbits(64) -> unit
function write_rip(value) = {
    if canonical_address_p(signed(value)) then {
        rip = regval_into_reg(value)
    } else {
        x86_fault("write_rip: Non-canonical address " + bits_str(value))
    };
    if log_register_writes then log_rip_write()
}

type msr_index = range(0, 6)

val read_msr : msr_index -> bits(64)
function read_msr(idx) = msrs[idx]

val write_msr : (msr_index, bits(64)) -> unit
function write_msr(idx, value) = {
    msrs[idx] = value
}

$endif
