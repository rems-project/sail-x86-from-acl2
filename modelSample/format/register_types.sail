
type regtype = sbits(64)

val regval_from_reg : regtype -> sbits(64)

function regval_from_reg r = {
    r
}

val regval_into_reg : sbits(64) -> regtype

function regval_into_reg v = {
    v
}
