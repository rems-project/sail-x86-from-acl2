// A bitvector version which has no branching when 'n is fixed.

val logcount_bits : forall 'n, 'n >= 0. (bits('n)) -> int
function logcount_bits(x) = {
  var r : int = 0;
  foreach (i from 0 to ('n - 1)) {
    r = r + unsigned([x[i]]);
  };
  r
}

// Remove the bitvector to integer conversions

val pf_spec8 : bits(8) -> bits(1)

function pf_spec8 result = {
    bool_to_bits(not_bool(logbitp(0, logcount((result)))))
}

val pf_spec16 : bits(16) -> bits(1)

function pf_spec16 result = {
    bool_to_bits(not_bool(logbitp(0, logcount((loghead(8, result))))))
}

val pf_spec32 : bits(32) -> bits(1)

function pf_spec32 result = {
    bool_to_bits(not_bool(logbitp(0, logcount((loghead(8, result))))))
}

val pf_spec64 : bits(64) -> bits(1)

function pf_spec64 result = {
    bool_to_bits(not_bool(logbitp(0, logcount((loghead(8, result))))))
}
