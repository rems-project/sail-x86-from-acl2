val bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)
function bitslice (x, low, width) = {
	assert('n >= 0 & low >= 0 & width > 0);
	if (low + width <= 'n) then {
		slice(x, low, width)
	} else if (low < 'n - 1) then {
		sail_zeros(low + width - 'n) @ x['n - 1 .. low]
	} else {
		sail_zeros(width)
	}
}

val signed_bitslice : forall 'n 'low 'width. (bits('n), int('low), int('width)) -> bits('width)
function signed_bitslice (x, low, width) = {
	assert('n >= 0 & low >= 0 & width > 0);
	if (low + width <= 'n) then {
		slice(x, low, width)
	} else if (low < 'n - 1) then {
		// Sign-extend by replicating the MSB
		replicate_bits([x['n - 1]], low + width - 'n) @ x['n - 1 .. low]
	} else {
		sail_zeros(width)
	}
}
