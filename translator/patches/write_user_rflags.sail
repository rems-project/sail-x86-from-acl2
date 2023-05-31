val write_user_rflags : (rflagsbits, rflagsbits) -> unit

function write_user_rflags (user_flags_vector, undefined_mask) = {
    let input_rflags : rflagsbits = rflags;
    if undefined_mask[cf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with cf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with cf = user_flags_vector[cf]];
        rflags = new_rflags
    };
    if undefined_mask[pf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with pf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with pf = user_flags_vector[pf]];
        rflags = new_rflags
    };
    if undefined_mask[af] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with af = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with af = user_flags_vector[af]];
        rflags = new_rflags
    };
    if undefined_mask[zf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with zf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with zf = user_flags_vector[zf]];
        rflags = new_rflags
    };
    if undefined_mask[sf] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with sf = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with sf = user_flags_vector[sf]];
        rflags = new_rflags
    };
    if undefined_mask[of] == 0b1 then {
        let rflags_var : rflagsbits = rflags;
        let sailval : bits(1) = undef_flg();
        let new_rflags : rflagsbits = [rflags_var with of = sailval];
        rflags = new_rflags
    } else {
        let rflags_var : rflagsbits = rflags;
        let new_rflags : rflagsbits = [rflags_var with of = user_flags_vector[of]];
        rflags = new_rflags
    };
    if log_register_writes then log_rflags_write()
}
