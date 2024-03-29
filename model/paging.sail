$include "./paging_structures.sail"
$include "./physical_memory.sail"

val page_fault_err_no : (bits(1), string, bits(2), bits(1), bits(1), bits(1), bits(1)) -> int

function page_fault_err_no (p_flag, r_w_x, cpl, rsvd, smep, pae, nxe) = {
    logior(if p_flag == 0b1 then 1 else 0, if r_w_x == ":W" then 2 else 0)
}

val page_fault_exception : (sbits(48), int) -> (bool, {|0|})

function page_fault_exception (addr, err_no) = {
    let old_faults : bool = fault();
    let new_faults : ((string, (int, sbits(48))), bool) = ((":PAGE-FAULT", (err_no, addr)), old_faults);
    x86_fault("(!fault new-faults x86)");
    (true, 0)
}

val page_present : bits(64) -> bits(1)

function page_present entry = {
    Mk_ia32e_page_tablesbits(entry)[p]
}

val page_size : bits(64) -> bits(1)

function page_size entry = {
    Mk_ia32e_page_tablesbits(entry)[ps]
}

val page_read_write : bits(64) -> bits(1)

function page_read_write entry = {
    Mk_ia32e_page_tablesbits(entry)[r_w]
}

val page_user_supervisor : bits(64) -> bits(1)

function page_user_supervisor entry = {
    Mk_ia32e_page_tablesbits(entry)[u_s]
}

val page_execute_disable : bits(64) -> bits(1)

function page_execute_disable entry = {
    Mk_ia32e_page_tablesbits(entry)[xd]
}

val accessed_bit : bits(64) -> bits(1)

function accessed_bit entry = {
    Mk_ia32e_page_tablesbits(entry)[a]
}

val dirty_bit : bits(64) -> bits(1)

function dirty_bit entry = {
    Mk_ia32e_page_tablesbits(entry)[d]
}

val set_accessed_bit : bits(64) -> ia32e_page_tablesbits

function set_accessed_bit entry = {
    [Mk_ia32e_page_tablesbits(entry) with a = 0b1]
}

val set_dirty_bit : bits(64) -> ia32e_page_tablesbits

function set_dirty_bit entry = {
    [Mk_ia32e_page_tablesbits(entry) with d = 0b1]
}

val page_table_entry_addr : (sbits(48), bits(52)) -> bits(52)

function page_table_entry_addr (lin_addr, base_addr) = {
    assert(unsigned_byte_p(52, unsigned(base_addr)) & loghead(12, base_addr) == 0x000);
    changeSlice(base_addr, 3, 9, lin_addr[20 .. 12])
}

val page_directory_entry_addr : (sbits(48), bits(52)) -> bits(52)

function page_directory_entry_addr (lin_addr, base_addr) = {
    assert(unsigned_byte_p(52, unsigned(base_addr)) & loghead(12, base_addr) == 0x000);
    changeSlice(base_addr, 3, 9, lin_addr[29 .. 21])
}

val page_dir_ptr_table_entry_addr : (sbits(48), bits(52)) -> bits(52)

function page_dir_ptr_table_entry_addr (lin_addr, base_addr) = {
    assert(unsigned_byte_p(52, unsigned(base_addr)) & loghead(12, base_addr) == 0x000);
    changeSlice(base_addr, 3, 9, lin_addr[38 .. 30])
}

val pml4_table_entry_addr : (sbits(48), bits(52)) -> bits(52)

function pml4_table_entry_addr (lin_addr, base_addr) = {
    assert(unsigned_byte_p(52, unsigned(base_addr)) & loghead(12, base_addr) == 0x000);
    changeSlice(base_addr, 3, 9, lin_addr[47 .. 39])
}

/*! Determining access rights and detecting page faults
<p>Source for determining the access rights: Section 4.6 in 
 the Intel Manuals, Vol. 3A.</p> 
 
 <p>It is important to differentiate between:</p> 
 
 <ol> 
 <li>a supervisor-mode and a user-mode access</li> 
 <li>an implicit and an explicit supervisor-mode access</li> 
 <li>a supervisor-mode and a user-mode address</li> 
 </ol> 
 
 <p>These concepts are defined below.</p> 
 
 <ul> 
 
 <li>Every access to a linear address is either a <b>supervisor-mode 
 access</b> or a <b>user-mode access</b>. For all instruction fetches 
 and most data accesses, accesses made while CPL @ ( ' < ' ) 3 are 
 <b>supervisor-mode accesses</b>, while accesses made while CPL = 3 are 
 <b>user-mode accesses</b>.</li> 
 
 <li>Some operations implicitly access system data structures with 
 linear addresses; the resulting accesses to those data structures are 
 supervisor-mode accesses regardless of CPL. Examples of such accesses 
 include the following: accesses to the global descriptor table \ ( GDT\ ) 
 or local descriptor table \ ( LDT\ ) to load a segment descriptor; 
 accesses to the interrupt descriptor table \ ( IDT\ ) when delivering an 
 interrupt or exception; and accesses to the task-state segment \ ( TSS\ ) 
 as part of a task switch or change of CPL. All these accesses are 
 called <b>implicit supervisor-mode accesses</b> regardless of 
 CPL. Other accesses made while CPL @ ( ' < ' ) 3 are called <b>explicit 
 supervisor-mode accesses</b>.</li> 
 
 <li>If the U/S flag \ ( bit 2\ ) is 0 in at least one of the 
 paging-structure entries, the address is a <b>supervisor-mode 
 address</b>. Otherwise, the address is a <b>user-mode 
 address</b>.</li> 
 
 <p><i>TO-DO:</i> For now, we are treating all supervisor-mode accesses 
 as explicit. We need to detect and then account for implicit 
 accesses.</p> 
 
 </ul> */
struct struct_paging_entry_no_page_fault_p = {supervisor_mode_access_type : int}

val paging_entry_no_page_fault_p : (bits(2), sbits(48), bits(64), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2), struct_paging_entry_no_page_fault_p) -> (bool, {|0|})

function paging_entry_no_page_fault_p (structure_type, lin_addr, entry, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl, keywords) = {
    let entry : bits(64) = loghead(64, entry);
    let page_present_var : bits(1) = page_present(entry);
    if page_present_var == 0b0 then {
        let err_no : int = page_fault_err_no(page_present_var, r_w_x, cpl, 0b0, smep, 0b1, nxe);
        page_fault_exception(lin_addr, err_no)
    } else {
        let read_write : bits(1) = page_read_write(entry);
        let r_w_all : bits(1) = logand(read_write, r_w_acc);
        let user_supervisor : bits(1) = page_user_supervisor(entry);
        let u_s_all : bits(1) = logand(user_supervisor, u_s_acc);
        let execute_disable : bits(1) = page_execute_disable(entry);
        let x_d_all : bits(1) = logand(execute_disable, x_d_acc);
        let page_size_var : bits(1) = page_size(entry);
        let rsvd : {|0, 1|} = {
            if structure_type == 0b11 & page_size_var == 0b1 | not_bool(structure_type == 0b00) & not_bool(structure_type == 0b11) & page_size_var == 0b1 & (if structure_type == 0b01 then {
                not_bool(entry[20 .. 13] == 0x00)
            } else {
                not_bool(entry[29 .. 13] == 0b00000000000000000)
            }) | nxe == 0b0 & not_bool(execute_disable == 0b0) then 1 else 0
        };
        if rsvd == 1 then {
            let err_no : int = page_fault_err_no(page_present_var, r_w_x, cpl, bits_of_int(rsvd, 1), smep, 0b1, nxe);
            page_fault_exception(lin_addr, err_no)
        } else if r_w_x == ":R" & (if unsigned(cpl) < 3 then {
            if u_s_all == 0b0 then false else if smap == 0b0 then false else {
                keywords.supervisor_mode_access_type == 1 | ac == 0b0
            }
        } else {
            u_s_all == 0b0
        }) | r_w_x == ":W" & (if unsigned(cpl) < 3 then {
            if u_s_all == 0b0 then {
                wp == 0b1 & r_w_all == 0b0
            } else if wp == 0b0 then {
                if smap == 0b0 then false else {
                    keywords.supervisor_mode_access_type == 1 | ac == 0b0
                }
            } else if smap == 0b0 then {
                r_w_all == 0b0
            } else if keywords.supervisor_mode_access_type == 0 & ac == 0b1 then {
                r_w_all == 0b0
            } else true
        } else {
            u_s_all == 0b0 | r_w_all == 0b0
        }) | r_w_x == ":X" & (if unsigned(cpl) < 3 then {
            if u_s_all == 0b0 then {
                if nxe == 0b0 then false else {
                    x_d_all == 0b1
                }
            } else if smep == 0b0 then {
                if nxe == 0b0 then false else {
                    x_d_all == 0b1
                }
            } else true
        } else {
            u_s_all == 0b0 | nxe == 0b1 & x_d_all == 0b1
        }) then {
            let err_no : int = page_fault_err_no(page_present_var, r_w_x, cpl, bits_of_int(rsvd, 1), smep, 0b1, nxe);
            page_fault_exception(lin_addr, err_no)
        } else {
            (false, 0)
        }
    }
}

val ia32e_la_to_pa_page_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> int

function ia32e_la_to_pa_page_table (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) = {
    assert(not_bool(app_view));
    let lin_addr : int = binary_logext(48, unsigned(loghead(48, lin_addr)));
    let base_addr : bits(52) = changeSlice(loghead(52, base_addr), 0, 12, 0x000);
    let p_entry_addr : bits(52) = page_table_entry_addr(bits_of_int(lin_addr, 48), base_addr);
    let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
    let (fault_flg, sailval) : (bool, {|0|}) = paging_entry_no_page_fault_p(0b00, bits_of_int(lin_addr, 48), entry, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl, struct { supervisor_mode_access_type = 0 });
    if fault_flg then x86_model_error("Page-Fault");
    if marking_view then {
        let accessed : bits(1) = accessed_bit(entry);
        let dirty : bits(1) = dirty_bit(entry);
        let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
            if accessed == 0b0 then {
                (true, set_accessed_bit(entry))
            } else {
                (false, Mk_ia32e_page_tablesbits(entry))
            }
        };
        let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
            if dirty == 0b0 & r_w_x == ":W" then {
                (true, set_dirty_bit(entry.bits))
            } else {
                (updated?, entry)
            }
        };
        if updated? then {
            wm_low_64(p_entry_addr, entry.bits)
        };
        ()
    };
    changeSlice(ash(unsigned(Mk_ia32e_pte_4k_pagebits(entry)[page]), 12), 0, 12, unsigned(get_slice_int(12, lin_addr, 0)))
}

val ia32e_la_to_pa_page_directory : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> int

function ia32e_la_to_pa_page_directory (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) = {
    assert(not_bool(app_view));
    let lin_addr : int = binary_logext(48, unsigned(loghead(48, lin_addr)));
    let base_addr : bits(52) = changeSlice(loghead(52, base_addr), 0, 12, 0x000);
    let p_entry_addr : bits(52) = page_directory_entry_addr(bits_of_int(lin_addr, 48), base_addr);
    let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
    let u_s_all : bits(1) = logand(u_s_acc, page_user_supervisor(entry));
    let r_w_all : bits(1) = logand(r_w_acc, page_read_write(entry));
    let x_d_all : bits(1) = logand(x_d_acc, page_execute_disable(entry));
    let (fault_flg, sailval) : (bool, {|0|}) = paging_entry_no_page_fault_p(0b01, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct { supervisor_mode_access_type = 0 });
    if fault_flg then x86_model_error("Page-Fault");
    if page_size(entry) == 0b1 then {
        if marking_view then {
            let accessed : bits(1) = accessed_bit(entry);
            let dirty : bits(1) = dirty_bit(entry);
            let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
                if accessed == 0b0 then {
                    (true, set_accessed_bit(entry))
                } else {
                    (false, Mk_ia32e_page_tablesbits(entry))
                }
            };
            let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
                if dirty == 0b0 & r_w_x == ":W" then {
                    (true, set_dirty_bit(entry.bits))
                } else {
                    (updated?, entry)
                }
            };
            if updated? then {
                wm_low_64(p_entry_addr, entry.bits)
            };
            ()
        };
        changeSlice(ash(unsigned(Mk_ia32e_pde_2mb_pagebits(entry)[page]), 21), 0, 21, unsigned(get_slice_int(21, lin_addr, 0)))
    } else {
        let page_table_base_addr : int = ash(unsigned(Mk_ia32e_pde_pg_tablebits(entry)[pt]), 12);
        let p_addr : bits(52) = bits_of_int(ia32e_la_to_pa_page_table(bits_of_int(lin_addr, 48), bits_of_int(page_table_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl), 52);
        if marking_view then {
            let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
            let accessed : bits(1) = accessed_bit(entry);
            let entry : ia32e_page_tablesbits = {
                if accessed == 0b0 then {
                    set_accessed_bit(entry)
                } else {
                    Mk_ia32e_page_tablesbits(entry)
                }
            };
            if accessed == 0b0 then {
                wm_low_64(p_entry_addr, entry.bits)
            };
            ()
        };
        unsigned(p_addr)
    }
}

val ia32e_la_to_pa_page_dir_ptr_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> int

function ia32e_la_to_pa_page_dir_ptr_table (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) = {
    assert(not_bool(app_view));
    let lin_addr : int = binary_logext(48, unsigned(loghead(48, lin_addr)));
    let base_addr : bits(52) = changeSlice(loghead(52, base_addr), 0, 12, 0x000);
    let p_entry_addr : bits(52) = page_dir_ptr_table_entry_addr(bits_of_int(lin_addr, 48), base_addr);
    let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
    let u_s_all : bits(1) = logand(u_s_acc, page_user_supervisor(entry));
    let r_w_all : bits(1) = logand(r_w_acc, page_read_write(entry));
    let x_d_all : bits(1) = logand(x_d_acc, page_execute_disable(entry));
    let (fault_flg, sailval) : (bool, {|0|}) = paging_entry_no_page_fault_p(0b10, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct { supervisor_mode_access_type = 0 });
    if fault_flg then x86_model_error("Page-Fault");
    if page_size(entry) == 0b1 then {
        if marking_view then {
            let accessed : bits(1) = accessed_bit(entry);
            let dirty : bits(1) = dirty_bit(entry);
            let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
                if accessed == 0b0 then {
                    (true, set_accessed_bit(entry))
                } else {
                    (false, Mk_ia32e_page_tablesbits(entry))
                }
            };
            let (updated?, entry) : (bool, ia32e_page_tablesbits) = {
                if dirty == 0b0 & r_w_x == ":W" then {
                    (true, set_dirty_bit(entry.bits))
                } else {
                    (updated?, entry)
                }
            };
            if updated? then {
                wm_low_64(p_entry_addr, entry.bits)
            };
            ()
        };
        changeSlice(ash(unsigned(Mk_ia32e_pdpte_1gb_pagebits(entry)[page]), 30), 0, 30, unsigned(get_slice_int(30, lin_addr, 0)))
    } else {
        let page_directory_base_addr : int = ash(unsigned(Mk_ia32e_pdpte_pg_dirbits(entry)[pd]), 12);
        let p_addr : bits(52) = bits_of_int(ia32e_la_to_pa_page_directory(bits_of_int(lin_addr, 48), bits_of_int(page_directory_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl), 52);
        if marking_view then {
            let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
            let accessed : bits(1) = accessed_bit(entry);
            let entry : ia32e_page_tablesbits = {
                if accessed == 0b0 then {
                    set_accessed_bit(entry)
                } else {
                    Mk_ia32e_page_tablesbits(entry)
                }
            };
            if accessed == 0b0 then {
                wm_low_64(p_entry_addr, entry.bits)
            };
            ()
        };
        unsigned(p_addr)
    }
}

val ia32e_la_to_pa_pml4_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> bits(52)

function ia32e_la_to_pa_pml4_table (lin_addr, base_addr, wp, smep, smap, ac, nxe, r_w_x, cpl) = {
    assert(not_bool(app_view));
    let lin_addr : int = binary_logext(48, unsigned(loghead(48, lin_addr)));
    let base_addr : bits(52) = changeSlice(loghead(52, base_addr), 0, 12, 0x000);
    let p_entry_addr : bits(52) = pml4_table_entry_addr(bits_of_int(lin_addr, 48), base_addr);
    let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
    let u_s_all : bits(1) = page_user_supervisor(entry);
    let r_w_all : bits(1) = page_read_write(entry);
    let x_d_all : bits(1) = page_execute_disable(entry);
    let (fault_flg, sailval) : (bool, {|0|}) = paging_entry_no_page_fault_p(0b11, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct { supervisor_mode_access_type = 0 });
    if fault_flg then x86_model_error("Page-Fault");
    let page_dir_ptr_table_base_addr : int = ash(unsigned(Mk_ia32e_pml4ebits(entry)[pdpt]), 12);
    let p_addr : bits(52) = bits_of_int(ia32e_la_to_pa_page_dir_ptr_table(bits_of_int(lin_addr, 48), bits_of_int(page_dir_ptr_table_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl), 52);
    if marking_view then {
        let entry : bits(64) = rm_low_64(unsigned(p_entry_addr));
        let accessed : bits(1) = accessed_bit(entry);
        let entry : ia32e_page_tablesbits = {
            if accessed == 0b0 then {
                set_accessed_bit(entry)
            } else {
                Mk_ia32e_page_tablesbits(entry)
            }
        };
        if accessed == 0b0 then {
            wm_low_64(p_entry_addr, entry.bits)
        };
        ()
    };
    p_addr
}

function ia32e_la_to_pa (lin_addr, r_w_x) = {
    assert(not_bool(app_view));
    let lin_addr : int = binary_logext(48, unsigned(loghead(48, lin_addr)));
    let cr0 : bits(32) = truncate(ctrs[0], 32);
    let cr4 : bits(22) = truncate(ctrs[4], 22);
    let cpl : bits(2) = Mk_segment_selectorbits(seg_visibles[1])[rpl];
    let ia32_efer : bits(12) = truncate(read_msr(0), 12);
    let wp : bits(1) = Mk_cr0bits(cr0)[wp];
    let smep : bits(1) = Mk_cr4bits(cr4)[smep];
    let smap : bits(1) = Mk_cr4bits(cr4)[smap];
    let ac : bits(1) = rflags[ac];
    let nxe : bits(1) = Mk_ia32_eferbits(ia32_efer)[nxe];
    let cr3 : bits(64) = ctrs[3];
    let pml4_table_base_addr : int = ash(unsigned(Mk_cr3bits(cr3)[pdb]), 12);
    ia32e_la_to_pa_pml4_table(bits_of_int(lin_addr, 48), bits_of_int(pml4_table_base_addr, 52), wp, smep, smap, ac, nxe, r_w_x, cpl)
}

/*! Top-level page translation function */
val la_to_pa : (sbits(48), string) -> bits(52)

function la_to_pa (lin_addr, r_w_x) = {
    assert(canonical_address_p(signed(lin_addr)) & not_bool(app_view));
    ia32e_la_to_pa(lin_addr, r_w_x)
}
