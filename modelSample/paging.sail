$include <prelude.sail>
$include <string.sail>
$include "paging_structures.sail"
$include "physical_memory.sail"
val page_fault_err_no : (bits(1), string, bits(2), bits(1), bits(1), bits(1), bits(1)) -> int
function page_fault_err_no (p_flag, r_w_x, cpl, rsvd, smep, pae, nxe) =
logior(if (p_flag) == (0b1)
then 1
else 0, if (r_w_x) == (":W")
then 2
else 0)

val page_fault_exception : (sbits(48), int) -> (bool, {|0|}) effect {escape}
function page_fault_exception (addr, err_no) =
let old_faults : bool = (fault()) in
let new_faults : ((string, (int, (sbits(48)))), (bool)) = (((":PAGE-FAULT", (err_no, (addr))), (old_faults))) in
{ throw(Emsg("A fault occurred.  Original ACL2 AST:
['!fault', 'new-faults', 'x86']"));
(true, 0)
}

val page_present : (bits(64)) -> bits(1)
function page_present (entry) =
ia32e_page_tablesbits_get_p(entry)

val page_size : (bits(64)) -> bits(1)
function page_size (entry) =
ia32e_page_tablesbits_get_ps(entry)

val page_read_write : (bits(64)) -> bits(1)
function page_read_write (entry) =
ia32e_page_tablesbits_get_r_w(entry)

val page_user_supervisor : (bits(64)) -> bits(1)
function page_user_supervisor (entry) =
ia32e_page_tablesbits_get_u_s(entry)

val page_execute_disable : (bits(64)) -> bits(1)
function page_execute_disable (entry) =
ia32e_page_tablesbits_get_xd(entry)

val accessed_bit : (bits(64)) -> bits(1)
function accessed_bit (entry) =
ia32e_page_tablesbits_get_a(entry)

val dirty_bit : (bits(64)) -> bits(1)
function dirty_bit (entry) =
ia32e_page_tablesbits_get_d(entry)

val set_accessed_bit : (bits(64)) -> bits(64)
function set_accessed_bit (entry) =
set_ia32e_page_tablesbits_get_a(0b1, entry)

val set_dirty_bit : (bits(64)) -> bits(64)
function set_dirty_bit (entry) =
set_ia32e_page_tablesbits_get_d(0b1, entry)

val page_table_entry_addr : (sbits(48), bits(52)) -> bits(52) effect {escape}
function page_table_entry_addr (lin_addr, base_addr) =
{ assert((unsigned_byte_p(52, unsigned(base_addr))) & ((loghead(12, unsigned(base_addr))) == (0)));
changeSlice(base_addr, 3, 9, slice(lin_addr, 12, 9))
}

val page_directory_entry_addr : (sbits(48), bits(52)) -> bits(52) effect {escape}
function page_directory_entry_addr (lin_addr, base_addr) =
{ assert((unsigned_byte_p(52, unsigned(base_addr))) & ((loghead(12, unsigned(base_addr))) == (0)));
changeSlice(base_addr, 3, 9, slice(lin_addr, 21, 9))
}

val page_dir_ptr_table_entry_addr : (sbits(48), bits(52)) -> bits(52) effect {escape}
function page_dir_ptr_table_entry_addr (lin_addr, base_addr) =
{ assert((unsigned_byte_p(52, unsigned(base_addr))) & ((loghead(12, unsigned(base_addr))) == (0)));
changeSlice(base_addr, 3, 9, slice(lin_addr, 30, 9))
}

val pml4_table_entry_addr : (sbits(48), bits(52)) -> bits(52) effect {escape}
function pml4_table_entry_addr (lin_addr, base_addr) =
{ assert((unsigned_byte_p(52, unsigned(base_addr))) & ((loghead(12, unsigned(base_addr))) == (0)));
changeSlice(base_addr, 3, 9, slice(lin_addr, 39, 9))
}

/*Determining access rights and detecting page faults*/
/*<p>Source for determining the access rights: Section 4.6 in 
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
 
 </ul>*/
struct struct_paging_entry_no_page_fault_p = {
	supervisor_mode_access_type : int
}
val paging_entry_no_page_fault_p : (bits(2), sbits(48), bits(64), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2), struct_paging_entry_no_page_fault_p) -> (bool, {|0|}) effect {escape}
function paging_entry_no_page_fault_p (structure_type, lin_addr, entry, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl, keywords) =
let entry : int = (loghead(64, unsigned(entry))) in
let page_present_var : bits(1) = (page_present(bits_of_int(entry, 64))) in
if (page_present_var) == (0b0)
then let err_no : int = (page_fault_err_no(page_present_var, r_w_x, cpl, 0b0, smep, 0b1, nxe)) in
page_fault_exception(lin_addr, err_no)
else let read_write : bits(1) = (page_read_write(bits_of_int(entry, 64))) in
let r_w_all : bits(1) = (logand(read_write, r_w_acc)) in
let user_supervisor : bits(1) = (page_user_supervisor(bits_of_int(entry, 64))) in
let u_s_all : bits(1) = (logand(user_supervisor, u_s_acc)) in
let execute_disable : bits(1) = (page_execute_disable(bits_of_int(entry, 64))) in
let x_d_all : bits(1) = (logand(execute_disable, x_d_acc)) in
let page_size_var : bits(1) = (page_size(bits_of_int(entry, 64))) in
let rsvd : {|0, 1|} = { (if (((structure_type) == (0b11)) & ((page_size_var) == (0b1))) | (((not_bool((structure_type) == (0b00))) & ((not_bool((structure_type) == (0b11))) & (((page_size_var) == (0b1)) & (if (structure_type) == (0b01)
then not_bool((get_slice_int(8, entry, 13)) == (0x00))
else not_bool((get_slice_int(17, entry, 13)) == (0b0_0000_0000_0000_0000)))))) | (((nxe) == (0b0)) & (not_bool((execute_disable) == (0b0)))))
then 1
else 0) } in
if (rsvd) == (1)
then let err_no : int = (page_fault_err_no(page_present_var, r_w_x, cpl, bits_of_int(rsvd, 1), smep, 0b1, nxe)) in
page_fault_exception(lin_addr, err_no)
else if (((r_w_x) == (":R")) & (if (unsigned(cpl)) < (3)
then if (u_s_all) == (0b0)
then false
else if (smap) == (0b0)
then false
else (((keywords).supervisor_mode_access_type) == (1)) | ((ac) == (0b0))
else (u_s_all) == (0b0))) | ((((r_w_x) == (":W")) & (if (unsigned(cpl)) < (3)
then if (u_s_all) == (0b0)
then ((wp) == (0b1)) & ((r_w_all) == (0b0))
else if (wp) == (0b0)
then if (smap) == (0b0)
then false
else (((keywords).supervisor_mode_access_type) == (1)) | ((ac) == (0b0))
else if (smap) == (0b0)
then (r_w_all) == (0b0)
else if (((keywords).supervisor_mode_access_type) == (0)) & ((ac) == (0b1))
then (r_w_all) == (0b0)
else true
else ((u_s_all) == (0b0)) | ((r_w_all) == (0b0)))) | (((r_w_x) == (":X")) & (if (unsigned(cpl)) < (3)
then if (u_s_all) == (0b0)
then if (nxe) == (0b0)
then false
else (x_d_all) == (0b1)
else if (smep) == (0b0)
then if (nxe) == (0b0)
then false
else (x_d_all) == (0b1)
else true
else ((u_s_all) == (0b0)) | (((nxe) == (0b1)) & ((x_d_all) == (0b1))))))
then let err_no : int = (page_fault_err_no(page_present_var, r_w_x, cpl, bits_of_int(rsvd, 1), smep, 0b1, nxe)) in
page_fault_exception(lin_addr, err_no)
else (false, 0)

val ia32e_la_to_pa_page_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> (option(string), int) effect {eamem, escape, rmem, rreg, wmv}
function ia32e_la_to_pa_page_table (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) =
{ assert(not_bool(app_view));
let lin_addr : int = (binary_logext(48, loghead(48, signed(lin_addr)))) in
let base_addr : int = (changeSlice(loghead(52, unsigned(base_addr)), 0, 12, 0)) in
let p_entry_addr : bits(52) = (page_table_entry_addr(bits_of_int(lin_addr, 48), bits_of_int(base_addr, 52))) in
let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let (fault_flg, sailval) : (bool, {|0|}) = (paging_entry_no_page_fault_p(0b00, bits_of_int(lin_addr, 48), entry, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl, struct {supervisor_mode_access_type = 0})) in
if fault_flg
then (Some("Page-Fault"), sailval)
else { if marking_view
then { let accessed : bits(1) = (accessed_bit(entry)) in
let dirty : bits(1) = (dirty_bit(entry)) in
let (updated?, entry) : (bool, bits(64)) = { (if (accessed) == (0b0)
then (true, set_accessed_bit(entry))
else (false, entry)) } in
let (updated?, entry) : (bool, bits(64)) = { (if ((dirty) == (0b0)) & ((r_w_x) == (":W"))
then (true, set_dirty_bit(entry))
else (updated?, entry)) } in
if updated?
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), changeSlice(ash(unsigned(ia32e_pte_4k_pagebits_get_page(entry)), 12), 0, 12, unsigned(get_slice_int(12, lin_addr, 0))))
}
}

val ia32e_la_to_pa_page_directory : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> (option(string), int) effect {eamem, escape, rmem, rreg, wmv}
function ia32e_la_to_pa_page_directory (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) =
{ assert(not_bool(app_view));
let lin_addr : int = (binary_logext(48, loghead(48, signed(lin_addr)))) in
let base_addr : int = (changeSlice(loghead(52, unsigned(base_addr)), 0, 12, 0)) in
let p_entry_addr : bits(52) = (page_directory_entry_addr(bits_of_int(lin_addr, 48), bits_of_int(base_addr, 52))) in
let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let u_s_all : bits(1) = (logand(u_s_acc, page_user_supervisor(entry))) in
let r_w_all : bits(1) = (logand(r_w_acc, page_read_write(entry))) in
let x_d_all : bits(1) = (logand(x_d_acc, page_execute_disable(entry))) in
let (fault_flg, sailval) : (bool, {|0|}) = (paging_entry_no_page_fault_p(0b01, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct {supervisor_mode_access_type = 0})) in
if fault_flg
then (Some("Page-Fault"), sailval)
else if (page_size(entry)) == (0b1)
then { if marking_view
then { let accessed : bits(1) = (accessed_bit(entry)) in
let dirty : bits(1) = (dirty_bit(entry)) in
let (updated?, entry) : (bool, bits(64)) = { (if (accessed) == (0b0)
then (true, set_accessed_bit(entry))
else (false, entry)) } in
let (updated?, entry) : (bool, bits(64)) = { (if ((dirty) == (0b0)) & ((r_w_x) == (":W"))
then (true, set_dirty_bit(entry))
else (updated?, entry)) } in
if updated?
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), changeSlice(ash(unsigned(ia32e_pde_2mb_pagebits_get_page(entry)), 21), 0, 21, unsigned(get_slice_int(21, lin_addr, 0))))
}
else let (elem0, elem1) : (option(string), bits(52)) = { (let page_table_base_addr : int = (ash(unsigned(ia32e_pde_pg_tablebits_get_pt(entry)), 12)) in
let (flag, p_addr) : (option(string), int) = (ia32e_la_to_pa_page_table(bits_of_int(lin_addr, 48), bits_of_int(page_table_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl)) in
let p_addr : bits(52) = (bits_of_int(p_addr, 52)) in
if is_some(flag)
then (flag, 0x0_0000_0000_0000)
else { if marking_view
then { let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let accessed : bits(1) = (accessed_bit(entry)) in
let entry : bits(64) = { (if (accessed) == (0b0)
then set_accessed_bit(entry)
else entry) } in
if (accessed) == (0b0)
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), p_addr)
}) } in
(elem0, unsigned(elem1))
}

val ia32e_la_to_pa_page_dir_ptr_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> (option(string), int) effect {eamem, escape, rmem, rreg, wmv}
function ia32e_la_to_pa_page_dir_ptr_table (lin_addr, base_addr, u_s_acc, r_w_acc, x_d_acc, wp, smep, smap, ac, nxe, r_w_x, cpl) =
{ assert(not_bool(app_view));
let lin_addr : int = (binary_logext(48, loghead(48, signed(lin_addr)))) in
let base_addr : int = (changeSlice(loghead(52, unsigned(base_addr)), 0, 12, 0)) in
let p_entry_addr : bits(52) = (page_dir_ptr_table_entry_addr(bits_of_int(lin_addr, 48), bits_of_int(base_addr, 52))) in
let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let u_s_all : bits(1) = (logand(u_s_acc, page_user_supervisor(entry))) in
let r_w_all : bits(1) = (logand(r_w_acc, page_read_write(entry))) in
let x_d_all : bits(1) = (logand(x_d_acc, page_execute_disable(entry))) in
let (fault_flg, sailval) : (bool, {|0|}) = (paging_entry_no_page_fault_p(0b10, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct {supervisor_mode_access_type = 0})) in
if fault_flg
then (Some("Page-Fault"), sailval)
else if (page_size(entry)) == (0b1)
then { if marking_view
then { let accessed : bits(1) = (accessed_bit(entry)) in
let dirty : bits(1) = (dirty_bit(entry)) in
let (updated?, entry) : (bool, bits(64)) = { (if (accessed) == (0b0)
then (true, set_accessed_bit(entry))
else (false, entry)) } in
let (updated?, entry) : (bool, bits(64)) = { (if ((dirty) == (0b0)) & ((r_w_x) == (":W"))
then (true, set_dirty_bit(entry))
else (updated?, entry)) } in
if updated?
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), changeSlice(ash(unsigned(ia32e_pdpte_1gb_pagebits_get_page(entry)), 30), 0, 30, unsigned(get_slice_int(30, lin_addr, 0))))
}
else let (elem0, elem1) : (option(string), bits(52)) = { (let page_directory_base_addr : int = (ash(unsigned(ia32e_pdpte_pg_dirbits_get_pd(entry)), 12)) in
let (flag, p_addr) : (option(string), int) = (ia32e_la_to_pa_page_directory(bits_of_int(lin_addr, 48), bits_of_int(page_directory_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl)) in
let p_addr : bits(52) = (bits_of_int(p_addr, 52)) in
if is_some(flag)
then (flag, 0x0_0000_0000_0000)
else { if marking_view
then { let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let accessed : bits(1) = (accessed_bit(entry)) in
let entry : bits(64) = { (if (accessed) == (0b0)
then set_accessed_bit(entry)
else entry) } in
if (accessed) == (0b0)
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), p_addr)
}) } in
(elem0, unsigned(elem1))
}

val ia32e_la_to_pa_pml4_table : (sbits(48), bits(52), bits(1), bits(1), bits(1), bits(1), bits(1), string, bits(2)) -> (option(string), bits(52)) effect {eamem, escape, rmem, rreg, wmv}
function ia32e_la_to_pa_pml4_table (lin_addr, base_addr, wp, smep, smap, ac, nxe, r_w_x, cpl) =
{ assert(not_bool(app_view));
let lin_addr : int = (binary_logext(48, loghead(48, signed(lin_addr)))) in
let base_addr : int = (changeSlice(loghead(52, unsigned(base_addr)), 0, 12, 0)) in
let p_entry_addr : bits(52) = (pml4_table_entry_addr(bits_of_int(lin_addr, 48), bits_of_int(base_addr, 52))) in
let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let u_s_all : bits(1) = (page_user_supervisor(entry)) in
let r_w_all : bits(1) = (page_read_write(entry)) in
let x_d_all : bits(1) = (page_execute_disable(entry)) in
let (fault_flg, sailval) : (bool, {|0|}) = (paging_entry_no_page_fault_p(0b11, bits_of_int(lin_addr, 48), entry, u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl, struct {supervisor_mode_access_type = 0})) in
if fault_flg
then (Some("Page-Fault"), bits_of_int(sailval, 52))
else let page_dir_ptr_table_base_addr : int = (ash(unsigned(ia32e_pml4ebits_get_pdpt(entry)), 12)) in
let (flag, p_addr) : (option(string), int) = (ia32e_la_to_pa_page_dir_ptr_table(bits_of_int(lin_addr, 48), bits_of_int(page_dir_ptr_table_base_addr, 52), u_s_all, r_w_all, x_d_all, wp, smep, smap, ac, nxe, r_w_x, cpl)) in
let p_addr : bits(52) = (bits_of_int(p_addr, 52)) in
if is_some(flag)
then (flag, 0x0_0000_0000_0000)
else { if marking_view
then { let entry : bits(64) = (rm_low_64(unsigned(p_entry_addr))) in
let accessed : bits(1) = (accessed_bit(entry)) in
let entry : bits(64) = { (if (accessed) == (0b0)
then set_accessed_bit(entry)
else entry) } in
if (accessed) == (0b0)
then { wm_low_64(p_entry_addr, entry)
}
else ()
}
else ();
(None() : option(string), p_addr)
}
}

val ia32e_la_to_pa : (sbits(48), string) -> (option(string), bits(52)) effect {eamem, escape, rmem, rreg, wmv}
function ia32e_la_to_pa (lin_addr, r_w_x) =
{ assert(not_bool(app_view));
let lin_addr : int = (binary_logext(48, loghead(48, signed(lin_addr)))) in
let cr0 : bits(32) = (truncate(ctrs[0], 32)) in
let cr4 : bits(22) = (truncate(ctrs[4], 22)) in
let cpl : bits(2) = { ({ ();
segment_selectorbits_get_rpl(seg_visibles[1])
}) } in
let ia32_efer : bits(12) = (truncate(msrs[0], 12)) in
let wp : bits(1) = (cr0bits_get_wp(cr0)) in
let smep : bits(1) = (cr4bits_get_smep(cr4)) in
let smap : bits(1) = (cr4bits_get_smap(cr4)) in
let ac : bits(1) = (rflagsbits_get_ac(rflags)) in
let nxe : bits(1) = (ia32_eferbits_get_nxe(ia32_efer)) in
let cr3 : bits(64) = (ctrs[3]) in
let pml4_table_base_addr : int = (ash(unsigned(cr3bits_get_pdb(cr3)), 12)) in
ia32e_la_to_pa_pml4_table(bits_of_int(lin_addr, 48), bits_of_int(pml4_table_base_addr, 52), wp, smep, smap, ac, nxe, r_w_x, cpl)
}

/*Top-level page translation function*/
val la_to_pa : (sbits(48), string) -> (option(string), bits(52)) effect {eamem, escape, rmem, rreg, wmv}
function la_to_pa (lin_addr, r_w_x) =
{ assert((canonical_address_p(signed(lin_addr))) & (not_bool(app_view)));
ia32e_la_to_pa(lin_addr, r_w_x)
}

