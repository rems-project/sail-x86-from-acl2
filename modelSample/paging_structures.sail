$include <prelude.sail>
$include <string.sail>
val ia32e_page_tablesbits_get_p : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_page_tablesbits_get_r_w : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_page_tablesbits_get_u_s : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_page_tablesbits_get_pwt : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_page_tablesbits_get_pcd : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_page_tablesbits_get_a : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_page_tablesbits_get_d : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_page_tablesbits_get_ps : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_page_tablesbits_get_res1 : (bits(64)) -> bits(4)
function ia32e_page_tablesbits_get_res1 (inputbits) =
genericBitstructAccessor(4, inputbits, 8)

val ia32e_page_tablesbits_get_reference_addr : (bits(64)) -> bits(40)
function ia32e_page_tablesbits_get_reference_addr (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val ia32e_page_tablesbits_get_res2 : (bits(64)) -> bits(11)
function ia32e_page_tablesbits_get_res2 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_page_tablesbits_get_xd : (bits(64)) -> bits(1)
function ia32e_page_tablesbits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_page_tablesbits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_page_tablesbits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_page_tablesbits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_page_tablesbits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_page_tablesbits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_page_tablesbits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_page_tablesbits_get_d : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_page_tablesbits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_page_tablesbits_get_res1 : (bits(4), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 8, inputbits)

val set_ia32e_page_tablesbits_get_reference_addr : (bits(40), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_reference_addr (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_ia32e_page_tablesbits_get_res2 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_page_tablesbits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_page_tablesbits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_page_tablesbits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(4)), option(bits(40)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_page_tablesbits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_d, input_ps, input_res1, input_reference_addr, input_res2, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_page_tablesbits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_page_tablesbits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_page_tablesbits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_page_tablesbits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_page_tablesbits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_page_tablesbits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_d : bits(64) = { (match input_d {
Some(input_d) => set_ia32e_page_tablesbits_get_d(input_d, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_page_tablesbits_get_ps(input_ps, output_d),
_ => output_d
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_page_tablesbits_get_res1(input_res1, output_ps),
_ => output_ps
}) } in
let output_reference_addr : bits(64) = { (match input_reference_addr {
Some(input_reference_addr) => set_ia32e_page_tablesbits_get_reference_addr(input_reference_addr, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_page_tablesbits_get_res2(input_res2, output_reference_addr),
_ => output_reference_addr
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_page_tablesbits_get_xd(input_xd, output_res2),
_ => output_res2
}) } in
output_xd

val ia32e_pml4ebits_get_p : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pml4ebits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pml4ebits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pml4ebits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pml4ebits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pml4ebits_get_a : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pml4ebits_get_res1 : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pml4ebits_get_ps : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pml4ebits_get_res2 : (bits(64)) -> bits(4)
function ia32e_pml4ebits_get_res2 (inputbits) =
genericBitstructAccessor(4, inputbits, 8)

val ia32e_pml4ebits_get_pdpt : (bits(64)) -> bits(40)
function ia32e_pml4ebits_get_pdpt (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val ia32e_pml4ebits_get_res3 : (bits(64)) -> bits(11)
function ia32e_pml4ebits_get_res3 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pml4ebits_get_xd : (bits(64)) -> bits(1)
function ia32e_pml4ebits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pml4ebits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pml4ebits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pml4ebits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pml4ebits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pml4ebits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pml4ebits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pml4ebits_get_res1 : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pml4ebits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pml4ebits_get_res2 : (bits(4), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 8, inputbits)

val set_ia32e_pml4ebits_get_pdpt : (bits(40), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_pdpt (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_ia32e_pml4ebits_get_res3 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pml4ebits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pml4ebits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pml4ebits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(4)), option(bits(40)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pml4ebits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_res1, input_ps, input_res2, input_pdpt, input_res3, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pml4ebits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pml4ebits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pml4ebits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pml4ebits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pml4ebits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pml4ebits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pml4ebits_get_res1(input_res1, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_pml4ebits_get_ps(input_ps, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pml4ebits_get_res2(input_res2, output_ps),
_ => output_ps
}) } in
let output_pdpt : bits(64) = { (match input_pdpt {
Some(input_pdpt) => set_ia32e_pml4ebits_get_pdpt(input_pdpt, output_res2),
_ => output_res2
}) } in
let output_res3 : bits(64) = { (match input_res3 {
Some(input_res3) => set_ia32e_pml4ebits_get_res3(input_res3, output_pdpt),
_ => output_pdpt
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pml4ebits_get_xd(input_xd, output_res3),
_ => output_res3
}) } in
output_xd

val ia32e_pdpte_1gb_pagebits_get_p : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pdpte_1gb_pagebits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pdpte_1gb_pagebits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pdpte_1gb_pagebits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pdpte_1gb_pagebits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pdpte_1gb_pagebits_get_a : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pdpte_1gb_pagebits_get_d : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pdpte_1gb_pagebits_get_ps : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pdpte_1gb_pagebits_get_g : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val ia32e_pdpte_1gb_pagebits_get_res1 : (bits(64)) -> bits(3)
function ia32e_pdpte_1gb_pagebits_get_res1 (inputbits) =
genericBitstructAccessor(3, inputbits, 9)

val ia32e_pdpte_1gb_pagebits_get_pat : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_pat (inputbits) =
genericBitstructAccessor(1, inputbits, 12)

val ia32e_pdpte_1gb_pagebits_get_res2 : (bits(64)) -> bits(17)
function ia32e_pdpte_1gb_pagebits_get_res2 (inputbits) =
genericBitstructAccessor(17, inputbits, 13)

val ia32e_pdpte_1gb_pagebits_get_page : (bits(64)) -> bits(22)
function ia32e_pdpte_1gb_pagebits_get_page (inputbits) =
genericBitstructAccessor(22, inputbits, 30)

val ia32e_pdpte_1gb_pagebits_get_res3 : (bits(64)) -> bits(11)
function ia32e_pdpte_1gb_pagebits_get_res3 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pdpte_1gb_pagebits_get_xd : (bits(64)) -> bits(1)
function ia32e_pdpte_1gb_pagebits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pdpte_1gb_pagebits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_d : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_g : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_res1 : (bits(3), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 9, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_pat : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_pat (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 12, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_res2 : (bits(17), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(17, splicebits, 13, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_page : (bits(22), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_page (splicebits, inputbits) =
genericBitstructUpdater(22, splicebits, 30, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_res3 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pdpte_1gb_pagebits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_1gb_pagebits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pdpte_1gb_pagebits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(3)), option(bits(1)), option(bits(17)), option(bits(22)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pdpte_1gb_pagebits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_d, input_ps, input_g, input_res1, input_pat, input_res2, input_page, input_res3, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pdpte_1gb_pagebits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pdpte_1gb_pagebits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pdpte_1gb_pagebits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pdpte_1gb_pagebits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pdpte_1gb_pagebits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pdpte_1gb_pagebits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_d : bits(64) = { (match input_d {
Some(input_d) => set_ia32e_pdpte_1gb_pagebits_get_d(input_d, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_pdpte_1gb_pagebits_get_ps(input_ps, output_d),
_ => output_d
}) } in
let output_g : bits(64) = { (match input_g {
Some(input_g) => set_ia32e_pdpte_1gb_pagebits_get_g(input_g, output_ps),
_ => output_ps
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pdpte_1gb_pagebits_get_res1(input_res1, output_g),
_ => output_g
}) } in
let output_pat : bits(64) = { (match input_pat {
Some(input_pat) => set_ia32e_pdpte_1gb_pagebits_get_pat(input_pat, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pdpte_1gb_pagebits_get_res2(input_res2, output_pat),
_ => output_pat
}) } in
let output_page : bits(64) = { (match input_page {
Some(input_page) => set_ia32e_pdpte_1gb_pagebits_get_page(input_page, output_res2),
_ => output_res2
}) } in
let output_res3 : bits(64) = { (match input_res3 {
Some(input_res3) => set_ia32e_pdpte_1gb_pagebits_get_res3(input_res3, output_page),
_ => output_page
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pdpte_1gb_pagebits_get_xd(input_xd, output_res3),
_ => output_res3
}) } in
output_xd

val ia32e_pdpte_pg_dirbits_get_p : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pdpte_pg_dirbits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pdpte_pg_dirbits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pdpte_pg_dirbits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pdpte_pg_dirbits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pdpte_pg_dirbits_get_a : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pdpte_pg_dirbits_get_res1 : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pdpte_pg_dirbits_get_ps : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pdpte_pg_dirbits_get_res2 : (bits(64)) -> bits(4)
function ia32e_pdpte_pg_dirbits_get_res2 (inputbits) =
genericBitstructAccessor(4, inputbits, 8)

val ia32e_pdpte_pg_dirbits_get_pd : (bits(64)) -> bits(40)
function ia32e_pdpte_pg_dirbits_get_pd (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val ia32e_pdpte_pg_dirbits_get_res3 : (bits(64)) -> bits(11)
function ia32e_pdpte_pg_dirbits_get_res3 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pdpte_pg_dirbits_get_xd : (bits(64)) -> bits(1)
function ia32e_pdpte_pg_dirbits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pdpte_pg_dirbits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_res1 : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_res2 : (bits(4), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 8, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_pd : (bits(40), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_pd (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_res3 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pdpte_pg_dirbits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pdpte_pg_dirbits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pdpte_pg_dirbits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(4)), option(bits(40)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pdpte_pg_dirbits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_res1, input_ps, input_res2, input_pd, input_res3, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pdpte_pg_dirbits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pdpte_pg_dirbits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pdpte_pg_dirbits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pdpte_pg_dirbits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pdpte_pg_dirbits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pdpte_pg_dirbits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pdpte_pg_dirbits_get_res1(input_res1, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_pdpte_pg_dirbits_get_ps(input_ps, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pdpte_pg_dirbits_get_res2(input_res2, output_ps),
_ => output_ps
}) } in
let output_pd : bits(64) = { (match input_pd {
Some(input_pd) => set_ia32e_pdpte_pg_dirbits_get_pd(input_pd, output_res2),
_ => output_res2
}) } in
let output_res3 : bits(64) = { (match input_res3 {
Some(input_res3) => set_ia32e_pdpte_pg_dirbits_get_res3(input_res3, output_pd),
_ => output_pd
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pdpte_pg_dirbits_get_xd(input_xd, output_res3),
_ => output_res3
}) } in
output_xd

val ia32e_pde_2mb_pagebits_get_p : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pde_2mb_pagebits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pde_2mb_pagebits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pde_2mb_pagebits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pde_2mb_pagebits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pde_2mb_pagebits_get_a : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pde_2mb_pagebits_get_d : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pde_2mb_pagebits_get_ps : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pde_2mb_pagebits_get_g : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val ia32e_pde_2mb_pagebits_get_res1 : (bits(64)) -> bits(3)
function ia32e_pde_2mb_pagebits_get_res1 (inputbits) =
genericBitstructAccessor(3, inputbits, 9)

val ia32e_pde_2mb_pagebits_get_pat : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_pat (inputbits) =
genericBitstructAccessor(1, inputbits, 12)

val ia32e_pde_2mb_pagebits_get_res2 : (bits(64)) -> bits(8)
function ia32e_pde_2mb_pagebits_get_res2 (inputbits) =
genericBitstructAccessor(8, inputbits, 13)

val ia32e_pde_2mb_pagebits_get_page : (bits(64)) -> bits(31)
function ia32e_pde_2mb_pagebits_get_page (inputbits) =
genericBitstructAccessor(31, inputbits, 21)

val ia32e_pde_2mb_pagebits_get_res3 : (bits(64)) -> bits(11)
function ia32e_pde_2mb_pagebits_get_res3 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pde_2mb_pagebits_get_xd : (bits(64)) -> bits(1)
function ia32e_pde_2mb_pagebits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pde_2mb_pagebits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pde_2mb_pagebits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pde_2mb_pagebits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pde_2mb_pagebits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pde_2mb_pagebits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pde_2mb_pagebits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pde_2mb_pagebits_get_d : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pde_2mb_pagebits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pde_2mb_pagebits_get_g : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_ia32e_pde_2mb_pagebits_get_res1 : (bits(3), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 9, inputbits)

val set_ia32e_pde_2mb_pagebits_get_pat : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_pat (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 12, inputbits)

val set_ia32e_pde_2mb_pagebits_get_res2 : (bits(8), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 13, inputbits)

val set_ia32e_pde_2mb_pagebits_get_page : (bits(31), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_page (splicebits, inputbits) =
genericBitstructUpdater(31, splicebits, 21, inputbits)

val set_ia32e_pde_2mb_pagebits_get_res3 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pde_2mb_pagebits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_2mb_pagebits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pde_2mb_pagebits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(3)), option(bits(1)), option(bits(8)), option(bits(31)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pde_2mb_pagebits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_d, input_ps, input_g, input_res1, input_pat, input_res2, input_page, input_res3, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pde_2mb_pagebits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pde_2mb_pagebits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pde_2mb_pagebits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pde_2mb_pagebits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pde_2mb_pagebits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pde_2mb_pagebits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_d : bits(64) = { (match input_d {
Some(input_d) => set_ia32e_pde_2mb_pagebits_get_d(input_d, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_pde_2mb_pagebits_get_ps(input_ps, output_d),
_ => output_d
}) } in
let output_g : bits(64) = { (match input_g {
Some(input_g) => set_ia32e_pde_2mb_pagebits_get_g(input_g, output_ps),
_ => output_ps
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pde_2mb_pagebits_get_res1(input_res1, output_g),
_ => output_g
}) } in
let output_pat : bits(64) = { (match input_pat {
Some(input_pat) => set_ia32e_pde_2mb_pagebits_get_pat(input_pat, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pde_2mb_pagebits_get_res2(input_res2, output_pat),
_ => output_pat
}) } in
let output_page : bits(64) = { (match input_page {
Some(input_page) => set_ia32e_pde_2mb_pagebits_get_page(input_page, output_res2),
_ => output_res2
}) } in
let output_res3 : bits(64) = { (match input_res3 {
Some(input_res3) => set_ia32e_pde_2mb_pagebits_get_res3(input_res3, output_page),
_ => output_page
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pde_2mb_pagebits_get_xd(input_xd, output_res3),
_ => output_res3
}) } in
output_xd

val ia32e_pde_pg_tablebits_get_p : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pde_pg_tablebits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pde_pg_tablebits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pde_pg_tablebits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pde_pg_tablebits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pde_pg_tablebits_get_a : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pde_pg_tablebits_get_res1 : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pde_pg_tablebits_get_ps : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_ps (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pde_pg_tablebits_get_res2 : (bits(64)) -> bits(4)
function ia32e_pde_pg_tablebits_get_res2 (inputbits) =
genericBitstructAccessor(4, inputbits, 8)

val ia32e_pde_pg_tablebits_get_pt : (bits(64)) -> bits(40)
function ia32e_pde_pg_tablebits_get_pt (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val ia32e_pde_pg_tablebits_get_res3 : (bits(64)) -> bits(11)
function ia32e_pde_pg_tablebits_get_res3 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pde_pg_tablebits_get_xd : (bits(64)) -> bits(1)
function ia32e_pde_pg_tablebits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pde_pg_tablebits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pde_pg_tablebits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pde_pg_tablebits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pde_pg_tablebits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pde_pg_tablebits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pde_pg_tablebits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pde_pg_tablebits_get_res1 : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pde_pg_tablebits_get_ps : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_ps (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pde_pg_tablebits_get_res2 : (bits(4), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 8, inputbits)

val set_ia32e_pde_pg_tablebits_get_pt : (bits(40), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_pt (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_ia32e_pde_pg_tablebits_get_res3 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pde_pg_tablebits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pde_pg_tablebits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pde_pg_tablebits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(4)), option(bits(40)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pde_pg_tablebits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_res1, input_ps, input_res2, input_pt, input_res3, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pde_pg_tablebits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pde_pg_tablebits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pde_pg_tablebits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pde_pg_tablebits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pde_pg_tablebits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pde_pg_tablebits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pde_pg_tablebits_get_res1(input_res1, output_a),
_ => output_a
}) } in
let output_ps : bits(64) = { (match input_ps {
Some(input_ps) => set_ia32e_pde_pg_tablebits_get_ps(input_ps, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pde_pg_tablebits_get_res2(input_res2, output_ps),
_ => output_ps
}) } in
let output_pt : bits(64) = { (match input_pt {
Some(input_pt) => set_ia32e_pde_pg_tablebits_get_pt(input_pt, output_res2),
_ => output_res2
}) } in
let output_res3 : bits(64) = { (match input_res3 {
Some(input_res3) => set_ia32e_pde_pg_tablebits_get_res3(input_res3, output_pt),
_ => output_pt
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pde_pg_tablebits_get_xd(input_xd, output_res3),
_ => output_res3
}) } in
output_xd

val ia32e_pte_4k_pagebits_get_p : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32e_pte_4k_pagebits_get_r_w : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_r_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val ia32e_pte_4k_pagebits_get_u_s : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_u_s (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val ia32e_pte_4k_pagebits_get_pwt : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val ia32e_pte_4k_pagebits_get_pcd : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val ia32e_pte_4k_pagebits_get_a : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val ia32e_pte_4k_pagebits_get_d : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val ia32e_pte_4k_pagebits_get_pat : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_pat (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val ia32e_pte_4k_pagebits_get_g : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val ia32e_pte_4k_pagebits_get_res1 : (bits(64)) -> bits(3)
function ia32e_pte_4k_pagebits_get_res1 (inputbits) =
genericBitstructAccessor(3, inputbits, 9)

val ia32e_pte_4k_pagebits_get_page : (bits(64)) -> bits(40)
function ia32e_pte_4k_pagebits_get_page (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val ia32e_pte_4k_pagebits_get_res2 : (bits(64)) -> bits(11)
function ia32e_pte_4k_pagebits_get_res2 (inputbits) =
genericBitstructAccessor(11, inputbits, 52)

val ia32e_pte_4k_pagebits_get_xd : (bits(64)) -> bits(1)
function ia32e_pte_4k_pagebits_get_xd (inputbits) =
genericBitstructAccessor(1, inputbits, 63)

val set_ia32e_pte_4k_pagebits_get_p : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32e_pte_4k_pagebits_get_r_w : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_r_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_ia32e_pte_4k_pagebits_get_u_s : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_u_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_ia32e_pte_4k_pagebits_get_pwt : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_ia32e_pte_4k_pagebits_get_pcd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_ia32e_pte_4k_pagebits_get_a : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_ia32e_pte_4k_pagebits_get_d : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_ia32e_pte_4k_pagebits_get_pat : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_pat (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_ia32e_pte_4k_pagebits_get_g : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_ia32e_pte_4k_pagebits_get_res1 : (bits(3), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 9, inputbits)

val set_ia32e_pte_4k_pagebits_get_page : (bits(40), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_page (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_ia32e_pte_4k_pagebits_get_res2 : (bits(11), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(11, splicebits, 52, inputbits)

val set_ia32e_pte_4k_pagebits_get_xd : (bits(1), bits(64)) -> bits(64)
function set_ia32e_pte_4k_pagebits_get_xd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 63, inputbits)

val change_ia32e_pte_4k_pagebits : (bits(64), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(1)), option(bits(3)), option(bits(40)), option(bits(11)), option(bits(1))) -> bits(64)
function change_ia32e_pte_4k_pagebits (input_bits, input_p, input_r_w, input_u_s, input_pwt, input_pcd, input_a, input_d, input_pat, input_g, input_res1, input_page, input_res2, input_xd) =
let output_p : bits(64) = { (match input_p {
Some(input_p) => set_ia32e_pte_4k_pagebits_get_p(input_p, input_bits),
_ => input_bits
}) } in
let output_r_w : bits(64) = { (match input_r_w {
Some(input_r_w) => set_ia32e_pte_4k_pagebits_get_r_w(input_r_w, output_p),
_ => output_p
}) } in
let output_u_s : bits(64) = { (match input_u_s {
Some(input_u_s) => set_ia32e_pte_4k_pagebits_get_u_s(input_u_s, output_r_w),
_ => output_r_w
}) } in
let output_pwt : bits(64) = { (match input_pwt {
Some(input_pwt) => set_ia32e_pte_4k_pagebits_get_pwt(input_pwt, output_u_s),
_ => output_u_s
}) } in
let output_pcd : bits(64) = { (match input_pcd {
Some(input_pcd) => set_ia32e_pte_4k_pagebits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) } in
let output_a : bits(64) = { (match input_a {
Some(input_a) => set_ia32e_pte_4k_pagebits_get_a(input_a, output_pcd),
_ => output_pcd
}) } in
let output_d : bits(64) = { (match input_d {
Some(input_d) => set_ia32e_pte_4k_pagebits_get_d(input_d, output_a),
_ => output_a
}) } in
let output_pat : bits(64) = { (match input_pat {
Some(input_pat) => set_ia32e_pte_4k_pagebits_get_pat(input_pat, output_d),
_ => output_d
}) } in
let output_g : bits(64) = { (match input_g {
Some(input_g) => set_ia32e_pte_4k_pagebits_get_g(input_g, output_pat),
_ => output_pat
}) } in
let output_res1 : bits(64) = { (match input_res1 {
Some(input_res1) => set_ia32e_pte_4k_pagebits_get_res1(input_res1, output_g),
_ => output_g
}) } in
let output_page : bits(64) = { (match input_page {
Some(input_page) => set_ia32e_pte_4k_pagebits_get_page(input_page, output_res1),
_ => output_res1
}) } in
let output_res2 : bits(64) = { (match input_res2 {
Some(input_res2) => set_ia32e_pte_4k_pagebits_get_res2(input_res2, output_page),
_ => output_page
}) } in
let output_xd : bits(64) = { (match input_xd {
Some(input_xd) => set_ia32e_pte_4k_pagebits_get_xd(input_xd, output_res2),
_ => output_res2
}) } in
output_xd

