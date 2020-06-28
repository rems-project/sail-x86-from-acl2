$include <prelude.sail>
$include <string.sail>
val hidden_segment_registerbits_get_base_addr : (int) -> int
function hidden_segment_registerbits_get_base_addr (inputbits) =
genericBitstructAccessor(64, inputbits, 0)

val hidden_segment_registerbits_get_limit : (int) -> int
function hidden_segment_registerbits_get_limit (inputbits) =
genericBitstructAccessor(32, inputbits, 64)

val hidden_segment_registerbits_get_attr : (int) -> int
function hidden_segment_registerbits_get_attr (inputbits) =
genericBitstructAccessor(16, inputbits, 96)

val set_hidden_segment_registerbits_get_base_addr : (int, int) -> int
function set_hidden_segment_registerbits_get_base_addr (splicebits, inputbits) =
genericBitstructUpdater(64, splicebits, 0, inputbits)

val set_hidden_segment_registerbits_get_limit : (int, int) -> int
function set_hidden_segment_registerbits_get_limit (splicebits, inputbits) =
genericBitstructUpdater(32, splicebits, 64, inputbits)

val set_hidden_segment_registerbits_get_attr : (int, int) -> int
function set_hidden_segment_registerbits_get_attr (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 96, inputbits)

val change_hidden_segment_registerbits : (int, option(int), option(int), option(int)) -> int
function change_hidden_segment_registerbits (input_bits, input_base_addr, input_limit, input_attr) =
let output_base_addr = ((match input_base_addr {
Some(input_base_addr) => set_hidden_segment_registerbits_get_base_addr(input_base_addr, input_bits),
_ => input_bits
}) : int) : int in
let output_limit = ((match input_limit {
Some(input_limit) => set_hidden_segment_registerbits_get_limit(input_limit, output_base_addr),
_ => output_base_addr
}) : int) : int in
let output_attr = ((match input_attr {
Some(input_attr) => set_hidden_segment_registerbits_get_attr(input_attr, output_limit),
_ => output_limit
}) : int) : int in
output_attr

val segment_selectorbits_get_rpl : (int) -> int
function segment_selectorbits_get_rpl (inputbits) =
genericBitstructAccessor(2, inputbits, 0)

val segment_selectorbits_get_ti : (int) -> int
function segment_selectorbits_get_ti (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val segment_selectorbits_get_index : (int) -> int
function segment_selectorbits_get_index (inputbits) =
genericBitstructAccessor(13, inputbits, 3)

val set_segment_selectorbits_get_rpl : (int, int) -> int
function set_segment_selectorbits_get_rpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 0, inputbits)

val set_segment_selectorbits_get_ti : (int, int) -> int
function set_segment_selectorbits_get_ti (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_segment_selectorbits_get_index : (int, int) -> int
function set_segment_selectorbits_get_index (splicebits, inputbits) =
genericBitstructUpdater(13, splicebits, 3, inputbits)

val change_segment_selectorbits : (int, option(int), option(int), option(int)) -> int
function change_segment_selectorbits (input_bits, input_rpl, input_ti, input_index) =
let output_rpl = ((match input_rpl {
Some(input_rpl) => set_segment_selectorbits_get_rpl(input_rpl, input_bits),
_ => input_bits
}) : int) : int in
let output_ti = ((match input_ti {
Some(input_ti) => set_segment_selectorbits_get_ti(input_ti, output_rpl),
_ => output_rpl
}) : int) : int in
let output_index = ((match input_index {
Some(input_index) => set_segment_selectorbits_get_index(input_index, output_ti),
_ => output_ti
}) : int) : int in
output_index

val interrupt_trap_gate_descriptorbits_get_offset15_0 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_offset15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 0)

val interrupt_trap_gate_descriptorbits_get_selector : (int) -> int
function interrupt_trap_gate_descriptorbits_get_selector (inputbits) =
genericBitstructAccessor(16, inputbits, 16)

val interrupt_trap_gate_descriptorbits_get_ist : (int) -> int
function interrupt_trap_gate_descriptorbits_get_ist (inputbits) =
genericBitstructAccessor(3, inputbits, 32)

val interrupt_trap_gate_descriptorbits_get_res1 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_res1 (inputbits) =
genericBitstructAccessor(5, inputbits, 35)

val interrupt_trap_gate_descriptorbits_get_type : (int) -> int
function interrupt_trap_gate_descriptorbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 40)

val interrupt_trap_gate_descriptorbits_get_s : (int) -> int
function interrupt_trap_gate_descriptorbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 44)

val interrupt_trap_gate_descriptorbits_get_dpl : (int) -> int
function interrupt_trap_gate_descriptorbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 45)

val interrupt_trap_gate_descriptorbits_get_p : (int) -> int
function interrupt_trap_gate_descriptorbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 47)

val interrupt_trap_gate_descriptorbits_get_offset31_16 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_offset31_16 (inputbits) =
genericBitstructAccessor(16, inputbits, 48)

val interrupt_trap_gate_descriptorbits_get_offset63_32 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_offset63_32 (inputbits) =
genericBitstructAccessor(32, inputbits, 64)

val interrupt_trap_gate_descriptorbits_get_res2 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_res2 (inputbits) =
genericBitstructAccessor(8, inputbits, 96)

val interrupt_trap_gate_descriptorbits_get_all_zeros? : (int) -> int
function interrupt_trap_gate_descriptorbits_get_all_zeros? (inputbits) =
genericBitstructAccessor(5, inputbits, 104)

val interrupt_trap_gate_descriptorbits_get_res3 : (int) -> int
function interrupt_trap_gate_descriptorbits_get_res3 (inputbits) =
genericBitstructAccessor(19, inputbits, 109)

val set_interrupt_trap_gate_descriptorbits_get_offset15_0 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_offset15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 0, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_selector : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_selector (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 16, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_ist : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_ist (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 32, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_res1 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 35, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_type : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 40, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_s : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 44, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_dpl : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 45, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_p : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 47, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_offset31_16 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_offset31_16 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 48, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_offset63_32 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_offset63_32 (splicebits, inputbits) =
genericBitstructUpdater(32, splicebits, 64, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_res2 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 96, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_all_zeros? : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_all_zeros? (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 104, inputbits)

val set_interrupt_trap_gate_descriptorbits_get_res3 : (int, int) -> int
function set_interrupt_trap_gate_descriptorbits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(19, splicebits, 109, inputbits)

val change_interrupt_trap_gate_descriptorbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_interrupt_trap_gate_descriptorbits (input_bits, input_offset15_0, input_selector, input_ist, input_res1, input_type, input_s, input_dpl, input_p, input_offset31_16, input_offset63_32, input_res2, input_all_zeros?, input_res3) =
let output_offset15_0 = ((match input_offset15_0 {
Some(input_offset15_0) => set_interrupt_trap_gate_descriptorbits_get_offset15_0(input_offset15_0, input_bits),
_ => input_bits
}) : int) : int in
let output_selector = ((match input_selector {
Some(input_selector) => set_interrupt_trap_gate_descriptorbits_get_selector(input_selector, output_offset15_0),
_ => output_offset15_0
}) : int) : int in
let output_ist = ((match input_ist {
Some(input_ist) => set_interrupt_trap_gate_descriptorbits_get_ist(input_ist, output_selector),
_ => output_selector
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_interrupt_trap_gate_descriptorbits_get_res1(input_res1, output_ist),
_ => output_ist
}) : int) : int in
let output_type = ((match input_type {
Some(input_type) => set_interrupt_trap_gate_descriptorbits_get_type(input_type, output_res1),
_ => output_res1
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_interrupt_trap_gate_descriptorbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_interrupt_trap_gate_descriptorbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_interrupt_trap_gate_descriptorbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_offset31_16 = ((match input_offset31_16 {
Some(input_offset31_16) => set_interrupt_trap_gate_descriptorbits_get_offset31_16(input_offset31_16, output_p),
_ => output_p
}) : int) : int in
let output_offset63_32 = ((match input_offset63_32 {
Some(input_offset63_32) => set_interrupt_trap_gate_descriptorbits_get_offset63_32(input_offset63_32, output_offset31_16),
_ => output_offset31_16
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_interrupt_trap_gate_descriptorbits_get_res2(input_res2, output_offset63_32),
_ => output_offset63_32
}) : int) : int in
let output_all_zeros? = ((match input_all_zeros? {
Some(input_all_zeros?) => set_interrupt_trap_gate_descriptorbits_get_all_zeros?(input_all_zeros?, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_interrupt_trap_gate_descriptorbits_get_res3(input_res3, output_all_zeros?),
_ => output_all_zeros?
}) : int) : int in
output_res3

val interrupt_trap_gate_descriptor_attributesbits_get_ist : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_ist (inputbits) =
genericBitstructAccessor(3, inputbits, 0)

val interrupt_trap_gate_descriptor_attributesbits_get_type : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 3)

val interrupt_trap_gate_descriptor_attributesbits_get_s : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val interrupt_trap_gate_descriptor_attributesbits_get_dpl : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 8)

val interrupt_trap_gate_descriptor_attributesbits_get_p : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val interrupt_trap_gate_descriptor_attributesbits_get_unknownbits : (int) -> int
function interrupt_trap_gate_descriptor_attributesbits_get_unknownbits (inputbits) =
genericBitstructAccessor(5, inputbits, 11)

val set_interrupt_trap_gate_descriptor_attributesbits_get_ist : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_ist (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 0, inputbits)

val set_interrupt_trap_gate_descriptor_attributesbits_get_type : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 3, inputbits)

val set_interrupt_trap_gate_descriptor_attributesbits_get_s : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_interrupt_trap_gate_descriptor_attributesbits_get_dpl : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 8, inputbits)

val set_interrupt_trap_gate_descriptor_attributesbits_get_p : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_interrupt_trap_gate_descriptor_attributesbits_get_unknownbits : (int, int) -> int
function set_interrupt_trap_gate_descriptor_attributesbits_get_unknownbits (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 11, inputbits)

val change_interrupt_trap_gate_descriptor_attributesbits : (int, option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_interrupt_trap_gate_descriptor_attributesbits (input_bits, input_ist, input_type, input_s, input_dpl, input_p, input_unknownbits) =
let output_ist = ((match input_ist {
Some(input_ist) => set_interrupt_trap_gate_descriptor_attributesbits_get_ist(input_ist, input_bits),
_ => input_bits
}) : int) : int in
let output_type = ((match input_type {
Some(input_type) => set_interrupt_trap_gate_descriptor_attributesbits_get_type(input_type, output_ist),
_ => output_ist
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_interrupt_trap_gate_descriptor_attributesbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_interrupt_trap_gate_descriptor_attributesbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_interrupt_trap_gate_descriptor_attributesbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_unknownbits = ((match input_unknownbits {
Some(input_unknownbits) => set_interrupt_trap_gate_descriptor_attributesbits_get_unknownbits(input_unknownbits, output_p),
_ => output_p
}) : int) : int in
output_unknownbits

val gdtr_idtrbits_get_base_addr : (int) -> int
function gdtr_idtrbits_get_base_addr (inputbits) =
genericBitstructAccessor(64, inputbits, 0)

val gdtr_idtrbits_get_limit : (int) -> int
function gdtr_idtrbits_get_limit (inputbits) =
genericBitstructAccessor(16, inputbits, 64)

val set_gdtr_idtrbits_get_base_addr : (int, int) -> int
function set_gdtr_idtrbits_get_base_addr (splicebits, inputbits) =
genericBitstructUpdater(64, splicebits, 0, inputbits)

val set_gdtr_idtrbits_get_limit : (int, int) -> int
function set_gdtr_idtrbits_get_limit (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 64, inputbits)

val change_gdtr_idtrbits : (int, option(int), option(int)) -> int
function change_gdtr_idtrbits (input_bits, input_base_addr, input_limit) =
let output_base_addr = ((match input_base_addr {
Some(input_base_addr) => set_gdtr_idtrbits_get_base_addr(input_base_addr, input_bits),
_ => input_bits
}) : int) : int in
let output_limit = ((match input_limit {
Some(input_limit) => set_gdtr_idtrbits_get_limit(input_limit, output_base_addr),
_ => output_base_addr
}) : int) : int in
output_limit

val code_segment_descriptorbits_get_limit15_0 : (int) -> int
function code_segment_descriptorbits_get_limit15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 0)

val code_segment_descriptorbits_get_base15_0 : (int) -> int
function code_segment_descriptorbits_get_base15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 16)

val code_segment_descriptorbits_get_base23_16 : (int) -> int
function code_segment_descriptorbits_get_base23_16 (inputbits) =
genericBitstructAccessor(8, inputbits, 32)

val code_segment_descriptorbits_get_a : (int) -> int
function code_segment_descriptorbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 40)

val code_segment_descriptorbits_get_r : (int) -> int
function code_segment_descriptorbits_get_r (inputbits) =
genericBitstructAccessor(1, inputbits, 41)

val code_segment_descriptorbits_get_c : (int) -> int
function code_segment_descriptorbits_get_c (inputbits) =
genericBitstructAccessor(1, inputbits, 42)

val code_segment_descriptorbits_get_msb_of_type : (int) -> int
function code_segment_descriptorbits_get_msb_of_type (inputbits) =
genericBitstructAccessor(1, inputbits, 43)

val code_segment_descriptorbits_get_s : (int) -> int
function code_segment_descriptorbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 44)

val code_segment_descriptorbits_get_dpl : (int) -> int
function code_segment_descriptorbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 45)

val code_segment_descriptorbits_get_p : (int) -> int
function code_segment_descriptorbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 47)

val code_segment_descriptorbits_get_limit19_16 : (int) -> int
function code_segment_descriptorbits_get_limit19_16 (inputbits) =
genericBitstructAccessor(4, inputbits, 48)

val code_segment_descriptorbits_get_avl : (int) -> int
function code_segment_descriptorbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 52)

val code_segment_descriptorbits_get_l : (int) -> int
function code_segment_descriptorbits_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 53)

val code_segment_descriptorbits_get_d : (int) -> int
function code_segment_descriptorbits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 54)

val code_segment_descriptorbits_get_g : (int) -> int
function code_segment_descriptorbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 55)

val code_segment_descriptorbits_get_base31_24 : (int) -> int
function code_segment_descriptorbits_get_base31_24 (inputbits) =
genericBitstructAccessor(8, inputbits, 56)

val set_code_segment_descriptorbits_get_limit15_0 : (int, int) -> int
function set_code_segment_descriptorbits_get_limit15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 0, inputbits)

val set_code_segment_descriptorbits_get_base15_0 : (int, int) -> int
function set_code_segment_descriptorbits_get_base15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 16, inputbits)

val set_code_segment_descriptorbits_get_base23_16 : (int, int) -> int
function set_code_segment_descriptorbits_get_base23_16 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 32, inputbits)

val set_code_segment_descriptorbits_get_a : (int, int) -> int
function set_code_segment_descriptorbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 40, inputbits)

val set_code_segment_descriptorbits_get_r : (int, int) -> int
function set_code_segment_descriptorbits_get_r (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 41, inputbits)

val set_code_segment_descriptorbits_get_c : (int, int) -> int
function set_code_segment_descriptorbits_get_c (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 42, inputbits)

val set_code_segment_descriptorbits_get_msb_of_type : (int, int) -> int
function set_code_segment_descriptorbits_get_msb_of_type (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 43, inputbits)

val set_code_segment_descriptorbits_get_s : (int, int) -> int
function set_code_segment_descriptorbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 44, inputbits)

val set_code_segment_descriptorbits_get_dpl : (int, int) -> int
function set_code_segment_descriptorbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 45, inputbits)

val set_code_segment_descriptorbits_get_p : (int, int) -> int
function set_code_segment_descriptorbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 47, inputbits)

val set_code_segment_descriptorbits_get_limit19_16 : (int, int) -> int
function set_code_segment_descriptorbits_get_limit19_16 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 48, inputbits)

val set_code_segment_descriptorbits_get_avl : (int, int) -> int
function set_code_segment_descriptorbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 52, inputbits)

val set_code_segment_descriptorbits_get_l : (int, int) -> int
function set_code_segment_descriptorbits_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 53, inputbits)

val set_code_segment_descriptorbits_get_d : (int, int) -> int
function set_code_segment_descriptorbits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 54, inputbits)

val set_code_segment_descriptorbits_get_g : (int, int) -> int
function set_code_segment_descriptorbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 55, inputbits)

val set_code_segment_descriptorbits_get_base31_24 : (int, int) -> int
function set_code_segment_descriptorbits_get_base31_24 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 56, inputbits)

val change_code_segment_descriptorbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_code_segment_descriptorbits (input_bits, input_limit15_0, input_base15_0, input_base23_16, input_a, input_r, input_c, input_msb_of_type, input_s, input_dpl, input_p, input_limit19_16, input_avl, input_l, input_d, input_g, input_base31_24) =
let output_limit15_0 = ((match input_limit15_0 {
Some(input_limit15_0) => set_code_segment_descriptorbits_get_limit15_0(input_limit15_0, input_bits),
_ => input_bits
}) : int) : int in
let output_base15_0 = ((match input_base15_0 {
Some(input_base15_0) => set_code_segment_descriptorbits_get_base15_0(input_base15_0, output_limit15_0),
_ => output_limit15_0
}) : int) : int in
let output_base23_16 = ((match input_base23_16 {
Some(input_base23_16) => set_code_segment_descriptorbits_get_base23_16(input_base23_16, output_base15_0),
_ => output_base15_0
}) : int) : int in
let output_a = ((match input_a {
Some(input_a) => set_code_segment_descriptorbits_get_a(input_a, output_base23_16),
_ => output_base23_16
}) : int) : int in
let output_r = ((match input_r {
Some(input_r) => set_code_segment_descriptorbits_get_r(input_r, output_a),
_ => output_a
}) : int) : int in
let output_c = ((match input_c {
Some(input_c) => set_code_segment_descriptorbits_get_c(input_c, output_r),
_ => output_r
}) : int) : int in
let output_msb_of_type = ((match input_msb_of_type {
Some(input_msb_of_type) => set_code_segment_descriptorbits_get_msb_of_type(input_msb_of_type, output_c),
_ => output_c
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_code_segment_descriptorbits_get_s(input_s, output_msb_of_type),
_ => output_msb_of_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_code_segment_descriptorbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_code_segment_descriptorbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_limit19_16 = ((match input_limit19_16 {
Some(input_limit19_16) => set_code_segment_descriptorbits_get_limit19_16(input_limit19_16, output_p),
_ => output_p
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_code_segment_descriptorbits_get_avl(input_avl, output_limit19_16),
_ => output_limit19_16
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_code_segment_descriptorbits_get_l(input_l, output_avl),
_ => output_avl
}) : int) : int in
let output_d = ((match input_d {
Some(input_d) => set_code_segment_descriptorbits_get_d(input_d, output_l),
_ => output_l
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_code_segment_descriptorbits_get_g(input_g, output_d),
_ => output_d
}) : int) : int in
let output_base31_24 = ((match input_base31_24 {
Some(input_base31_24) => set_code_segment_descriptorbits_get_base31_24(input_base31_24, output_g),
_ => output_g
}) : int) : int in
output_base31_24

val code_segment_descriptor_attributesbits_get_a : (int) -> int
function code_segment_descriptor_attributesbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val code_segment_descriptor_attributesbits_get_r : (int) -> int
function code_segment_descriptor_attributesbits_get_r (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val code_segment_descriptor_attributesbits_get_c : (int) -> int
function code_segment_descriptor_attributesbits_get_c (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val code_segment_descriptor_attributesbits_get_msb_of_type : (int) -> int
function code_segment_descriptor_attributesbits_get_msb_of_type (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val code_segment_descriptor_attributesbits_get_s : (int) -> int
function code_segment_descriptor_attributesbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val code_segment_descriptor_attributesbits_get_dpl : (int) -> int
function code_segment_descriptor_attributesbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 5)

val code_segment_descriptor_attributesbits_get_p : (int) -> int
function code_segment_descriptor_attributesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val code_segment_descriptor_attributesbits_get_avl : (int) -> int
function code_segment_descriptor_attributesbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val code_segment_descriptor_attributesbits_get_l : (int) -> int
function code_segment_descriptor_attributesbits_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val code_segment_descriptor_attributesbits_get_d : (int) -> int
function code_segment_descriptor_attributesbits_get_d (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val code_segment_descriptor_attributesbits_get_g : (int) -> int
function code_segment_descriptor_attributesbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 11)

val code_segment_descriptor_attributesbits_get_unknownbits : (int) -> int
function code_segment_descriptor_attributesbits_get_unknownbits (inputbits) =
genericBitstructAccessor(4, inputbits, 12)

val set_code_segment_descriptor_attributesbits_get_a : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_code_segment_descriptor_attributesbits_get_r : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_r (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_code_segment_descriptor_attributesbits_get_c : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_c (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_code_segment_descriptor_attributesbits_get_msb_of_type : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_msb_of_type (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_code_segment_descriptor_attributesbits_get_s : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_code_segment_descriptor_attributesbits_get_dpl : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 5, inputbits)

val set_code_segment_descriptor_attributesbits_get_p : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_code_segment_descriptor_attributesbits_get_avl : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_code_segment_descriptor_attributesbits_get_l : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_code_segment_descriptor_attributesbits_get_d : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_d (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_code_segment_descriptor_attributesbits_get_g : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 11, inputbits)

val set_code_segment_descriptor_attributesbits_get_unknownbits : (int, int) -> int
function set_code_segment_descriptor_attributesbits_get_unknownbits (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 12, inputbits)

val change_code_segment_descriptor_attributesbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_code_segment_descriptor_attributesbits (input_bits, input_a, input_r, input_c, input_msb_of_type, input_s, input_dpl, input_p, input_avl, input_l, input_d, input_g, input_unknownbits) =
let output_a = ((match input_a {
Some(input_a) => set_code_segment_descriptor_attributesbits_get_a(input_a, input_bits),
_ => input_bits
}) : int) : int in
let output_r = ((match input_r {
Some(input_r) => set_code_segment_descriptor_attributesbits_get_r(input_r, output_a),
_ => output_a
}) : int) : int in
let output_c = ((match input_c {
Some(input_c) => set_code_segment_descriptor_attributesbits_get_c(input_c, output_r),
_ => output_r
}) : int) : int in
let output_msb_of_type = ((match input_msb_of_type {
Some(input_msb_of_type) => set_code_segment_descriptor_attributesbits_get_msb_of_type(input_msb_of_type, output_c),
_ => output_c
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_code_segment_descriptor_attributesbits_get_s(input_s, output_msb_of_type),
_ => output_msb_of_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_code_segment_descriptor_attributesbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_code_segment_descriptor_attributesbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_code_segment_descriptor_attributesbits_get_avl(input_avl, output_p),
_ => output_p
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_code_segment_descriptor_attributesbits_get_l(input_l, output_avl),
_ => output_avl
}) : int) : int in
let output_d = ((match input_d {
Some(input_d) => set_code_segment_descriptor_attributesbits_get_d(input_d, output_l),
_ => output_l
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_code_segment_descriptor_attributesbits_get_g(input_g, output_d),
_ => output_d
}) : int) : int in
let output_unknownbits = ((match input_unknownbits {
Some(input_unknownbits) => set_code_segment_descriptor_attributesbits_get_unknownbits(input_unknownbits, output_g),
_ => output_g
}) : int) : int in
output_unknownbits

val data_segment_descriptorbits_get_limit15_0 : (int) -> int
function data_segment_descriptorbits_get_limit15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 0)

val data_segment_descriptorbits_get_base15_0 : (int) -> int
function data_segment_descriptorbits_get_base15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 16)

val data_segment_descriptorbits_get_base23_16 : (int) -> int
function data_segment_descriptorbits_get_base23_16 (inputbits) =
genericBitstructAccessor(8, inputbits, 32)

val data_segment_descriptorbits_get_a : (int) -> int
function data_segment_descriptorbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 40)

val data_segment_descriptorbits_get_w : (int) -> int
function data_segment_descriptorbits_get_w (inputbits) =
genericBitstructAccessor(1, inputbits, 41)

val data_segment_descriptorbits_get_e : (int) -> int
function data_segment_descriptorbits_get_e (inputbits) =
genericBitstructAccessor(1, inputbits, 42)

val data_segment_descriptorbits_get_msb_of_type : (int) -> int
function data_segment_descriptorbits_get_msb_of_type (inputbits) =
genericBitstructAccessor(1, inputbits, 43)

val data_segment_descriptorbits_get_s : (int) -> int
function data_segment_descriptorbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 44)

val data_segment_descriptorbits_get_dpl : (int) -> int
function data_segment_descriptorbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 45)

val data_segment_descriptorbits_get_p : (int) -> int
function data_segment_descriptorbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 47)

val data_segment_descriptorbits_get_limit19_16 : (int) -> int
function data_segment_descriptorbits_get_limit19_16 (inputbits) =
genericBitstructAccessor(4, inputbits, 48)

val data_segment_descriptorbits_get_avl : (int) -> int
function data_segment_descriptorbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 52)

val data_segment_descriptorbits_get_l : (int) -> int
function data_segment_descriptorbits_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 53)

val data_segment_descriptorbits_get_d_b : (int) -> int
function data_segment_descriptorbits_get_d_b (inputbits) =
genericBitstructAccessor(1, inputbits, 54)

val data_segment_descriptorbits_get_g : (int) -> int
function data_segment_descriptorbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 55)

val data_segment_descriptorbits_get_base31_24 : (int) -> int
function data_segment_descriptorbits_get_base31_24 (inputbits) =
genericBitstructAccessor(8, inputbits, 56)

val set_data_segment_descriptorbits_get_limit15_0 : (int, int) -> int
function set_data_segment_descriptorbits_get_limit15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 0, inputbits)

val set_data_segment_descriptorbits_get_base15_0 : (int, int) -> int
function set_data_segment_descriptorbits_get_base15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 16, inputbits)

val set_data_segment_descriptorbits_get_base23_16 : (int, int) -> int
function set_data_segment_descriptorbits_get_base23_16 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 32, inputbits)

val set_data_segment_descriptorbits_get_a : (int, int) -> int
function set_data_segment_descriptorbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 40, inputbits)

val set_data_segment_descriptorbits_get_w : (int, int) -> int
function set_data_segment_descriptorbits_get_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 41, inputbits)

val set_data_segment_descriptorbits_get_e : (int, int) -> int
function set_data_segment_descriptorbits_get_e (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 42, inputbits)

val set_data_segment_descriptorbits_get_msb_of_type : (int, int) -> int
function set_data_segment_descriptorbits_get_msb_of_type (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 43, inputbits)

val set_data_segment_descriptorbits_get_s : (int, int) -> int
function set_data_segment_descriptorbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 44, inputbits)

val set_data_segment_descriptorbits_get_dpl : (int, int) -> int
function set_data_segment_descriptorbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 45, inputbits)

val set_data_segment_descriptorbits_get_p : (int, int) -> int
function set_data_segment_descriptorbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 47, inputbits)

val set_data_segment_descriptorbits_get_limit19_16 : (int, int) -> int
function set_data_segment_descriptorbits_get_limit19_16 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 48, inputbits)

val set_data_segment_descriptorbits_get_avl : (int, int) -> int
function set_data_segment_descriptorbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 52, inputbits)

val set_data_segment_descriptorbits_get_l : (int, int) -> int
function set_data_segment_descriptorbits_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 53, inputbits)

val set_data_segment_descriptorbits_get_d_b : (int, int) -> int
function set_data_segment_descriptorbits_get_d_b (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 54, inputbits)

val set_data_segment_descriptorbits_get_g : (int, int) -> int
function set_data_segment_descriptorbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 55, inputbits)

val set_data_segment_descriptorbits_get_base31_24 : (int, int) -> int
function set_data_segment_descriptorbits_get_base31_24 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 56, inputbits)

val change_data_segment_descriptorbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_data_segment_descriptorbits (input_bits, input_limit15_0, input_base15_0, input_base23_16, input_a, input_w, input_e, input_msb_of_type, input_s, input_dpl, input_p, input_limit19_16, input_avl, input_l, input_d_b, input_g, input_base31_24) =
let output_limit15_0 = ((match input_limit15_0 {
Some(input_limit15_0) => set_data_segment_descriptorbits_get_limit15_0(input_limit15_0, input_bits),
_ => input_bits
}) : int) : int in
let output_base15_0 = ((match input_base15_0 {
Some(input_base15_0) => set_data_segment_descriptorbits_get_base15_0(input_base15_0, output_limit15_0),
_ => output_limit15_0
}) : int) : int in
let output_base23_16 = ((match input_base23_16 {
Some(input_base23_16) => set_data_segment_descriptorbits_get_base23_16(input_base23_16, output_base15_0),
_ => output_base15_0
}) : int) : int in
let output_a = ((match input_a {
Some(input_a) => set_data_segment_descriptorbits_get_a(input_a, output_base23_16),
_ => output_base23_16
}) : int) : int in
let output_w = ((match input_w {
Some(input_w) => set_data_segment_descriptorbits_get_w(input_w, output_a),
_ => output_a
}) : int) : int in
let output_e = ((match input_e {
Some(input_e) => set_data_segment_descriptorbits_get_e(input_e, output_w),
_ => output_w
}) : int) : int in
let output_msb_of_type = ((match input_msb_of_type {
Some(input_msb_of_type) => set_data_segment_descriptorbits_get_msb_of_type(input_msb_of_type, output_e),
_ => output_e
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_data_segment_descriptorbits_get_s(input_s, output_msb_of_type),
_ => output_msb_of_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_data_segment_descriptorbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_data_segment_descriptorbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_limit19_16 = ((match input_limit19_16 {
Some(input_limit19_16) => set_data_segment_descriptorbits_get_limit19_16(input_limit19_16, output_p),
_ => output_p
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_data_segment_descriptorbits_get_avl(input_avl, output_limit19_16),
_ => output_limit19_16
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_data_segment_descriptorbits_get_l(input_l, output_avl),
_ => output_avl
}) : int) : int in
let output_d_b = ((match input_d_b {
Some(input_d_b) => set_data_segment_descriptorbits_get_d_b(input_d_b, output_l),
_ => output_l
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_data_segment_descriptorbits_get_g(input_g, output_d_b),
_ => output_d_b
}) : int) : int in
let output_base31_24 = ((match input_base31_24 {
Some(input_base31_24) => set_data_segment_descriptorbits_get_base31_24(input_base31_24, output_g),
_ => output_g
}) : int) : int in
output_base31_24

val data_segment_descriptor_attributesbits_get_a : (int) -> int
function data_segment_descriptor_attributesbits_get_a (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val data_segment_descriptor_attributesbits_get_w : (int) -> int
function data_segment_descriptor_attributesbits_get_w (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val data_segment_descriptor_attributesbits_get_e : (int) -> int
function data_segment_descriptor_attributesbits_get_e (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val data_segment_descriptor_attributesbits_get_msb_of_type : (int) -> int
function data_segment_descriptor_attributesbits_get_msb_of_type (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val data_segment_descriptor_attributesbits_get_s : (int) -> int
function data_segment_descriptor_attributesbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val data_segment_descriptor_attributesbits_get_dpl : (int) -> int
function data_segment_descriptor_attributesbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 5)

val data_segment_descriptor_attributesbits_get_p : (int) -> int
function data_segment_descriptor_attributesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val data_segment_descriptor_attributesbits_get_avl : (int) -> int
function data_segment_descriptor_attributesbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val data_segment_descriptor_attributesbits_get_l : (int) -> int
function data_segment_descriptor_attributesbits_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val data_segment_descriptor_attributesbits_get_d_b : (int) -> int
function data_segment_descriptor_attributesbits_get_d_b (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val data_segment_descriptor_attributesbits_get_g : (int) -> int
function data_segment_descriptor_attributesbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 11)

val data_segment_descriptor_attributesbits_get_unknownbits : (int) -> int
function data_segment_descriptor_attributesbits_get_unknownbits (inputbits) =
genericBitstructAccessor(4, inputbits, 12)

val set_data_segment_descriptor_attributesbits_get_a : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_a (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_data_segment_descriptor_attributesbits_get_w : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_data_segment_descriptor_attributesbits_get_e : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_e (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_data_segment_descriptor_attributesbits_get_msb_of_type : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_msb_of_type (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_data_segment_descriptor_attributesbits_get_s : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_data_segment_descriptor_attributesbits_get_dpl : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 5, inputbits)

val set_data_segment_descriptor_attributesbits_get_p : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_data_segment_descriptor_attributesbits_get_avl : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_data_segment_descriptor_attributesbits_get_l : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_data_segment_descriptor_attributesbits_get_d_b : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_d_b (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_data_segment_descriptor_attributesbits_get_g : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 11, inputbits)

val set_data_segment_descriptor_attributesbits_get_unknownbits : (int, int) -> int
function set_data_segment_descriptor_attributesbits_get_unknownbits (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 12, inputbits)

val change_data_segment_descriptor_attributesbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_data_segment_descriptor_attributesbits (input_bits, input_a, input_w, input_e, input_msb_of_type, input_s, input_dpl, input_p, input_avl, input_l, input_d_b, input_g, input_unknownbits) =
let output_a = ((match input_a {
Some(input_a) => set_data_segment_descriptor_attributesbits_get_a(input_a, input_bits),
_ => input_bits
}) : int) : int in
let output_w = ((match input_w {
Some(input_w) => set_data_segment_descriptor_attributesbits_get_w(input_w, output_a),
_ => output_a
}) : int) : int in
let output_e = ((match input_e {
Some(input_e) => set_data_segment_descriptor_attributesbits_get_e(input_e, output_w),
_ => output_w
}) : int) : int in
let output_msb_of_type = ((match input_msb_of_type {
Some(input_msb_of_type) => set_data_segment_descriptor_attributesbits_get_msb_of_type(input_msb_of_type, output_e),
_ => output_e
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_data_segment_descriptor_attributesbits_get_s(input_s, output_msb_of_type),
_ => output_msb_of_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_data_segment_descriptor_attributesbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_data_segment_descriptor_attributesbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_data_segment_descriptor_attributesbits_get_avl(input_avl, output_p),
_ => output_p
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_data_segment_descriptor_attributesbits_get_l(input_l, output_avl),
_ => output_avl
}) : int) : int in
let output_d_b = ((match input_d_b {
Some(input_d_b) => set_data_segment_descriptor_attributesbits_get_d_b(input_d_b, output_l),
_ => output_l
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_data_segment_descriptor_attributesbits_get_g(input_g, output_d_b),
_ => output_d_b
}) : int) : int in
let output_unknownbits = ((match input_unknownbits {
Some(input_unknownbits) => set_data_segment_descriptor_attributesbits_get_unknownbits(input_unknownbits, output_g),
_ => output_g
}) : int) : int in
output_unknownbits

val system_segment_descriptorbits_get_limit15_0 : (int) -> int
function system_segment_descriptorbits_get_limit15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 0)

val system_segment_descriptorbits_get_base15_0 : (int) -> int
function system_segment_descriptorbits_get_base15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 16)

val system_segment_descriptorbits_get_base23_16 : (int) -> int
function system_segment_descriptorbits_get_base23_16 (inputbits) =
genericBitstructAccessor(8, inputbits, 32)

val system_segment_descriptorbits_get_type : (int) -> int
function system_segment_descriptorbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 40)

val system_segment_descriptorbits_get_s : (int) -> int
function system_segment_descriptorbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 44)

val system_segment_descriptorbits_get_dpl : (int) -> int
function system_segment_descriptorbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 45)

val system_segment_descriptorbits_get_p : (int) -> int
function system_segment_descriptorbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 47)

val system_segment_descriptorbits_get_limit19_16 : (int) -> int
function system_segment_descriptorbits_get_limit19_16 (inputbits) =
genericBitstructAccessor(4, inputbits, 48)

val system_segment_descriptorbits_get_avl : (int) -> int
function system_segment_descriptorbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 52)

val system_segment_descriptorbits_get_res1 : (int) -> int
function system_segment_descriptorbits_get_res1 (inputbits) =
genericBitstructAccessor(2, inputbits, 53)

val system_segment_descriptorbits_get_g : (int) -> int
function system_segment_descriptorbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 55)

val system_segment_descriptorbits_get_base31_24 : (int) -> int
function system_segment_descriptorbits_get_base31_24 (inputbits) =
genericBitstructAccessor(8, inputbits, 56)

val system_segment_descriptorbits_get_base63_32 : (int) -> int
function system_segment_descriptorbits_get_base63_32 (inputbits) =
genericBitstructAccessor(32, inputbits, 64)

val system_segment_descriptorbits_get_res2 : (int) -> int
function system_segment_descriptorbits_get_res2 (inputbits) =
genericBitstructAccessor(8, inputbits, 96)

val system_segment_descriptorbits_get_all_zeroes? : (int) -> int
function system_segment_descriptorbits_get_all_zeroes? (inputbits) =
genericBitstructAccessor(5, inputbits, 104)

val system_segment_descriptorbits_get_res3 : (int) -> int
function system_segment_descriptorbits_get_res3 (inputbits) =
genericBitstructAccessor(19, inputbits, 109)

val set_system_segment_descriptorbits_get_limit15_0 : (int, int) -> int
function set_system_segment_descriptorbits_get_limit15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 0, inputbits)

val set_system_segment_descriptorbits_get_base15_0 : (int, int) -> int
function set_system_segment_descriptorbits_get_base15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 16, inputbits)

val set_system_segment_descriptorbits_get_base23_16 : (int, int) -> int
function set_system_segment_descriptorbits_get_base23_16 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 32, inputbits)

val set_system_segment_descriptorbits_get_type : (int, int) -> int
function set_system_segment_descriptorbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 40, inputbits)

val set_system_segment_descriptorbits_get_s : (int, int) -> int
function set_system_segment_descriptorbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 44, inputbits)

val set_system_segment_descriptorbits_get_dpl : (int, int) -> int
function set_system_segment_descriptorbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 45, inputbits)

val set_system_segment_descriptorbits_get_p : (int, int) -> int
function set_system_segment_descriptorbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 47, inputbits)

val set_system_segment_descriptorbits_get_limit19_16 : (int, int) -> int
function set_system_segment_descriptorbits_get_limit19_16 (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 48, inputbits)

val set_system_segment_descriptorbits_get_avl : (int, int) -> int
function set_system_segment_descriptorbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 52, inputbits)

val set_system_segment_descriptorbits_get_res1 : (int, int) -> int
function set_system_segment_descriptorbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 53, inputbits)

val set_system_segment_descriptorbits_get_g : (int, int) -> int
function set_system_segment_descriptorbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 55, inputbits)

val set_system_segment_descriptorbits_get_base31_24 : (int, int) -> int
function set_system_segment_descriptorbits_get_base31_24 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 56, inputbits)

val set_system_segment_descriptorbits_get_base63_32 : (int, int) -> int
function set_system_segment_descriptorbits_get_base63_32 (splicebits, inputbits) =
genericBitstructUpdater(32, splicebits, 64, inputbits)

val set_system_segment_descriptorbits_get_res2 : (int, int) -> int
function set_system_segment_descriptorbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 96, inputbits)

val set_system_segment_descriptorbits_get_all_zeroes? : (int, int) -> int
function set_system_segment_descriptorbits_get_all_zeroes? (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 104, inputbits)

val set_system_segment_descriptorbits_get_res3 : (int, int) -> int
function set_system_segment_descriptorbits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(19, splicebits, 109, inputbits)

val change_system_segment_descriptorbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_system_segment_descriptorbits (input_bits, input_limit15_0, input_base15_0, input_base23_16, input_type, input_s, input_dpl, input_p, input_limit19_16, input_avl, input_res1, input_g, input_base31_24, input_base63_32, input_res2, input_all_zeroes?, input_res3) =
let output_limit15_0 = ((match input_limit15_0 {
Some(input_limit15_0) => set_system_segment_descriptorbits_get_limit15_0(input_limit15_0, input_bits),
_ => input_bits
}) : int) : int in
let output_base15_0 = ((match input_base15_0 {
Some(input_base15_0) => set_system_segment_descriptorbits_get_base15_0(input_base15_0, output_limit15_0),
_ => output_limit15_0
}) : int) : int in
let output_base23_16 = ((match input_base23_16 {
Some(input_base23_16) => set_system_segment_descriptorbits_get_base23_16(input_base23_16, output_base15_0),
_ => output_base15_0
}) : int) : int in
let output_type = ((match input_type {
Some(input_type) => set_system_segment_descriptorbits_get_type(input_type, output_base23_16),
_ => output_base23_16
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_system_segment_descriptorbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_system_segment_descriptorbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_system_segment_descriptorbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_limit19_16 = ((match input_limit19_16 {
Some(input_limit19_16) => set_system_segment_descriptorbits_get_limit19_16(input_limit19_16, output_p),
_ => output_p
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_system_segment_descriptorbits_get_avl(input_avl, output_limit19_16),
_ => output_limit19_16
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_system_segment_descriptorbits_get_res1(input_res1, output_avl),
_ => output_avl
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_system_segment_descriptorbits_get_g(input_g, output_res1),
_ => output_res1
}) : int) : int in
let output_base31_24 = ((match input_base31_24 {
Some(input_base31_24) => set_system_segment_descriptorbits_get_base31_24(input_base31_24, output_g),
_ => output_g
}) : int) : int in
let output_base63_32 = ((match input_base63_32 {
Some(input_base63_32) => set_system_segment_descriptorbits_get_base63_32(input_base63_32, output_base31_24),
_ => output_base31_24
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_system_segment_descriptorbits_get_res2(input_res2, output_base63_32),
_ => output_base63_32
}) : int) : int in
let output_all_zeroes? = ((match input_all_zeroes? {
Some(input_all_zeroes?) => set_system_segment_descriptorbits_get_all_zeroes?(input_all_zeroes?, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_system_segment_descriptorbits_get_res3(input_res3, output_all_zeroes?),
_ => output_all_zeroes?
}) : int) : int in
output_res3

val system_segment_descriptor_attributesbits_get_type : (int) -> int
function system_segment_descriptor_attributesbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 0)

val system_segment_descriptor_attributesbits_get_s : (int) -> int
function system_segment_descriptor_attributesbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val system_segment_descriptor_attributesbits_get_dpl : (int) -> int
function system_segment_descriptor_attributesbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 5)

val system_segment_descriptor_attributesbits_get_p : (int) -> int
function system_segment_descriptor_attributesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val system_segment_descriptor_attributesbits_get_avl : (int) -> int
function system_segment_descriptor_attributesbits_get_avl (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val system_segment_descriptor_attributesbits_get_g : (int) -> int
function system_segment_descriptor_attributesbits_get_g (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val system_segment_descriptor_attributesbits_get_unknownbits : (int) -> int
function system_segment_descriptor_attributesbits_get_unknownbits (inputbits) =
genericBitstructAccessor(6, inputbits, 10)

val set_system_segment_descriptor_attributesbits_get_type : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 0, inputbits)

val set_system_segment_descriptor_attributesbits_get_s : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_system_segment_descriptor_attributesbits_get_dpl : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 5, inputbits)

val set_system_segment_descriptor_attributesbits_get_p : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_system_segment_descriptor_attributesbits_get_avl : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_avl (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_system_segment_descriptor_attributesbits_get_g : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_g (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_system_segment_descriptor_attributesbits_get_unknownbits : (int, int) -> int
function set_system_segment_descriptor_attributesbits_get_unknownbits (splicebits, inputbits) =
genericBitstructUpdater(6, splicebits, 10, inputbits)

val change_system_segment_descriptor_attributesbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_system_segment_descriptor_attributesbits (input_bits, input_type, input_s, input_dpl, input_p, input_avl, input_g, input_unknownbits) =
let output_type = ((match input_type {
Some(input_type) => set_system_segment_descriptor_attributesbits_get_type(input_type, input_bits),
_ => input_bits
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_system_segment_descriptor_attributesbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_system_segment_descriptor_attributesbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_system_segment_descriptor_attributesbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_avl = ((match input_avl {
Some(input_avl) => set_system_segment_descriptor_attributesbits_get_avl(input_avl, output_p),
_ => output_p
}) : int) : int in
let output_g = ((match input_g {
Some(input_g) => set_system_segment_descriptor_attributesbits_get_g(input_g, output_avl),
_ => output_avl
}) : int) : int in
let output_unknownbits = ((match input_unknownbits {
Some(input_unknownbits) => set_system_segment_descriptor_attributesbits_get_unknownbits(input_unknownbits, output_g),
_ => output_g
}) : int) : int in
output_unknownbits

val call_gate_descriptorbits_get_offset15_0 : (int) -> int
function call_gate_descriptorbits_get_offset15_0 (inputbits) =
genericBitstructAccessor(16, inputbits, 0)

val call_gate_descriptorbits_get_selector : (int) -> int
function call_gate_descriptorbits_get_selector (inputbits) =
genericBitstructAccessor(16, inputbits, 16)

val call_gate_descriptorbits_get_res1 : (int) -> int
function call_gate_descriptorbits_get_res1 (inputbits) =
genericBitstructAccessor(8, inputbits, 32)

val call_gate_descriptorbits_get_type : (int) -> int
function call_gate_descriptorbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 40)

val call_gate_descriptorbits_get_s : (int) -> int
function call_gate_descriptorbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 44)

val call_gate_descriptorbits_get_dpl : (int) -> int
function call_gate_descriptorbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 45)

val call_gate_descriptorbits_get_p : (int) -> int
function call_gate_descriptorbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 47)

val call_gate_descriptorbits_get_offset31_16 : (int) -> int
function call_gate_descriptorbits_get_offset31_16 (inputbits) =
genericBitstructAccessor(16, inputbits, 48)

val call_gate_descriptorbits_get_offset63_32 : (int) -> int
function call_gate_descriptorbits_get_offset63_32 (inputbits) =
genericBitstructAccessor(32, inputbits, 64)

val call_gate_descriptorbits_get_res2 : (int) -> int
function call_gate_descriptorbits_get_res2 (inputbits) =
genericBitstructAccessor(8, inputbits, 96)

val call_gate_descriptorbits_get_all_zeroes? : (int) -> int
function call_gate_descriptorbits_get_all_zeroes? (inputbits) =
genericBitstructAccessor(5, inputbits, 104)

val call_gate_descriptorbits_get_res3 : (int) -> int
function call_gate_descriptorbits_get_res3 (inputbits) =
genericBitstructAccessor(19, inputbits, 109)

val set_call_gate_descriptorbits_get_offset15_0 : (int, int) -> int
function set_call_gate_descriptorbits_get_offset15_0 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 0, inputbits)

val set_call_gate_descriptorbits_get_selector : (int, int) -> int
function set_call_gate_descriptorbits_get_selector (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 16, inputbits)

val set_call_gate_descriptorbits_get_res1 : (int, int) -> int
function set_call_gate_descriptorbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 32, inputbits)

val set_call_gate_descriptorbits_get_type : (int, int) -> int
function set_call_gate_descriptorbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 40, inputbits)

val set_call_gate_descriptorbits_get_s : (int, int) -> int
function set_call_gate_descriptorbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 44, inputbits)

val set_call_gate_descriptorbits_get_dpl : (int, int) -> int
function set_call_gate_descriptorbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 45, inputbits)

val set_call_gate_descriptorbits_get_p : (int, int) -> int
function set_call_gate_descriptorbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 47, inputbits)

val set_call_gate_descriptorbits_get_offset31_16 : (int, int) -> int
function set_call_gate_descriptorbits_get_offset31_16 (splicebits, inputbits) =
genericBitstructUpdater(16, splicebits, 48, inputbits)

val set_call_gate_descriptorbits_get_offset63_32 : (int, int) -> int
function set_call_gate_descriptorbits_get_offset63_32 (splicebits, inputbits) =
genericBitstructUpdater(32, splicebits, 64, inputbits)

val set_call_gate_descriptorbits_get_res2 : (int, int) -> int
function set_call_gate_descriptorbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 96, inputbits)

val set_call_gate_descriptorbits_get_all_zeroes? : (int, int) -> int
function set_call_gate_descriptorbits_get_all_zeroes? (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 104, inputbits)

val set_call_gate_descriptorbits_get_res3 : (int, int) -> int
function set_call_gate_descriptorbits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(19, splicebits, 109, inputbits)

val change_call_gate_descriptorbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_call_gate_descriptorbits (input_bits, input_offset15_0, input_selector, input_res1, input_type, input_s, input_dpl, input_p, input_offset31_16, input_offset63_32, input_res2, input_all_zeroes?, input_res3) =
let output_offset15_0 = ((match input_offset15_0 {
Some(input_offset15_0) => set_call_gate_descriptorbits_get_offset15_0(input_offset15_0, input_bits),
_ => input_bits
}) : int) : int in
let output_selector = ((match input_selector {
Some(input_selector) => set_call_gate_descriptorbits_get_selector(input_selector, output_offset15_0),
_ => output_offset15_0
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_call_gate_descriptorbits_get_res1(input_res1, output_selector),
_ => output_selector
}) : int) : int in
let output_type = ((match input_type {
Some(input_type) => set_call_gate_descriptorbits_get_type(input_type, output_res1),
_ => output_res1
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_call_gate_descriptorbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_call_gate_descriptorbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_call_gate_descriptorbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_offset31_16 = ((match input_offset31_16 {
Some(input_offset31_16) => set_call_gate_descriptorbits_get_offset31_16(input_offset31_16, output_p),
_ => output_p
}) : int) : int in
let output_offset63_32 = ((match input_offset63_32 {
Some(input_offset63_32) => set_call_gate_descriptorbits_get_offset63_32(input_offset63_32, output_offset31_16),
_ => output_offset31_16
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_call_gate_descriptorbits_get_res2(input_res2, output_offset63_32),
_ => output_offset63_32
}) : int) : int in
let output_all_zeroes? = ((match input_all_zeroes? {
Some(input_all_zeroes?) => set_call_gate_descriptorbits_get_all_zeroes?(input_all_zeroes?, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_call_gate_descriptorbits_get_res3(input_res3, output_all_zeroes?),
_ => output_all_zeroes?
}) : int) : int in
output_res3

val call_gate_descriptor_attributesbits_get_type : (int) -> int
function call_gate_descriptor_attributesbits_get_type (inputbits) =
genericBitstructAccessor(4, inputbits, 0)

val call_gate_descriptor_attributesbits_get_s : (int) -> int
function call_gate_descriptor_attributesbits_get_s (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val call_gate_descriptor_attributesbits_get_dpl : (int) -> int
function call_gate_descriptor_attributesbits_get_dpl (inputbits) =
genericBitstructAccessor(2, inputbits, 5)

val call_gate_descriptor_attributesbits_get_p : (int) -> int
function call_gate_descriptor_attributesbits_get_p (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val call_gate_descriptor_attributesbits_get_unknownbits : (int) -> int
function call_gate_descriptor_attributesbits_get_unknownbits (inputbits) =
genericBitstructAccessor(8, inputbits, 8)

val set_call_gate_descriptor_attributesbits_get_type : (int, int) -> int
function set_call_gate_descriptor_attributesbits_get_type (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 0, inputbits)

val set_call_gate_descriptor_attributesbits_get_s : (int, int) -> int
function set_call_gate_descriptor_attributesbits_get_s (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_call_gate_descriptor_attributesbits_get_dpl : (int, int) -> int
function set_call_gate_descriptor_attributesbits_get_dpl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 5, inputbits)

val set_call_gate_descriptor_attributesbits_get_p : (int, int) -> int
function set_call_gate_descriptor_attributesbits_get_p (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_call_gate_descriptor_attributesbits_get_unknownbits : (int, int) -> int
function set_call_gate_descriptor_attributesbits_get_unknownbits (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 8, inputbits)

val change_call_gate_descriptor_attributesbits : (int, option(int), option(int), option(int), option(int), option(int)) -> int
function change_call_gate_descriptor_attributesbits (input_bits, input_type, input_s, input_dpl, input_p, input_unknownbits) =
let output_type = ((match input_type {
Some(input_type) => set_call_gate_descriptor_attributesbits_get_type(input_type, input_bits),
_ => input_bits
}) : int) : int in
let output_s = ((match input_s {
Some(input_s) => set_call_gate_descriptor_attributesbits_get_s(input_s, output_type),
_ => output_type
}) : int) : int in
let output_dpl = ((match input_dpl {
Some(input_dpl) => set_call_gate_descriptor_attributesbits_get_dpl(input_dpl, output_s),
_ => output_s
}) : int) : int in
let output_p = ((match input_p {
Some(input_p) => set_call_gate_descriptor_attributesbits_get_p(input_p, output_dpl),
_ => output_dpl
}) : int) : int in
let output_unknownbits = ((match input_unknownbits {
Some(input_unknownbits) => set_call_gate_descriptor_attributesbits_get_unknownbits(input_unknownbits, output_p),
_ => output_p
}) : int) : int in
output_unknownbits

