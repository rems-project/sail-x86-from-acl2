$include <prelude.sail>
$include <string.sail>
$include "utilities.sail"
/*Functions to collect legacy prefix bytes from an x86 instruction*/
/*<p>The field @ ( ' num ' ) of @ ( ' prefixes ' ) not only includes the number of 
 legacy prefixes present in an instruction, but also the number of REX bytes, 
 even though REX bytes are not stored in this structure. See @ ( tsee 
 get-prefixes ) for details.</p>*/
val prefixes_get_num : (int) -> int
function prefixes_get_num (inputbits) =
genericBitstructAccessor(4, inputbits, 0)

val prefixes_get_lck : (int) -> int
function prefixes_get_lck (inputbits) =
genericBitstructAccessor(8, inputbits, 4)

val prefixes_get_rep : (int) -> int
function prefixes_get_rep (inputbits) =
genericBitstructAccessor(8, inputbits, 12)

val prefixes_get_seg : (int) -> int
function prefixes_get_seg (inputbits) =
genericBitstructAccessor(8, inputbits, 20)

val prefixes_get_opr : (int) -> int
function prefixes_get_opr (inputbits) =
genericBitstructAccessor(8, inputbits, 28)

val prefixes_get_adr : (int) -> int
function prefixes_get_adr (inputbits) =
genericBitstructAccessor(8, inputbits, 36)

val prefixes_get_nxt : (int) -> int
function prefixes_get_nxt (inputbits) =
genericBitstructAccessor(8, inputbits, 44)

val set_prefixes_get_num : (int, int) -> int
function set_prefixes_get_num (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 0, inputbits)

val set_prefixes_get_lck : (int, int) -> int
function set_prefixes_get_lck (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 4, inputbits)

val set_prefixes_get_rep : (int, int) -> int
function set_prefixes_get_rep (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 12, inputbits)

val set_prefixes_get_seg : (int, int) -> int
function set_prefixes_get_seg (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 20, inputbits)

val set_prefixes_get_opr : (int, int) -> int
function set_prefixes_get_opr (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 28, inputbits)

val set_prefixes_get_adr : (int, int) -> int
function set_prefixes_get_adr (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 36, inputbits)

val set_prefixes_get_nxt : (int, int) -> int
function set_prefixes_get_nxt (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 44, inputbits)

val change_prefixes : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_prefixes (input_bits, input_num, input_lck, input_rep, input_seg, input_opr, input_adr, input_nxt) =
let output_num = ((match input_num {
Some(input_num) => set_prefixes_get_num(input_num, input_bits),
_ => input_bits
}) : int) : int in
let output_lck = ((match input_lck {
Some(input_lck) => set_prefixes_get_lck(input_lck, output_num),
_ => output_num
}) : int) : int in
let output_rep = ((match input_rep {
Some(input_rep) => set_prefixes_get_rep(input_rep, output_lck),
_ => output_lck
}) : int) : int in
let output_seg = ((match input_seg {
Some(input_seg) => set_prefixes_get_seg(input_seg, output_rep),
_ => output_rep
}) : int) : int in
let output_opr = ((match input_opr {
Some(input_opr) => set_prefixes_get_opr(input_opr, output_seg),
_ => output_seg
}) : int) : int in
let output_adr = ((match input_adr {
Some(input_adr) => set_prefixes_get_adr(input_adr, output_opr),
_ => output_opr
}) : int) : int in
let output_nxt = ((match input_nxt {
Some(input_nxt) => set_prefixes_get_nxt(input_nxt, output_adr),
_ => output_adr
}) : int) : int in
output_nxt

/*Functions to decode and collect VEX prefix bytes from an x86 
 instruction*/
val vex_prefixes_get_byte0 : (int) -> int
function vex_prefixes_get_byte0 (inputbits) =
genericBitstructAccessor(8, inputbits, 0)

val vex_prefixes_get_byte1 : (int) -> int
function vex_prefixes_get_byte1 (inputbits) =
genericBitstructAccessor(8, inputbits, 8)

val vex_prefixes_get_byte2 : (int) -> int
function vex_prefixes_get_byte2 (inputbits) =
genericBitstructAccessor(8, inputbits, 16)

val set_vex_prefixes_get_byte0 : (int, int) -> int
function set_vex_prefixes_get_byte0 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 0, inputbits)

val set_vex_prefixes_get_byte1 : (int, int) -> int
function set_vex_prefixes_get_byte1 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 8, inputbits)

val set_vex_prefixes_get_byte2 : (int, int) -> int
function set_vex_prefixes_get_byte2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 16, inputbits)

val change_vex_prefixes : (int, option(int), option(int), option(int)) -> int
function change_vex_prefixes (input_bits, input_byte0, input_byte1, input_byte2) =
let output_byte0 = ((match input_byte0 {
Some(input_byte0) => set_vex_prefixes_get_byte0(input_byte0, input_bits),
_ => input_bits
}) : int) : int in
let output_byte1 = ((match input_byte1 {
Some(input_byte1) => set_vex_prefixes_get_byte1(input_byte1, output_byte0),
_ => output_byte0
}) : int) : int in
let output_byte2 = ((match input_byte2 {
Some(input_byte2) => set_vex_prefixes_get_byte2(input_byte2, output_byte1),
_ => output_byte1
}) : int) : int in
output_byte2

/*Returns @ ( ' t ' ) if byte0 of the @ ( ' vex-prefixes ' ) structure is 
 either @ ( ' *vex2-byte0* ' ) or @ ( ' *vex3-byte0* ' ) ; returns @ ( ' nil ' ) otherwise*/
val vex_prefixes_byte0_p : (int) -> bool
function vex_prefixes_byte0_p (vex_prefixes) =
let byte0 = (vex_prefixes_get_byte0(vex_prefixes)) : int in
((byte0) == (197)) | ((byte0) == (196))

val vex2_byte1_get_pp : (int) -> int
function vex2_byte1_get_pp (inputbits) =
genericBitstructAccessor(2, inputbits, 0)

val vex2_byte1_get_l : (int) -> int
function vex2_byte1_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val vex2_byte1_get_vvvv : (int) -> int
function vex2_byte1_get_vvvv (inputbits) =
genericBitstructAccessor(4, inputbits, 3)

val vex2_byte1_get_r : (int) -> int
function vex2_byte1_get_r (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_vex2_byte1_get_pp : (int, int) -> int
function set_vex2_byte1_get_pp (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 0, inputbits)

val set_vex2_byte1_get_l : (int, int) -> int
function set_vex2_byte1_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_vex2_byte1_get_vvvv : (int, int) -> int
function set_vex2_byte1_get_vvvv (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 3, inputbits)

val set_vex2_byte1_get_r : (int, int) -> int
function set_vex2_byte1_get_r (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_vex2_byte1 : (int, option(int), option(int), option(int), option(int)) -> int
function change_vex2_byte1 (input_bits, input_pp, input_l, input_vvvv, input_r) =
let output_pp = ((match input_pp {
Some(input_pp) => set_vex2_byte1_get_pp(input_pp, input_bits),
_ => input_bits
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_vex2_byte1_get_l(input_l, output_pp),
_ => output_pp
}) : int) : int in
let output_vvvv = ((match input_vvvv {
Some(input_vvvv) => set_vex2_byte1_get_vvvv(input_vvvv, output_l),
_ => output_l
}) : int) : int in
let output_r = ((match input_r {
Some(input_r) => set_vex2_byte1_get_r(input_r, output_vvvv),
_ => output_vvvv
}) : int) : int in
output_r

val vex3_byte1_get_m_mmmm : (int) -> int
function vex3_byte1_get_m_mmmm (inputbits) =
genericBitstructAccessor(5, inputbits, 0)

val vex3_byte1_get_b : (int) -> int
function vex3_byte1_get_b (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val vex3_byte1_get_x : (int) -> int
function vex3_byte1_get_x (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val vex3_byte1_get_r : (int) -> int
function vex3_byte1_get_r (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_vex3_byte1_get_m_mmmm : (int, int) -> int
function set_vex3_byte1_get_m_mmmm (splicebits, inputbits) =
genericBitstructUpdater(5, splicebits, 0, inputbits)

val set_vex3_byte1_get_b : (int, int) -> int
function set_vex3_byte1_get_b (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_vex3_byte1_get_x : (int, int) -> int
function set_vex3_byte1_get_x (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_vex3_byte1_get_r : (int, int) -> int
function set_vex3_byte1_get_r (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_vex3_byte1 : (int, option(int), option(int), option(int), option(int)) -> int
function change_vex3_byte1 (input_bits, input_m_mmmm, input_b, input_x, input_r) =
let output_m_mmmm = ((match input_m_mmmm {
Some(input_m_mmmm) => set_vex3_byte1_get_m_mmmm(input_m_mmmm, input_bits),
_ => input_bits
}) : int) : int in
let output_b = ((match input_b {
Some(input_b) => set_vex3_byte1_get_b(input_b, output_m_mmmm),
_ => output_m_mmmm
}) : int) : int in
let output_x = ((match input_x {
Some(input_x) => set_vex3_byte1_get_x(input_x, output_b),
_ => output_b
}) : int) : int in
let output_r = ((match input_r {
Some(input_r) => set_vex3_byte1_get_r(input_r, output_x),
_ => output_x
}) : int) : int in
output_r

val vex3_byte2_get_pp : (int) -> int
function vex3_byte2_get_pp (inputbits) =
genericBitstructAccessor(2, inputbits, 0)

val vex3_byte2_get_l : (int) -> int
function vex3_byte2_get_l (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val vex3_byte2_get_vvvv : (int) -> int
function vex3_byte2_get_vvvv (inputbits) =
genericBitstructAccessor(4, inputbits, 3)

val vex3_byte2_get_w : (int) -> int
function vex3_byte2_get_w (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_vex3_byte2_get_pp : (int, int) -> int
function set_vex3_byte2_get_pp (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 0, inputbits)

val set_vex3_byte2_get_l : (int, int) -> int
function set_vex3_byte2_get_l (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_vex3_byte2_get_vvvv : (int, int) -> int
function set_vex3_byte2_get_vvvv (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 3, inputbits)

val set_vex3_byte2_get_w : (int, int) -> int
function set_vex3_byte2_get_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_vex3_byte2 : (int, option(int), option(int), option(int), option(int)) -> int
function change_vex3_byte2 (input_bits, input_pp, input_l, input_vvvv, input_w) =
let output_pp = ((match input_pp {
Some(input_pp) => set_vex3_byte2_get_pp(input_pp, input_bits),
_ => input_bits
}) : int) : int in
let output_l = ((match input_l {
Some(input_l) => set_vex3_byte2_get_l(input_l, output_pp),
_ => output_pp
}) : int) : int in
let output_vvvv = ((match input_vvvv {
Some(input_vvvv) => set_vex3_byte2_get_vvvv(input_vvvv, output_l),
_ => output_l
}) : int) : int in
let output_w = ((match input_w {
Some(input_w) => set_vex3_byte2_get_w(input_w, output_vvvv),
_ => output_vvvv
}) : int) : int in
output_w

/*Returns @ ( ' t ' ) if the @ ( ' vex-prefixes ' ) , irrespective of whether 
 they are two- or three-byte form, indicate the map that begins with the 
 escape bytes @ ( ' bytes ' )*/
val vex_prefixes_map_p : (int, int) -> bool
function vex_prefixes_map_p (bytes, vex_prefixes) =
let byte0 = (vex_prefixes_get_byte0(vex_prefixes)) : int in
let byte1 = (vex_prefixes_get_byte1(vex_prefixes)) : int in
(match bytes {
15 => ((byte0) == (197)) | (((byte0) == (196)) & (((vex3_byte1_get_m_mmmm(byte1)) == (1)) : bool)),
_ => ((byte0) == (196)) & (if (bytes) == (3896)
then ((vex3_byte1_get_m_mmmm(byte1)) == (2)) : bool
else ((vex3_byte1_get_m_mmmm(byte1)) == (3)) : bool)
}) : bool

/*Get the @ ( ' VVVV ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- 
 or three-byte VEX prefixes form*/
val vex_get_vvvv : (int) -> int
function vex_get_vvvv (vex_prefixes) =
(match vex_prefixes_get_byte0(vex_prefixes) {
197 => vex2_byte1_get_vvvv(vex_prefixes_get_byte1(vex_prefixes)),
196 => vex3_byte2_get_vvvv(vex_prefixes_get_byte2(vex_prefixes)),
_ => -1
}) : int

/*Get the @ ( ' L ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form*/
val vex_get_l : (int) -> int
function vex_get_l (vex_prefixes) =
(match vex_prefixes_get_byte0(vex_prefixes) {
197 => vex2_byte1_get_l(vex_prefixes_get_byte1(vex_prefixes)),
196 => vex3_byte2_get_l(vex_prefixes_get_byte2(vex_prefixes)),
_ => -1
}) : int

/*Get the @ ( ' PP ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form*/
val vex_get_pp : (int) -> int
function vex_get_pp (vex_prefixes) =
(match vex_prefixes_get_byte0(vex_prefixes) {
197 => vex2_byte1_get_pp(vex_prefixes_get_byte1(vex_prefixes)),
196 => vex3_byte2_get_pp(vex_prefixes_get_byte2(vex_prefixes)),
_ => -1
}) : int

/*Get the @ ( ' W ' ) field of @ ( ' vex-prefixes ' ) ; cognizant of the two- or 
 three-byte VEX prefixes form*/
val vex_get_w : (int) -> int
function vex_get_w (vex_prefixes) =
(match vex_prefixes_get_byte0(vex_prefixes) {
196 => vex3_byte2_get_w(vex_prefixes_get_byte2(vex_prefixes)),
_ => 0
}) : int

/*Functions to decode and collect EVEX prefix bytes from an x86 
 instruction*/
val evex_prefixes_get_byte0 : (int) -> int
function evex_prefixes_get_byte0 (inputbits) =
genericBitstructAccessor(8, inputbits, 0)

val evex_prefixes_get_byte1 : (int) -> int
function evex_prefixes_get_byte1 (inputbits) =
genericBitstructAccessor(8, inputbits, 8)

val evex_prefixes_get_byte2 : (int) -> int
function evex_prefixes_get_byte2 (inputbits) =
genericBitstructAccessor(8, inputbits, 16)

val evex_prefixes_get_byte3 : (int) -> int
function evex_prefixes_get_byte3 (inputbits) =
genericBitstructAccessor(8, inputbits, 24)

val set_evex_prefixes_get_byte0 : (int, int) -> int
function set_evex_prefixes_get_byte0 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 0, inputbits)

val set_evex_prefixes_get_byte1 : (int, int) -> int
function set_evex_prefixes_get_byte1 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 8, inputbits)

val set_evex_prefixes_get_byte2 : (int, int) -> int
function set_evex_prefixes_get_byte2 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 16, inputbits)

val set_evex_prefixes_get_byte3 : (int, int) -> int
function set_evex_prefixes_get_byte3 (splicebits, inputbits) =
genericBitstructUpdater(8, splicebits, 24, inputbits)

val change_evex_prefixes : (int, option(int), option(int), option(int), option(int)) -> int
function change_evex_prefixes (input_bits, input_byte0, input_byte1, input_byte2, input_byte3) =
let output_byte0 = ((match input_byte0 {
Some(input_byte0) => set_evex_prefixes_get_byte0(input_byte0, input_bits),
_ => input_bits
}) : int) : int in
let output_byte1 = ((match input_byte1 {
Some(input_byte1) => set_evex_prefixes_get_byte1(input_byte1, output_byte0),
_ => output_byte0
}) : int) : int in
let output_byte2 = ((match input_byte2 {
Some(input_byte2) => set_evex_prefixes_get_byte2(input_byte2, output_byte1),
_ => output_byte1
}) : int) : int in
let output_byte3 = ((match input_byte3 {
Some(input_byte3) => set_evex_prefixes_get_byte3(input_byte3, output_byte2),
_ => output_byte2
}) : int) : int in
output_byte3

val evex_byte1_get_mm : (int) -> int
function evex_byte1_get_mm (inputbits) =
genericBitstructAccessor(2, inputbits, 0)

val evex_byte1_get_res : (int) -> int
function evex_byte1_get_res (inputbits) =
genericBitstructAccessor(2, inputbits, 2)

val evex_byte1_get_r_prime : (int) -> int
function evex_byte1_get_r_prime (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val evex_byte1_get_b : (int) -> int
function evex_byte1_get_b (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val evex_byte1_get_x : (int) -> int
function evex_byte1_get_x (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val evex_byte1_get_r : (int) -> int
function evex_byte1_get_r (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_evex_byte1_get_mm : (int, int) -> int
function set_evex_byte1_get_mm (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 0, inputbits)

val set_evex_byte1_get_res : (int, int) -> int
function set_evex_byte1_get_res (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 2, inputbits)

val set_evex_byte1_get_r_prime : (int, int) -> int
function set_evex_byte1_get_r_prime (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_evex_byte1_get_b : (int, int) -> int
function set_evex_byte1_get_b (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_evex_byte1_get_x : (int, int) -> int
function set_evex_byte1_get_x (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_evex_byte1_get_r : (int, int) -> int
function set_evex_byte1_get_r (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_evex_byte1 : (int, option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_evex_byte1 (input_bits, input_mm, input_res, input_r_prime, input_b, input_x, input_r) =
let output_mm = ((match input_mm {
Some(input_mm) => set_evex_byte1_get_mm(input_mm, input_bits),
_ => input_bits
}) : int) : int in
let output_res = ((match input_res {
Some(input_res) => set_evex_byte1_get_res(input_res, output_mm),
_ => output_mm
}) : int) : int in
let output_r_prime = ((match input_r_prime {
Some(input_r_prime) => set_evex_byte1_get_r_prime(input_r_prime, output_res),
_ => output_res
}) : int) : int in
let output_b = ((match input_b {
Some(input_b) => set_evex_byte1_get_b(input_b, output_r_prime),
_ => output_r_prime
}) : int) : int in
let output_x = ((match input_x {
Some(input_x) => set_evex_byte1_get_x(input_x, output_b),
_ => output_b
}) : int) : int in
let output_r = ((match input_r {
Some(input_r) => set_evex_byte1_get_r(input_r, output_x),
_ => output_x
}) : int) : int in
output_r

val evex_byte2_get_pp : (int) -> int
function evex_byte2_get_pp (inputbits) =
genericBitstructAccessor(2, inputbits, 0)

val evex_byte2_get_res : (int) -> int
function evex_byte2_get_res (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val evex_byte2_get_vvvv : (int) -> int
function evex_byte2_get_vvvv (inputbits) =
genericBitstructAccessor(4, inputbits, 3)

val evex_byte2_get_w : (int) -> int
function evex_byte2_get_w (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_evex_byte2_get_pp : (int, int) -> int
function set_evex_byte2_get_pp (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 0, inputbits)

val set_evex_byte2_get_res : (int, int) -> int
function set_evex_byte2_get_res (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_evex_byte2_get_vvvv : (int, int) -> int
function set_evex_byte2_get_vvvv (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 3, inputbits)

val set_evex_byte2_get_w : (int, int) -> int
function set_evex_byte2_get_w (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_evex_byte2 : (int, option(int), option(int), option(int), option(int)) -> int
function change_evex_byte2 (input_bits, input_pp, input_res, input_vvvv, input_w) =
let output_pp = ((match input_pp {
Some(input_pp) => set_evex_byte2_get_pp(input_pp, input_bits),
_ => input_bits
}) : int) : int in
let output_res = ((match input_res {
Some(input_res) => set_evex_byte2_get_res(input_res, output_pp),
_ => output_pp
}) : int) : int in
let output_vvvv = ((match input_vvvv {
Some(input_vvvv) => set_evex_byte2_get_vvvv(input_vvvv, output_res),
_ => output_res
}) : int) : int in
let output_w = ((match input_w {
Some(input_w) => set_evex_byte2_get_w(input_w, output_vvvv),
_ => output_vvvv
}) : int) : int in
output_w

val evex_byte3_get_aaa : (int) -> int
function evex_byte3_get_aaa (inputbits) =
genericBitstructAccessor(3, inputbits, 0)

val evex_byte3_get_v_prime : (int) -> int
function evex_byte3_get_v_prime (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val evex_byte3_get_b : (int) -> int
function evex_byte3_get_b (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val evex_byte3_get_vl_rc : (int) -> int
function evex_byte3_get_vl_rc (inputbits) =
genericBitstructAccessor(2, inputbits, 5)

val evex_byte3_get_z : (int) -> int
function evex_byte3_get_z (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val set_evex_byte3_get_aaa : (int, int) -> int
function set_evex_byte3_get_aaa (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 0, inputbits)

val set_evex_byte3_get_v_prime : (int, int) -> int
function set_evex_byte3_get_v_prime (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_evex_byte3_get_b : (int, int) -> int
function set_evex_byte3_get_b (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_evex_byte3_get_vl_rc : (int, int) -> int
function set_evex_byte3_get_vl_rc (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 5, inputbits)

val set_evex_byte3_get_z : (int, int) -> int
function set_evex_byte3_get_z (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val change_evex_byte3 : (int, option(int), option(int), option(int), option(int), option(int)) -> int
function change_evex_byte3 (input_bits, input_aaa, input_v_prime, input_b, input_vl_rc, input_z) =
let output_aaa = ((match input_aaa {
Some(input_aaa) => set_evex_byte3_get_aaa(input_aaa, input_bits),
_ => input_bits
}) : int) : int in
let output_v_prime = ((match input_v_prime {
Some(input_v_prime) => set_evex_byte3_get_v_prime(input_v_prime, output_aaa),
_ => output_aaa
}) : int) : int in
let output_b = ((match input_b {
Some(input_b) => set_evex_byte3_get_b(input_b, output_v_prime),
_ => output_v_prime
}) : int) : int in
let output_vl_rc = ((match input_vl_rc {
Some(input_vl_rc) => set_evex_byte3_get_vl_rc(input_vl_rc, output_b),
_ => output_b
}) : int) : int in
let output_z = ((match input_z {
Some(input_z) => set_evex_byte3_get_z(input_z, output_vl_rc),
_ => output_vl_rc
}) : int) : int in
output_z

/*Get the @ ( ' VVVV ' ) field of @ ( ' evex-prefixes ' )*/
val evex_get_vvvv : (int) -> int
function evex_get_vvvv (evex_prefixes) =
evex_byte2_get_vvvv(evex_prefixes_get_byte2(evex_prefixes))

/*Get the @ ( ' v-prime ' ) field of @ ( ' evex-prefixes ' )*/
val evex_get_v_prime : (int) -> int
function evex_get_v_prime (evex_prefixes) =
evex_byte3_get_v_prime(evex_prefixes_get_byte3(evex_prefixes))

/*Get the @ ( ' vl/rc ' ) field of @ ( ' evex-prefixes ' )*/
val evex_get_vl_rc : (int) -> int
function evex_get_vl_rc (evex_prefixes) =
evex_byte3_get_vl_rc(evex_prefixes_get_byte3(evex_prefixes))

/*Get the @ ( ' PP ' ) field of @ ( ' evex-prefixes ' )*/
val evex_get_pp : (int) -> int
function evex_get_pp (evex_prefixes) =
evex_byte2_get_pp(evex_prefixes_get_byte2(evex_prefixes))

/*Get the @ ( ' W ' ) field of @ ( ' evex-prefixes ' )*/
val evex_get_w : (int) -> int
function evex_get_w (evex_prefixes) =
evex_byte2_get_w(evex_prefixes_get_byte2(evex_prefixes))

/*Bitstruct definitions to store a ModR/M byte and its fields*/
val modr_m_get_r_m : (int) -> int
function modr_m_get_r_m (inputbits) =
genericBitstructAccessor(3, inputbits, 0)

val modr_m_get_reg : (int) -> int
function modr_m_get_reg (inputbits) =
genericBitstructAccessor(3, inputbits, 3)

val modr_m_get_mod : (int) -> int
function modr_m_get_mod (inputbits) =
genericBitstructAccessor(2, inputbits, 6)

val set_modr_m_get_r_m : (int, int) -> int
function set_modr_m_get_r_m (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 0, inputbits)

val set_modr_m_get_reg : (int, int) -> int
function set_modr_m_get_reg (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 3, inputbits)

val set_modr_m_get_mod : (int, int) -> int
function set_modr_m_get_mod (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 6, inputbits)

val change_modr_m : (int, option(int), option(int), option(int)) -> int
function change_modr_m (input_bits, input_r_m, input_reg, input_mod) =
let output_r_m = ((match input_r_m {
Some(input_r_m) => set_modr_m_get_r_m(input_r_m, input_bits),
_ => input_bits
}) : int) : int in
let output_reg = ((match input_reg {
Some(input_reg) => set_modr_m_get_reg(input_reg, output_r_m),
_ => output_r_m
}) : int) : int in
let output_mod = ((match input_mod {
Some(input_mod) => set_modr_m_get_mod(input_mod, output_reg),
_ => output_reg
}) : int) : int in
output_mod

/*Bitstruct definitions to store a SIB byte and its fields*/
val sib_get_base : (int) -> int
function sib_get_base (inputbits) =
genericBitstructAccessor(3, inputbits, 0)

val sib_get_index : (int) -> int
function sib_get_index (inputbits) =
genericBitstructAccessor(3, inputbits, 3)

val sib_get_scale : (int) -> int
function sib_get_scale (inputbits) =
genericBitstructAccessor(2, inputbits, 6)

val set_sib_get_base : (int, int) -> int
function set_sib_get_base (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 0, inputbits)

val set_sib_get_index : (int, int) -> int
function set_sib_get_index (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 3, inputbits)

val set_sib_get_scale : (int, int) -> int
function set_sib_get_scale (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 6, inputbits)

val change_sib : (int, option(int), option(int), option(int)) -> int
function change_sib (input_bits, input_base, input_index, input_scale) =
let output_base = ((match input_base {
Some(input_base) => set_sib_get_base(input_base, input_bits),
_ => input_bits
}) : int) : int in
let output_index = ((match input_index {
Some(input_index) => set_sib_get_index(input_index, output_base),
_ => output_base
}) : int) : int in
let output_scale = ((match input_scale {
Some(input_scale) => set_sib_get_scale(input_scale, output_index),
_ => output_index
}) : int) : int in
output_scale

val rflagsbits_get_cf : (int) -> int
function rflagsbits_get_cf (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val rflagsbits_get_res1 : (int) -> int
function rflagsbits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val rflagsbits_get_pf : (int) -> int
function rflagsbits_get_pf (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val rflagsbits_get_res2 : (int) -> int
function rflagsbits_get_res2 (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val rflagsbits_get_af : (int) -> int
function rflagsbits_get_af (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val rflagsbits_get_res3 : (int) -> int
function rflagsbits_get_res3 (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val rflagsbits_get_zf : (int) -> int
function rflagsbits_get_zf (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val rflagsbits_get_sf : (int) -> int
function rflagsbits_get_sf (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val rflagsbits_get_tf : (int) -> int
function rflagsbits_get_tf (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val rflagsbits_get_intf : (int) -> int
function rflagsbits_get_intf (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val rflagsbits_get_df : (int) -> int
function rflagsbits_get_df (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val rflagsbits_get_of : (int) -> int
function rflagsbits_get_of (inputbits) =
genericBitstructAccessor(1, inputbits, 11)

val rflagsbits_get_iopl : (int) -> int
function rflagsbits_get_iopl (inputbits) =
genericBitstructAccessor(2, inputbits, 12)

val rflagsbits_get_nt : (int) -> int
function rflagsbits_get_nt (inputbits) =
genericBitstructAccessor(1, inputbits, 14)

val rflagsbits_get_res4 : (int) -> int
function rflagsbits_get_res4 (inputbits) =
genericBitstructAccessor(1, inputbits, 15)

val rflagsbits_get_rf : (int) -> int
function rflagsbits_get_rf (inputbits) =
genericBitstructAccessor(1, inputbits, 16)

val rflagsbits_get_vm : (int) -> int
function rflagsbits_get_vm (inputbits) =
genericBitstructAccessor(1, inputbits, 17)

val rflagsbits_get_ac : (int) -> int
function rflagsbits_get_ac (inputbits) =
genericBitstructAccessor(1, inputbits, 18)

val rflagsbits_get_vif : (int) -> int
function rflagsbits_get_vif (inputbits) =
genericBitstructAccessor(1, inputbits, 19)

val rflagsbits_get_vip : (int) -> int
function rflagsbits_get_vip (inputbits) =
genericBitstructAccessor(1, inputbits, 20)

val rflagsbits_get_id : (int) -> int
function rflagsbits_get_id (inputbits) =
genericBitstructAccessor(1, inputbits, 21)

val rflagsbits_get_res5 : (int) -> int
function rflagsbits_get_res5 (inputbits) =
genericBitstructAccessor(10, inputbits, 22)

val set_rflagsbits_get_cf : (int, int) -> int
function set_rflagsbits_get_cf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_rflagsbits_get_res1 : (int, int) -> int
function set_rflagsbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_rflagsbits_get_pf : (int, int) -> int
function set_rflagsbits_get_pf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_rflagsbits_get_res2 : (int, int) -> int
function set_rflagsbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_rflagsbits_get_af : (int, int) -> int
function set_rflagsbits_get_af (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_rflagsbits_get_res3 : (int, int) -> int
function set_rflagsbits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_rflagsbits_get_zf : (int, int) -> int
function set_rflagsbits_get_zf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_rflagsbits_get_sf : (int, int) -> int
function set_rflagsbits_get_sf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_rflagsbits_get_tf : (int, int) -> int
function set_rflagsbits_get_tf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_rflagsbits_get_intf : (int, int) -> int
function set_rflagsbits_get_intf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_rflagsbits_get_df : (int, int) -> int
function set_rflagsbits_get_df (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_rflagsbits_get_of : (int, int) -> int
function set_rflagsbits_get_of (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 11, inputbits)

val set_rflagsbits_get_iopl : (int, int) -> int
function set_rflagsbits_get_iopl (splicebits, inputbits) =
genericBitstructUpdater(2, splicebits, 12, inputbits)

val set_rflagsbits_get_nt : (int, int) -> int
function set_rflagsbits_get_nt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 14, inputbits)

val set_rflagsbits_get_res4 : (int, int) -> int
function set_rflagsbits_get_res4 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 15, inputbits)

val set_rflagsbits_get_rf : (int, int) -> int
function set_rflagsbits_get_rf (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 16, inputbits)

val set_rflagsbits_get_vm : (int, int) -> int
function set_rflagsbits_get_vm (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 17, inputbits)

val set_rflagsbits_get_ac : (int, int) -> int
function set_rflagsbits_get_ac (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 18, inputbits)

val set_rflagsbits_get_vif : (int, int) -> int
function set_rflagsbits_get_vif (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 19, inputbits)

val set_rflagsbits_get_vip : (int, int) -> int
function set_rflagsbits_get_vip (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 20, inputbits)

val set_rflagsbits_get_id : (int, int) -> int
function set_rflagsbits_get_id (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 21, inputbits)

val set_rflagsbits_get_res5 : (int, int) -> int
function set_rflagsbits_get_res5 (splicebits, inputbits) =
genericBitstructUpdater(10, splicebits, 22, inputbits)

val change_rflagsbits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_rflagsbits (input_bits, input_cf, input_res1, input_pf, input_res2, input_af, input_res3, input_zf, input_sf, input_tf, input_intf, input_df, input_of, input_iopl, input_nt, input_res4, input_rf, input_vm, input_ac, input_vif, input_vip, input_id, input_res5) =
let output_cf = ((match input_cf {
Some(input_cf) => set_rflagsbits_get_cf(input_cf, input_bits),
_ => input_bits
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_rflagsbits_get_res1(input_res1, output_cf),
_ => output_cf
}) : int) : int in
let output_pf = ((match input_pf {
Some(input_pf) => set_rflagsbits_get_pf(input_pf, output_res1),
_ => output_res1
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_rflagsbits_get_res2(input_res2, output_pf),
_ => output_pf
}) : int) : int in
let output_af = ((match input_af {
Some(input_af) => set_rflagsbits_get_af(input_af, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_rflagsbits_get_res3(input_res3, output_af),
_ => output_af
}) : int) : int in
let output_zf = ((match input_zf {
Some(input_zf) => set_rflagsbits_get_zf(input_zf, output_res3),
_ => output_res3
}) : int) : int in
let output_sf = ((match input_sf {
Some(input_sf) => set_rflagsbits_get_sf(input_sf, output_zf),
_ => output_zf
}) : int) : int in
let output_tf = ((match input_tf {
Some(input_tf) => set_rflagsbits_get_tf(input_tf, output_sf),
_ => output_sf
}) : int) : int in
let output_intf = ((match input_intf {
Some(input_intf) => set_rflagsbits_get_intf(input_intf, output_tf),
_ => output_tf
}) : int) : int in
let output_df = ((match input_df {
Some(input_df) => set_rflagsbits_get_df(input_df, output_intf),
_ => output_intf
}) : int) : int in
let output_of = ((match input_of {
Some(input_of) => set_rflagsbits_get_of(input_of, output_df),
_ => output_df
}) : int) : int in
let output_iopl = ((match input_iopl {
Some(input_iopl) => set_rflagsbits_get_iopl(input_iopl, output_of),
_ => output_of
}) : int) : int in
let output_nt = ((match input_nt {
Some(input_nt) => set_rflagsbits_get_nt(input_nt, output_iopl),
_ => output_iopl
}) : int) : int in
let output_res4 = ((match input_res4 {
Some(input_res4) => set_rflagsbits_get_res4(input_res4, output_nt),
_ => output_nt
}) : int) : int in
let output_rf = ((match input_rf {
Some(input_rf) => set_rflagsbits_get_rf(input_rf, output_res4),
_ => output_res4
}) : int) : int in
let output_vm = ((match input_vm {
Some(input_vm) => set_rflagsbits_get_vm(input_vm, output_rf),
_ => output_rf
}) : int) : int in
let output_ac = ((match input_ac {
Some(input_ac) => set_rflagsbits_get_ac(input_ac, output_vm),
_ => output_vm
}) : int) : int in
let output_vif = ((match input_vif {
Some(input_vif) => set_rflagsbits_get_vif(input_vif, output_ac),
_ => output_ac
}) : int) : int in
let output_vip = ((match input_vip {
Some(input_vip) => set_rflagsbits_get_vip(input_vip, output_vif),
_ => output_vif
}) : int) : int in
let output_id = ((match input_id {
Some(input_id) => set_rflagsbits_get_id(input_id, output_vip),
_ => output_vip
}) : int) : int in
let output_res5 = ((match input_res5 {
Some(input_res5) => set_rflagsbits_get_res5(input_res5, output_id),
_ => output_id
}) : int) : int in
output_res5

val cr0bits_get_pe : (int) -> int
function cr0bits_get_pe (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val cr0bits_get_mp : (int) -> int
function cr0bits_get_mp (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val cr0bits_get_em : (int) -> int
function cr0bits_get_em (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val cr0bits_get_ts : (int) -> int
function cr0bits_get_ts (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val cr0bits_get_et : (int) -> int
function cr0bits_get_et (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val cr0bits_get_ne : (int) -> int
function cr0bits_get_ne (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val cr0bits_get_res1 : (int) -> int
function cr0bits_get_res1 (inputbits) =
genericBitstructAccessor(10, inputbits, 6)

val cr0bits_get_wp : (int) -> int
function cr0bits_get_wp (inputbits) =
genericBitstructAccessor(1, inputbits, 16)

val cr0bits_get_res2 : (int) -> int
function cr0bits_get_res2 (inputbits) =
genericBitstructAccessor(1, inputbits, 17)

val cr0bits_get_am : (int) -> int
function cr0bits_get_am (inputbits) =
genericBitstructAccessor(1, inputbits, 18)

val cr0bits_get_res3 : (int) -> int
function cr0bits_get_res3 (inputbits) =
genericBitstructAccessor(10, inputbits, 19)

val cr0bits_get_nw : (int) -> int
function cr0bits_get_nw (inputbits) =
genericBitstructAccessor(1, inputbits, 29)

val cr0bits_get_cd : (int) -> int
function cr0bits_get_cd (inputbits) =
genericBitstructAccessor(1, inputbits, 30)

val cr0bits_get_pg : (int) -> int
function cr0bits_get_pg (inputbits) =
genericBitstructAccessor(1, inputbits, 31)

val set_cr0bits_get_pe : (int, int) -> int
function set_cr0bits_get_pe (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_cr0bits_get_mp : (int, int) -> int
function set_cr0bits_get_mp (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_cr0bits_get_em : (int, int) -> int
function set_cr0bits_get_em (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_cr0bits_get_ts : (int, int) -> int
function set_cr0bits_get_ts (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_cr0bits_get_et : (int, int) -> int
function set_cr0bits_get_et (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_cr0bits_get_ne : (int, int) -> int
function set_cr0bits_get_ne (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_cr0bits_get_res1 : (int, int) -> int
function set_cr0bits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(10, splicebits, 6, inputbits)

val set_cr0bits_get_wp : (int, int) -> int
function set_cr0bits_get_wp (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 16, inputbits)

val set_cr0bits_get_res2 : (int, int) -> int
function set_cr0bits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 17, inputbits)

val set_cr0bits_get_am : (int, int) -> int
function set_cr0bits_get_am (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 18, inputbits)

val set_cr0bits_get_res3 : (int, int) -> int
function set_cr0bits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(10, splicebits, 19, inputbits)

val set_cr0bits_get_nw : (int, int) -> int
function set_cr0bits_get_nw (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 29, inputbits)

val set_cr0bits_get_cd : (int, int) -> int
function set_cr0bits_get_cd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 30, inputbits)

val set_cr0bits_get_pg : (int, int) -> int
function set_cr0bits_get_pg (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 31, inputbits)

val change_cr0bits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_cr0bits (input_bits, input_pe, input_mp, input_em, input_ts, input_et, input_ne, input_res1, input_wp, input_res2, input_am, input_res3, input_nw, input_cd, input_pg) =
let output_pe = ((match input_pe {
Some(input_pe) => set_cr0bits_get_pe(input_pe, input_bits),
_ => input_bits
}) : int) : int in
let output_mp = ((match input_mp {
Some(input_mp) => set_cr0bits_get_mp(input_mp, output_pe),
_ => output_pe
}) : int) : int in
let output_em = ((match input_em {
Some(input_em) => set_cr0bits_get_em(input_em, output_mp),
_ => output_mp
}) : int) : int in
let output_ts = ((match input_ts {
Some(input_ts) => set_cr0bits_get_ts(input_ts, output_em),
_ => output_em
}) : int) : int in
let output_et = ((match input_et {
Some(input_et) => set_cr0bits_get_et(input_et, output_ts),
_ => output_ts
}) : int) : int in
let output_ne = ((match input_ne {
Some(input_ne) => set_cr0bits_get_ne(input_ne, output_et),
_ => output_et
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_cr0bits_get_res1(input_res1, output_ne),
_ => output_ne
}) : int) : int in
let output_wp = ((match input_wp {
Some(input_wp) => set_cr0bits_get_wp(input_wp, output_res1),
_ => output_res1
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_cr0bits_get_res2(input_res2, output_wp),
_ => output_wp
}) : int) : int in
let output_am = ((match input_am {
Some(input_am) => set_cr0bits_get_am(input_am, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_cr0bits_get_res3(input_res3, output_am),
_ => output_am
}) : int) : int in
let output_nw = ((match input_nw {
Some(input_nw) => set_cr0bits_get_nw(input_nw, output_res3),
_ => output_res3
}) : int) : int in
let output_cd = ((match input_cd {
Some(input_cd) => set_cr0bits_get_cd(input_cd, output_nw),
_ => output_nw
}) : int) : int in
let output_pg = ((match input_pg {
Some(input_pg) => set_cr0bits_get_pg(input_pg, output_cd),
_ => output_cd
}) : int) : int in
output_pg

val cr3bits_get_res1 : (int) -> int
function cr3bits_get_res1 (inputbits) =
genericBitstructAccessor(3, inputbits, 0)

val cr3bits_get_pwt : (int) -> int
function cr3bits_get_pwt (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val cr3bits_get_pcd : (int) -> int
function cr3bits_get_pcd (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val cr3bits_get_res2 : (int) -> int
function cr3bits_get_res2 (inputbits) =
genericBitstructAccessor(7, inputbits, 5)

val cr3bits_get_pdb : (int) -> int
function cr3bits_get_pdb (inputbits) =
genericBitstructAccessor(40, inputbits, 12)

val cr3bits_get_res3 : (int) -> int
function cr3bits_get_res3 (inputbits) =
genericBitstructAccessor(12, inputbits, 52)

val set_cr3bits_get_res1 : (int, int) -> int
function set_cr3bits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(3, splicebits, 0, inputbits)

val set_cr3bits_get_pwt : (int, int) -> int
function set_cr3bits_get_pwt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_cr3bits_get_pcd : (int, int) -> int
function set_cr3bits_get_pcd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_cr3bits_get_res2 : (int, int) -> int
function set_cr3bits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(7, splicebits, 5, inputbits)

val set_cr3bits_get_pdb : (int, int) -> int
function set_cr3bits_get_pdb (splicebits, inputbits) =
genericBitstructUpdater(40, splicebits, 12, inputbits)

val set_cr3bits_get_res3 : (int, int) -> int
function set_cr3bits_get_res3 (splicebits, inputbits) =
genericBitstructUpdater(12, splicebits, 52, inputbits)

val change_cr3bits : (int, option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_cr3bits (input_bits, input_res1, input_pwt, input_pcd, input_res2, input_pdb, input_res3) =
let output_res1 = ((match input_res1 {
Some(input_res1) => set_cr3bits_get_res1(input_res1, input_bits),
_ => input_bits
}) : int) : int in
let output_pwt = ((match input_pwt {
Some(input_pwt) => set_cr3bits_get_pwt(input_pwt, output_res1),
_ => output_res1
}) : int) : int in
let output_pcd = ((match input_pcd {
Some(input_pcd) => set_cr3bits_get_pcd(input_pcd, output_pwt),
_ => output_pwt
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_cr3bits_get_res2(input_res2, output_pcd),
_ => output_pcd
}) : int) : int in
let output_pdb = ((match input_pdb {
Some(input_pdb) => set_cr3bits_get_pdb(input_pdb, output_res2),
_ => output_res2
}) : int) : int in
let output_res3 = ((match input_res3 {
Some(input_res3) => set_cr3bits_get_res3(input_res3, output_pdb),
_ => output_pdb
}) : int) : int in
output_res3

val cr4bits_get_vme : (int) -> int
function cr4bits_get_vme (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val cr4bits_get_pvi : (int) -> int
function cr4bits_get_pvi (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val cr4bits_get_tsd : (int) -> int
function cr4bits_get_tsd (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val cr4bits_get_de : (int) -> int
function cr4bits_get_de (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val cr4bits_get_pse : (int) -> int
function cr4bits_get_pse (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val cr4bits_get_pae : (int) -> int
function cr4bits_get_pae (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val cr4bits_get_mce : (int) -> int
function cr4bits_get_mce (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val cr4bits_get_pge : (int) -> int
function cr4bits_get_pge (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val cr4bits_get_pce : (int) -> int
function cr4bits_get_pce (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val cr4bits_get_osfxsr : (int) -> int
function cr4bits_get_osfxsr (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val cr4bits_get_osxmmexcpt : (int) -> int
function cr4bits_get_osxmmexcpt (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val cr4bits_get_umip : (int) -> int
function cr4bits_get_umip (inputbits) =
genericBitstructAccessor(1, inputbits, 11)

val cr4bits_get_la57 : (int) -> int
function cr4bits_get_la57 (inputbits) =
genericBitstructAccessor(1, inputbits, 12)

val cr4bits_get_vmxe : (int) -> int
function cr4bits_get_vmxe (inputbits) =
genericBitstructAccessor(1, inputbits, 13)

val cr4bits_get_smxe : (int) -> int
function cr4bits_get_smxe (inputbits) =
genericBitstructAccessor(1, inputbits, 14)

val cr4bits_get_res1 : (int) -> int
function cr4bits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 15)

val cr4bits_get_fsgsbase : (int) -> int
function cr4bits_get_fsgsbase (inputbits) =
genericBitstructAccessor(1, inputbits, 16)

val cr4bits_get_pcide : (int) -> int
function cr4bits_get_pcide (inputbits) =
genericBitstructAccessor(1, inputbits, 17)

val cr4bits_get_osxsave : (int) -> int
function cr4bits_get_osxsave (inputbits) =
genericBitstructAccessor(1, inputbits, 18)

val cr4bits_get_res2 : (int) -> int
function cr4bits_get_res2 (inputbits) =
genericBitstructAccessor(1, inputbits, 19)

val cr4bits_get_smep : (int) -> int
function cr4bits_get_smep (inputbits) =
genericBitstructAccessor(1, inputbits, 20)

val cr4bits_get_smap : (int) -> int
function cr4bits_get_smap (inputbits) =
genericBitstructAccessor(1, inputbits, 21)

val set_cr4bits_get_vme : (int, int) -> int
function set_cr4bits_get_vme (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_cr4bits_get_pvi : (int, int) -> int
function set_cr4bits_get_pvi (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_cr4bits_get_tsd : (int, int) -> int
function set_cr4bits_get_tsd (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_cr4bits_get_de : (int, int) -> int
function set_cr4bits_get_de (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_cr4bits_get_pse : (int, int) -> int
function set_cr4bits_get_pse (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_cr4bits_get_pae : (int, int) -> int
function set_cr4bits_get_pae (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_cr4bits_get_mce : (int, int) -> int
function set_cr4bits_get_mce (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_cr4bits_get_pge : (int, int) -> int
function set_cr4bits_get_pge (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_cr4bits_get_pce : (int, int) -> int
function set_cr4bits_get_pce (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_cr4bits_get_osfxsr : (int, int) -> int
function set_cr4bits_get_osfxsr (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_cr4bits_get_osxmmexcpt : (int, int) -> int
function set_cr4bits_get_osxmmexcpt (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_cr4bits_get_umip : (int, int) -> int
function set_cr4bits_get_umip (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 11, inputbits)

val set_cr4bits_get_la57 : (int, int) -> int
function set_cr4bits_get_la57 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 12, inputbits)

val set_cr4bits_get_vmxe : (int, int) -> int
function set_cr4bits_get_vmxe (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 13, inputbits)

val set_cr4bits_get_smxe : (int, int) -> int
function set_cr4bits_get_smxe (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 14, inputbits)

val set_cr4bits_get_res1 : (int, int) -> int
function set_cr4bits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 15, inputbits)

val set_cr4bits_get_fsgsbase : (int, int) -> int
function set_cr4bits_get_fsgsbase (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 16, inputbits)

val set_cr4bits_get_pcide : (int, int) -> int
function set_cr4bits_get_pcide (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 17, inputbits)

val set_cr4bits_get_osxsave : (int, int) -> int
function set_cr4bits_get_osxsave (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 18, inputbits)

val set_cr4bits_get_res2 : (int, int) -> int
function set_cr4bits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 19, inputbits)

val set_cr4bits_get_smep : (int, int) -> int
function set_cr4bits_get_smep (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 20, inputbits)

val set_cr4bits_get_smap : (int, int) -> int
function set_cr4bits_get_smap (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 21, inputbits)

val change_cr4bits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_cr4bits (input_bits, input_vme, input_pvi, input_tsd, input_de, input_pse, input_pae, input_mce, input_pge, input_pce, input_osfxsr, input_osxmmexcpt, input_umip, input_la57, input_vmxe, input_smxe, input_res1, input_fsgsbase, input_pcide, input_osxsave, input_res2, input_smep, input_smap) =
let output_vme = ((match input_vme {
Some(input_vme) => set_cr4bits_get_vme(input_vme, input_bits),
_ => input_bits
}) : int) : int in
let output_pvi = ((match input_pvi {
Some(input_pvi) => set_cr4bits_get_pvi(input_pvi, output_vme),
_ => output_vme
}) : int) : int in
let output_tsd = ((match input_tsd {
Some(input_tsd) => set_cr4bits_get_tsd(input_tsd, output_pvi),
_ => output_pvi
}) : int) : int in
let output_de = ((match input_de {
Some(input_de) => set_cr4bits_get_de(input_de, output_tsd),
_ => output_tsd
}) : int) : int in
let output_pse = ((match input_pse {
Some(input_pse) => set_cr4bits_get_pse(input_pse, output_de),
_ => output_de
}) : int) : int in
let output_pae = ((match input_pae {
Some(input_pae) => set_cr4bits_get_pae(input_pae, output_pse),
_ => output_pse
}) : int) : int in
let output_mce = ((match input_mce {
Some(input_mce) => set_cr4bits_get_mce(input_mce, output_pae),
_ => output_pae
}) : int) : int in
let output_pge = ((match input_pge {
Some(input_pge) => set_cr4bits_get_pge(input_pge, output_mce),
_ => output_mce
}) : int) : int in
let output_pce = ((match input_pce {
Some(input_pce) => set_cr4bits_get_pce(input_pce, output_pge),
_ => output_pge
}) : int) : int in
let output_osfxsr = ((match input_osfxsr {
Some(input_osfxsr) => set_cr4bits_get_osfxsr(input_osfxsr, output_pce),
_ => output_pce
}) : int) : int in
let output_osxmmexcpt = ((match input_osxmmexcpt {
Some(input_osxmmexcpt) => set_cr4bits_get_osxmmexcpt(input_osxmmexcpt, output_osfxsr),
_ => output_osfxsr
}) : int) : int in
let output_umip = ((match input_umip {
Some(input_umip) => set_cr4bits_get_umip(input_umip, output_osxmmexcpt),
_ => output_osxmmexcpt
}) : int) : int in
let output_la57 = ((match input_la57 {
Some(input_la57) => set_cr4bits_get_la57(input_la57, output_umip),
_ => output_umip
}) : int) : int in
let output_vmxe = ((match input_vmxe {
Some(input_vmxe) => set_cr4bits_get_vmxe(input_vmxe, output_la57),
_ => output_la57
}) : int) : int in
let output_smxe = ((match input_smxe {
Some(input_smxe) => set_cr4bits_get_smxe(input_smxe, output_vmxe),
_ => output_vmxe
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_cr4bits_get_res1(input_res1, output_smxe),
_ => output_smxe
}) : int) : int in
let output_fsgsbase = ((match input_fsgsbase {
Some(input_fsgsbase) => set_cr4bits_get_fsgsbase(input_fsgsbase, output_res1),
_ => output_res1
}) : int) : int in
let output_pcide = ((match input_pcide {
Some(input_pcide) => set_cr4bits_get_pcide(input_pcide, output_fsgsbase),
_ => output_fsgsbase
}) : int) : int in
let output_osxsave = ((match input_osxsave {
Some(input_osxsave) => set_cr4bits_get_osxsave(input_osxsave, output_pcide),
_ => output_pcide
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_cr4bits_get_res2(input_res2, output_osxsave),
_ => output_osxsave
}) : int) : int in
let output_smep = ((match input_smep {
Some(input_smep) => set_cr4bits_get_smep(input_smep, output_res2),
_ => output_res2
}) : int) : int in
let output_smap = ((match input_smap {
Some(input_smap) => set_cr4bits_get_smap(input_smap, output_smep),
_ => output_smep
}) : int) : int in
output_smap

val cr8bits_get_cr8_trpl : (int) -> int
function cr8bits_get_cr8_trpl (inputbits) =
genericBitstructAccessor(4, inputbits, 0)

val set_cr8bits_get_cr8_trpl : (int, int) -> int
function set_cr8bits_get_cr8_trpl (splicebits, inputbits) =
genericBitstructUpdater(4, splicebits, 0, inputbits)

val change_cr8bits : (int, option(int)) -> int
function change_cr8bits (input_bits, input_cr8_trpl) =
let output_cr8_trpl = ((match input_cr8_trpl {
Some(input_cr8_trpl) => set_cr8bits_get_cr8_trpl(input_cr8_trpl, input_bits),
_ => input_bits
}) : int) : int in
output_cr8_trpl

val xcr0bits_get_fpu_mmx_state : (int) -> int
function xcr0bits_get_fpu_mmx_state (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val xcr0bits_get_sse_state : (int) -> int
function xcr0bits_get_sse_state (inputbits) =
genericBitstructAccessor(1, inputbits, 1)

val xcr0bits_get_avx_state : (int) -> int
function xcr0bits_get_avx_state (inputbits) =
genericBitstructAccessor(1, inputbits, 2)

val xcr0bits_get_bndreg_state : (int) -> int
function xcr0bits_get_bndreg_state (inputbits) =
genericBitstructAccessor(1, inputbits, 3)

val xcr0bits_get_bndcsr_state : (int) -> int
function xcr0bits_get_bndcsr_state (inputbits) =
genericBitstructAccessor(1, inputbits, 4)

val xcr0bits_get_opmask_state : (int) -> int
function xcr0bits_get_opmask_state (inputbits) =
genericBitstructAccessor(1, inputbits, 5)

val xcr0bits_get_zmm_hi256_state : (int) -> int
function xcr0bits_get_zmm_hi256_state (inputbits) =
genericBitstructAccessor(1, inputbits, 6)

val xcr0bits_get_hi16_zmm_state : (int) -> int
function xcr0bits_get_hi16_zmm_state (inputbits) =
genericBitstructAccessor(1, inputbits, 7)

val xcr0bits_get_res1 : (int) -> int
function xcr0bits_get_res1 (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val xcr0bits_get_pkru_state : (int) -> int
function xcr0bits_get_pkru_state (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val xcr0bits_get_res2 : (int) -> int
function xcr0bits_get_res2 (inputbits) =
genericBitstructAccessor(54, inputbits, 10)

val set_xcr0bits_get_fpu_mmx_state : (int, int) -> int
function set_xcr0bits_get_fpu_mmx_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_xcr0bits_get_sse_state : (int, int) -> int
function set_xcr0bits_get_sse_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 1, inputbits)

val set_xcr0bits_get_avx_state : (int, int) -> int
function set_xcr0bits_get_avx_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 2, inputbits)

val set_xcr0bits_get_bndreg_state : (int, int) -> int
function set_xcr0bits_get_bndreg_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 3, inputbits)

val set_xcr0bits_get_bndcsr_state : (int, int) -> int
function set_xcr0bits_get_bndcsr_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 4, inputbits)

val set_xcr0bits_get_opmask_state : (int, int) -> int
function set_xcr0bits_get_opmask_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 5, inputbits)

val set_xcr0bits_get_zmm_hi256_state : (int, int) -> int
function set_xcr0bits_get_zmm_hi256_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 6, inputbits)

val set_xcr0bits_get_hi16_zmm_state : (int, int) -> int
function set_xcr0bits_get_hi16_zmm_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 7, inputbits)

val set_xcr0bits_get_res1 : (int, int) -> int
function set_xcr0bits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_xcr0bits_get_pkru_state : (int, int) -> int
function set_xcr0bits_get_pkru_state (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_xcr0bits_get_res2 : (int, int) -> int
function set_xcr0bits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(54, splicebits, 10, inputbits)

val change_xcr0bits : (int, option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_xcr0bits (input_bits, input_fpu_mmx_state, input_sse_state, input_avx_state, input_bndreg_state, input_bndcsr_state, input_opmask_state, input_zmm_hi256_state, input_hi16_zmm_state, input_res1, input_pkru_state, input_res2) =
let output_fpu_mmx_state = ((match input_fpu_mmx_state {
Some(input_fpu_mmx_state) => set_xcr0bits_get_fpu_mmx_state(input_fpu_mmx_state, input_bits),
_ => input_bits
}) : int) : int in
let output_sse_state = ((match input_sse_state {
Some(input_sse_state) => set_xcr0bits_get_sse_state(input_sse_state, output_fpu_mmx_state),
_ => output_fpu_mmx_state
}) : int) : int in
let output_avx_state = ((match input_avx_state {
Some(input_avx_state) => set_xcr0bits_get_avx_state(input_avx_state, output_sse_state),
_ => output_sse_state
}) : int) : int in
let output_bndreg_state = ((match input_bndreg_state {
Some(input_bndreg_state) => set_xcr0bits_get_bndreg_state(input_bndreg_state, output_avx_state),
_ => output_avx_state
}) : int) : int in
let output_bndcsr_state = ((match input_bndcsr_state {
Some(input_bndcsr_state) => set_xcr0bits_get_bndcsr_state(input_bndcsr_state, output_bndreg_state),
_ => output_bndreg_state
}) : int) : int in
let output_opmask_state = ((match input_opmask_state {
Some(input_opmask_state) => set_xcr0bits_get_opmask_state(input_opmask_state, output_bndcsr_state),
_ => output_bndcsr_state
}) : int) : int in
let output_zmm_hi256_state = ((match input_zmm_hi256_state {
Some(input_zmm_hi256_state) => set_xcr0bits_get_zmm_hi256_state(input_zmm_hi256_state, output_opmask_state),
_ => output_opmask_state
}) : int) : int in
let output_hi16_zmm_state = ((match input_hi16_zmm_state {
Some(input_hi16_zmm_state) => set_xcr0bits_get_hi16_zmm_state(input_hi16_zmm_state, output_zmm_hi256_state),
_ => output_zmm_hi256_state
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_xcr0bits_get_res1(input_res1, output_hi16_zmm_state),
_ => output_hi16_zmm_state
}) : int) : int in
let output_pkru_state = ((match input_pkru_state {
Some(input_pkru_state) => set_xcr0bits_get_pkru_state(input_pkru_state, output_res1),
_ => output_res1
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_xcr0bits_get_res2(input_res2, output_pkru_state),
_ => output_pkru_state
}) : int) : int in
output_res2

val ia32_eferbits_get_sce : (int) -> int
function ia32_eferbits_get_sce (inputbits) =
genericBitstructAccessor(1, inputbits, 0)

val ia32_eferbits_get_res1 : (int) -> int
function ia32_eferbits_get_res1 (inputbits) =
genericBitstructAccessor(7, inputbits, 1)

val ia32_eferbits_get_lme : (int) -> int
function ia32_eferbits_get_lme (inputbits) =
genericBitstructAccessor(1, inputbits, 8)

val ia32_eferbits_get_res2 : (int) -> int
function ia32_eferbits_get_res2 (inputbits) =
genericBitstructAccessor(1, inputbits, 9)

val ia32_eferbits_get_lma : (int) -> int
function ia32_eferbits_get_lma (inputbits) =
genericBitstructAccessor(1, inputbits, 10)

val ia32_eferbits_get_nxe : (int) -> int
function ia32_eferbits_get_nxe (inputbits) =
genericBitstructAccessor(1, inputbits, 11)

val set_ia32_eferbits_get_sce : (int, int) -> int
function set_ia32_eferbits_get_sce (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 0, inputbits)

val set_ia32_eferbits_get_res1 : (int, int) -> int
function set_ia32_eferbits_get_res1 (splicebits, inputbits) =
genericBitstructUpdater(7, splicebits, 1, inputbits)

val set_ia32_eferbits_get_lme : (int, int) -> int
function set_ia32_eferbits_get_lme (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 8, inputbits)

val set_ia32_eferbits_get_res2 : (int, int) -> int
function set_ia32_eferbits_get_res2 (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 9, inputbits)

val set_ia32_eferbits_get_lma : (int, int) -> int
function set_ia32_eferbits_get_lma (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 10, inputbits)

val set_ia32_eferbits_get_nxe : (int, int) -> int
function set_ia32_eferbits_get_nxe (splicebits, inputbits) =
genericBitstructUpdater(1, splicebits, 11, inputbits)

val change_ia32_eferbits : (int, option(int), option(int), option(int), option(int), option(int), option(int)) -> int
function change_ia32_eferbits (input_bits, input_sce, input_res1, input_lme, input_res2, input_lma, input_nxe) =
let output_sce = ((match input_sce {
Some(input_sce) => set_ia32_eferbits_get_sce(input_sce, input_bits),
_ => input_bits
}) : int) : int in
let output_res1 = ((match input_res1 {
Some(input_res1) => set_ia32_eferbits_get_res1(input_res1, output_sce),
_ => output_sce
}) : int) : int in
let output_lme = ((match input_lme {
Some(input_lme) => set_ia32_eferbits_get_lme(input_lme, output_res1),
_ => output_res1
}) : int) : int in
let output_res2 = ((match input_res2 {
Some(input_res2) => set_ia32_eferbits_get_res2(input_res2, output_lme),
_ => output_lme
}) : int) : int in
let output_lma = ((match input_lma {
Some(input_lma) => set_ia32_eferbits_get_lma(input_lma, output_res2),
_ => output_res2
}) : int) : int in
let output_nxe = ((match input_nxe {
Some(input_nxe) => set_ia32_eferbits_get_nxe(input_nxe, output_lma),
_ => output_lma
}) : int) : int in
output_nxe

