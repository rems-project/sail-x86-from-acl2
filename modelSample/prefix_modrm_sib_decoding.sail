$include <prelude.sail>
$include <string.sail>
$include "auxiliary.sail"
$include "dispatch_macros.sail"
/*Functions to detect and decode legacy prefixes*/
val get_one_byte_prefix_array_code : (bits(8)) -> {|0, 1, 2, 3, 4|} effect {escape}
function get_one_byte_prefix_array_code (byte) =
([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] : vector(256, dec, {|0, 1, 2, 3, 4|}))[the_range(0, 255, loghead(8, unsigned(byte)))]

/*Picking a mandatory prefix*/
/*<p><b>When should we interpret a SIMD prefix ( @ ( ' 66 ' ) , @ ( ' F2 ' ) , 
 @ ( ' F3 ' ) ) as the mandatory prefix for a given opcode in the two- and three-byte 
 opcode maps?</b></p> 
 
 <p>Here are some decoding rules for SIMD mandatory prefixes; note that these 
 do NOT apply for VEX/EVEX-encoded instructions because the mandatory prefixes 
 are explicitly stated there. All the examples listed below are from Intel ' s 
 XED ( x86 Encoder Decoder: @ ( ' https://intelxed.github.io/ ' ) ) .</p> 
 
 <ol> 
 
 <li> <p> For opcodes that can take mandatory prefixes, @ ( ' 66 ' ) is ignored when 
 @ ( ' F2 ' ) /@ ( ' F3 ' ) are present. Also, a mandatory prefix does not have to 
 <b>immediately</b> precede the opcode byte --- see ( 4 ) below.</p> 
 
 <p> <b> Examples: </b> </p> 
 
 <code> 
 
 ( 1 ) xed -64 -d 660f6f00 ;; movdqa xmm0, xmmword ptr [rax] 
 ( 2 ) xed -64 -d f30f6f00 ;; movdqu xmm0, xmmword ptr [rax] 
 ( 3 ) xed -64 -d 66f30f6f00 ;; movdqu xmm0, xmmword ptr [rax] ( same as ( 2 ) ) 
 ( 4 ) xed -64 -d f3660f6f00 ;; movdqu xmm0, xmmword ptr [rax] ( same as ( 2 ) ) 
 
 </code> 
 
 </li> 
 
 <li> <p> For opcodes that can take mandatory prefixes, the presence of an 
 unsupported SIMD prefix translates to a reserved instruction; such a prefix 
 does NOT act as a modifier prefix. </p> 
 
 <p> <b> Examples: </b> Opcode @ ( ' 0f 6b ' ) has a no-prefix form and @ ( ' 66 ' ) 
 mandatory prefix form. When used with @ ( ' f3 ' ) , it leads to an error; see ( 3 ) 
 below.</p> 
 
 <code> 
 
 ( 1 ) xed -64 -d 0f6b00 ;; packssdw mmx0, qword ptr [rax] 
 ( 2 ) xed -64 -d 660f6b00 ;; packssdw xmm0, xmmword ptr [rax] 
 ( 3 ) xed -64 -d f3660f6b00 ;; GENERAL_ERROR Could not decode... 
 
 </code> 
 
 </li> 
 
 </ol>*/
val compute_mandatory_prefix_for_two_byte_opcode : (range(0, 4), bits(8), bits(52)) -> bits(8) effect {escape}
function compute_mandatory_prefix_for_two_byte_opcode (proc_mode, opcode, prefixes) =
(match proc_mode {
0 => n64_bit_compute_mandatory_prefix_for_two_byte_opcode(opcode, prefixes),
_ => n32_bit_compute_mandatory_prefix_for_two_byte_opcode(opcode, prefixes)
}) : bits(8)

/*Functions to detect and decode ModR/M bytes*/
/*Returns a boolean saying whether, in 64-bit mode, 
 the given opcode in the one-byte opcode map expects a ModR/M byte.*/
val n64_bit_mode_one_byte_opcode_modr_m_p : (bits(8)) -> bool effect {escape}
function n64_bit_mode_one_byte_opcode_modr_m_p (opcode) =
([true, true, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, false, false, false, false, true, true, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true] : vector(256, dec, bool))[unsigned(opcode)]

/*Returns a boolean saying whether, in 32-bit mode, 
 the given opcode in the one-byte opcode map expects a ModR/M byte.*/
val n32_bit_mode_one_byte_opcode_modr_m_p : (bits(8)) -> bool effect {escape}
function n32_bit_mode_one_byte_opcode_modr_m_p (opcode) =
([true, true, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, false, false, false, false, true, true, true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, true, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true, false, false, false, false, true, true, true, true] : vector(256, dec, bool))[unsigned(opcode)]

/*Returns @ ( ' t ' ) if a one-byte opcode requires a ModR/M byte; 
 @ ( ' nil ' ) otherwise*/
val one_byte_opcode_modr_m_p : (range(0, 4), bits(8)) -> bool effect {escape}
function one_byte_opcode_modr_m_p (proc_mode, opcode) =
if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then n64_bit_mode_one_byte_opcode_modr_m_p(opcode)
else n32_bit_mode_one_byte_opcode_modr_m_p(opcode)

/*Returns a boolean saying whether, in 64-bit mode, 
 the given opcode in the two-byte opcode map expects a ModR/M byte.*/
val n64_bit_mode_two_byte_opcode_modr_m_p : (bits(8), bits(8)) -> bool effect {escape}
function n64_bit_mode_two_byte_opcode_modr_m_p (mandatory_prefix, opcode) =
(match mandatory_prefix {
102 => ([false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, true, true, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
243 => ([false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, true, false, false, false, false, true, true, false, false, false, true, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, true, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
242 => ([false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, true, true, true, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
_ => ([false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, true, true, true, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, true, true, false, true, true, true, true, true, true, true, true, true, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, true, true, true, true, true, false, false, false, true, true, false, true, true, true, true, true, true, true, true, true, false, false, true, false, false, false, false, false, false, false, false, false, true, true, true, true] : vector(256, dec, bool))[unsigned(opcode)]
}) : bool

/*Returns a boolean saying whether, in 32-bit mode, 
 the given opcode in the two-byte opcode map expects a ModR/M byte.*/
val n32_bit_mode_two_byte_opcode_modr_m_p : (bits(8), bits(8)) -> bool effect {escape}
function n32_bit_mode_two_byte_opcode_modr_m_p (mandatory_prefix, opcode) =
(match mandatory_prefix {
102 => ([false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, true, true, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
243 => ([false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, true, false, false, false, false, true, true, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, true, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
242 => ([false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, true, true, true, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false] : vector(256, dec, bool))[unsigned(opcode)],
_ => ([false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, true, true, true, false, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, false, false, false, false, true, true, false, true, true, true, true, true, true, true, true, true, false, false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true, true, true, true, false, false, false, false, true, true, true, true, true, false, false, false, true, true, false, true, true, true, true, true, true, true, true, true, false, false, true, false, false, false, false, false, false, false, false, false, true, true, true, true] : vector(256, dec, bool))[unsigned(opcode)]
}) : bool

/*Returns @ ( ' t ' ) if a two-byte opcode requires a ModR/M byte; 
 @ ( ' nil ' ) otherwise. Doesn ' t account for AVX/AVX2/AVX512 instructions.*/
val two_byte_opcode_modr_m_p : (range(0, 4), bits(8), bits(8)) -> bool effect {escape}
function two_byte_opcode_modr_m_p (proc_mode, mandatory_prefix, opcode) =
if (get_slice_int(8, proc_mode, 0)) == (get_slice_int(8, 0, 0))
then n64_bit_mode_two_byte_opcode_modr_m_p(mandatory_prefix, opcode)
else n32_bit_mode_two_byte_opcode_modr_m_p(mandatory_prefix, opcode)

/*Returns @ ( ' t ' ) if a VEX-encoded opcode requires a ModR/M byte; 
 @ ( ' nil ' ) otherwise.*/
val vex_opcode_modr_m_p : (bits(24), bits(8)) -> bool
function vex_opcode_modr_m_p (vex_prefixes, opcode) =
if not_bool((opcode) == (get_slice_int(8, 119, 0)))
then true
else not_bool(vex_prefixes_map_p(get_slice_int(16, 15, 0), vex_prefixes))

/*Functions to detect and decode SIB bytes*/
/*Returns a boolean saying whether a SIB byte is expected to follow a 
 ModR/M byte or not.*/
/*<p> 
 This is based on Intel manual, Jan ' 19, Volume 2, Tables 2-1 and 2-2, 
 as well as AMD manual, Dec ' 17, Volume 3, Tables A-33 and A-35. 
 When the address size is 32 or 64 bits, 
 Intel Table 2-2 and AMD Table A-35 apply: 
 a SIB byte is expected exactly when 
 ModR/M.mod is not #b11 and ModR/M.r/m is #b100. 
 When the address size is 16 bits, no SIB byte is expected. 
 </p> 
 <p> 
 Intel Table 2-3 applies to 32-bit addresses, 
 but Section 2.2.1.3 says that it also applies to 64-bit addresses. 
 AMD Table A-35 mentions both 32-bit and 64-bit addressing. 
 </p> 
 <p> 
 Intel manual, Jan ' 19, Volume 2, Table 2-7 is not very clear, 
 giving the impression that a SIB byte may be required 
 when Mod = 00 and R/M = 101 ( last column of first row ) . 
 But AMD manual, Dec ' 17, Volume 3, Table 1-16 
 ( which corresponds to Intel Table 2-7 ) clearly says that 
 no SIB byte is present in the first row. 
 Intel ' s last column of first row means that, 
 in order to use @ ( ' Disp32 ' ) instead of @ ( ' RIP+Disp32 ' ) , 
 64-bit mode must use the encoding with SIB in the second row. 
 </p> 
 <p> 
 The second argument of this function says whether 
 the address size is 16 bits or not ( i.e. 32 or 64 bits ) . 
 In 64-bit mode, this argument is always @ ( ' nil ' ) . 
 In 32-bit mode, this argument may be @ ( ' t ' ) or @ ( ' nil ' ) . 
 </p>*/
val x86_decode_sib_p : (bits(8), bool) -> bool
function x86_decode_sib_p (modr_m, n16_bit_addressp) =
(not_bool(n16_bit_addressp)) & (let r_m = (modr_m_get_r_m(modr_m)) : bits(3) in
let mod_var = (modr_m_get_mod(modr_m)) : bits(2) in
((the_bits(1, r_m)) == (get_slice_int(1, 4, 0))) & (not_bool((the_bits(1, mod_var)) == (get_slice_int(1, 3, 0)))))

