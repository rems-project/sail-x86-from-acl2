$ifndef __X86_SEGMENTATION_STRUCTURES
$define __X86_SEGMENTATION_STRUCTURES
$include <prelude.sail>
$include <string.sail>
bitfield hidden_segment_registerbits : bits(112) = {
  attr : 111 .. 96,
  limit : 95 .. 64,
  base_addr : 63 .. 0
}
bitfield segment_selectorbits : bits(16) = {
  index : 15 .. 3,
  ti : 2 .. 2,
  rpl : 1 .. 0
}
bitfield interrupt_trap_gate_descriptorbits : bits(128) = {
  res3 : 127 .. 109,
  all_zeros? : 108 .. 104,
  res2 : 103 .. 96,
  offset63_32 : 95 .. 64,
  offset31_16 : 63 .. 48,
  p : 47 .. 47,
  dpl : 46 .. 45,
  s : 44 .. 44,
  sailtype : 43 .. 40,
  res1 : 39 .. 35,
  ist : 34 .. 32,
  selector : 31 .. 16,
  offset15_0 : 15 .. 0
}
bitfield interrupt_trap_gate_descriptor_attributesbits : bits(16) = {
  unknownbits : 15 .. 11,
  p : 10 .. 10,
  dpl : 9 .. 8,
  s : 7 .. 7,
  sailtype : 6 .. 3,
  ist : 2 .. 0
}
bitfield gdtr_idtrbits : bits(80) = {
  limit : 79 .. 64,
  base_addr : 63 .. 0
}
bitfield code_segment_descriptorbits : bits(64) = {
  base31_24 : 63 .. 56,
  g : 55 .. 55,
  d : 54 .. 54,
  l : 53 .. 53,
  avl : 52 .. 52,
  limit19_16 : 51 .. 48,
  p : 47 .. 47,
  dpl : 46 .. 45,
  s : 44 .. 44,
  msb_of_type : 43 .. 43,
  c : 42 .. 42,
  r : 41 .. 41,
  a : 40 .. 40,
  base23_16 : 39 .. 32,
  base15_0 : 31 .. 16,
  limit15_0 : 15 .. 0
}
bitfield code_segment_descriptor_attributesbits : bits(16) = {
  unknownbits : 15 .. 12,
  g : 11 .. 11,
  d : 10 .. 10,
  l : 9 .. 9,
  avl : 8 .. 8,
  p : 7 .. 7,
  dpl : 6 .. 5,
  s : 4 .. 4,
  msb_of_type : 3 .. 3,
  c : 2 .. 2,
  r : 1 .. 1,
  a : 0 .. 0
}
bitfield data_segment_descriptorbits : bits(64) = {
  base31_24 : 63 .. 56,
  g : 55 .. 55,
  d_b : 54 .. 54,
  l : 53 .. 53,
  avl : 52 .. 52,
  limit19_16 : 51 .. 48,
  p : 47 .. 47,
  dpl : 46 .. 45,
  s : 44 .. 44,
  msb_of_type : 43 .. 43,
  e : 42 .. 42,
  w : 41 .. 41,
  a : 40 .. 40,
  base23_16 : 39 .. 32,
  base15_0 : 31 .. 16,
  limit15_0 : 15 .. 0
}
bitfield data_segment_descriptor_attributesbits : bits(16) = {
  unknownbits : 15 .. 12,
  g : 11 .. 11,
  d_b : 10 .. 10,
  l : 9 .. 9,
  avl : 8 .. 8,
  p : 7 .. 7,
  dpl : 6 .. 5,
  s : 4 .. 4,
  msb_of_type : 3 .. 3,
  e : 2 .. 2,
  w : 1 .. 1,
  a : 0 .. 0
}
bitfield system_segment_descriptorbits : bits(128) = {
  res3 : 127 .. 109,
  all_zeroes? : 108 .. 104,
  res2 : 103 .. 96,
  base63_32 : 95 .. 64,
  base31_24 : 63 .. 56,
  g : 55 .. 55,
  res1 : 54 .. 53,
  avl : 52 .. 52,
  limit19_16 : 51 .. 48,
  p : 47 .. 47,
  dpl : 46 .. 45,
  s : 44 .. 44,
  sailtype : 43 .. 40,
  base23_16 : 39 .. 32,
  base15_0 : 31 .. 16,
  limit15_0 : 15 .. 0
}
bitfield system_segment_descriptor_attributesbits : bits(16) = {
  unknownbits : 15 .. 10,
  g : 9 .. 9,
  avl : 8 .. 8,
  p : 7 .. 7,
  dpl : 6 .. 5,
  s : 4 .. 4,
  sailtype : 3 .. 0
}
bitfield call_gate_descriptorbits : bits(128) = {
  res3 : 127 .. 109,
  all_zeroes? : 108 .. 104,
  res2 : 103 .. 96,
  offset63_32 : 95 .. 64,
  offset31_16 : 63 .. 48,
  p : 47 .. 47,
  dpl : 46 .. 45,
  s : 44 .. 44,
  sailtype : 43 .. 40,
  res1 : 39 .. 32,
  selector : 31 .. 16,
  offset15_0 : 15 .. 0
}
bitfield call_gate_descriptor_attributesbits : bits(16) = {
  unknownbits : 15 .. 8,
  p : 7 .. 7,
  dpl : 6 .. 5,
  s : 4 .. 4,
  sailtype : 3 .. 0
}
$endif
