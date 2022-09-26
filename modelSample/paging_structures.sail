$ifndef __X86_PAGING_STRUCTURES
$define __X86_PAGING_STRUCTURES
$include <prelude.sail>
$include <string.sail>
bitfield ia32e_page_tablesbits : bits(64) = {
  xd : 63 .. 63,
  res2 : 62 .. 52,
  reference_addr : 51 .. 12,
  res1 : 11 .. 8,
  ps : 7 .. 7,
  d : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pml4ebits : bits(64) = {
  xd : 63 .. 63,
  res3 : 62 .. 52,
  pdpt : 51 .. 12,
  res2 : 11 .. 8,
  ps : 7 .. 7,
  res1 : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pdpte_1gb_pagebits : bits(64) = {
  xd : 63 .. 63,
  res3 : 62 .. 52,
  page : 51 .. 30,
  res2 : 29 .. 13,
  pat : 12 .. 12,
  res1 : 11 .. 9,
  g : 8 .. 8,
  ps : 7 .. 7,
  d : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pdpte_pg_dirbits : bits(64) = {
  xd : 63 .. 63,
  res3 : 62 .. 52,
  pd : 51 .. 12,
  res2 : 11 .. 8,
  ps : 7 .. 7,
  res1 : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pde_2mb_pagebits : bits(64) = {
  xd : 63 .. 63,
  res3 : 62 .. 52,
  page : 51 .. 21,
  res2 : 20 .. 13,
  pat : 12 .. 12,
  res1 : 11 .. 9,
  g : 8 .. 8,
  ps : 7 .. 7,
  d : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pde_pg_tablebits : bits(64) = {
  xd : 63 .. 63,
  res3 : 62 .. 52,
  pt : 51 .. 12,
  res2 : 11 .. 8,
  ps : 7 .. 7,
  res1 : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
bitfield ia32e_pte_4k_pagebits : bits(64) = {
  xd : 63 .. 63,
  res2 : 62 .. 52,
  page : 51 .. 12,
  res1 : 11 .. 9,
  g : 8 .. 8,
  pat : 7 .. 7,
  d : 6 .. 6,
  a : 5 .. 5,
  pcd : 4 .. 4,
  pwt : 3 .. 3,
  u_s : 2 .. 2,
  r_w : 1 .. 1,
  p : 0 .. 0
}
$endif
