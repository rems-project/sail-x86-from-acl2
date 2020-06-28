// roughly based on the popcount example program found at:
// http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index-seo.php/X86ISA____PROGRAM-EXECUTION

#include <stdio.h>
#include <stdlib.h>

int add1 (unsigned int v) {
  unsigned int w = v + 1;
  return(w);
}

// Version of main as per the original ACL2 example
int main (int argc, char *argv[], char *env[]) {
  int v;
  printf ("\nEnter a 32-bit number: ");
  scanf  ("%d", &v);
  v = v & 0xffffffff;
  printf ("\nIncrement of %d is: %d\n", v, add1(v));
  return 0;
}