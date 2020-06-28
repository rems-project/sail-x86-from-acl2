# Remove the following files from folders with a test.s present:
# instrument.sail
# scriptTemplate.sailscript
# emulator.c
# sailOut.log

folder=${KFSIT}
instruments=$(find ${folder} -type f -iname "test.s")

parallel 'rm -f {//}/a.out' ::: ${instruments}
parallel 'rm -f {//}/a.diself' ::: ${instruments}

parallel 'rm -f {//}/runSail.sh' ::: ${instruments}

parallel 'rm -f {//}/sailOut.log' ::: ${instruments}
parallel 'rm -f {//}/sail_coverage' ::: ${instruments}

parallel 'rm -f {//}/runACL2.lisp' ::: ${instruments}

parallel 'rm -f {//}/acl2Out.log' ::: ${instruments}
parallel 'rm -f {//}/acl2-instrument.log' ::: ${instruments}

parallel 'rm -f {//}/analyseOut.log' ::: ${instruments}