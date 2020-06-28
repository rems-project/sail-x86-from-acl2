set -e

# Find the runACL2.lisp files, excluding some rep folders
folder=${KFSIT}
SFILES=$(find ${folder} -type f -iname "runACL2.lisp" ! -path '*rep_mov*')

echo $SFILES

parallel --lb 'echo {#}; echo {//}; cd {//}; ACL2=${ACL2}; cat runACL2.lisp | $ACL2 > acl2Out.log' ::: ${SFILES}