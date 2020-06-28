set -e

# Find runSail.sh files excluding some rep folders
folder=${KFSIT}
SFILES=$(find ${folder} -type f -iname "runSail.sh" ! -path '*rep_mov*')

# Use --dryrun to see what command will be run
# {//} gives us the directory
# --ungroup will print Sail's stderr output (I think) and will give us an indication that something is happening.  Order will likely be mangled though.
# --lb (linebuffer) is similar but will allow us to see the current index
# parallel cheat sheet: https://www.gnu.org/software/parallel/parallel_cheat.pdf
# We could use `-j 4` but parallel detects this automaticall for us
parallel --lb 'echo {#}; echo {//}; cd {//}; rm -f sail_coverage; ./runSail.sh > sailOut.log' ::: ${SFILES}