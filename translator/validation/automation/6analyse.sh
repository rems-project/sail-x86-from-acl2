set -e

# Find folders containing .s but check if the necessary log files are actually there in Python
folder=${KFSIT}
SFILES=$(find ${folder} -type f -iname "*.s")

parallel --lb 'echo {#}; python3 analyse.py {//}' ::: ${SFILES}