# Collect `sail_coverage` files
folder=${KFSIT}
covFiles=$(find ${folder} -type f -iname "sail_coverage")

# Concatenate their contents into a single file
rm -f total_sail_coverage
cat ${covFiles} > total_sail_coverage
cd coverage_html
${SAIL_DIR}/sailcov/sailcov -a ${X86EMU_DIR}/all_branches -t ../total_sail_coverage ${X86SAIL_DIR}/*.sail > report.txt

# Print the report
cat report.txt
