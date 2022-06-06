import sys
import os

'''
This file configures file paths for locating the ACL2 model, setting the
output directory etc. and some general options.
'''


###############################################################################
# File paths and options
###############################################################################


'''
Top level file name to translate.  Likely `x86.lisp` (which is located at
`books/projects/x86isa/machine`).
'''
x86_project_folder = '/local/scratch/tb592/opt/acl2/books/projects/x86isa/'
translateFile = 'x86.lisp'
translatePath = os.path.join(x86_project_folder, 'machine', translateFile)


'''
Path to the ACL2 executable, which is likely called `saved_acl2`.
Port on localhost the ACL2 instance listens on for expansion requests.
'''
acl2Process = '/local/scratch/tb592/opt/acl2/saved_acl2'
acl2Port = 1159


'''
Output folder for translated files.
'''
outputFolder = '/auto/homes/tb592/REMS/sail-x86-from-acl2/model'


'''
Output path to the translation of `utilities.lisp`, which is handled manually
by `generateUtils.py`.
'''
utilitiesFile = os.path.join(outputFolder, "utils.sail")


'''
Output paths for file which log data about the type resolution algorithm
'''
unresolvedTypesFile = '/auto/homes/tb592/REMS/sail-x86-from-acl2/unresolvedTypes.txt'


'''
`mbe` forms allow us to choose between translating the `:logic` or `:exec`
branches.  See tr_mbe in specialTokens.py for more information or:
http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/index.html?topic=ACL2____MBE
'''
mbe_branch = ':logic'
#mbe_branch = ':exec'


'''
The Lisp form `the` translates to a dynamic type check in Sail.  Many `the`
forms use very big numbers to denote the range in which there argument must
lie, and this clutters up the code.  In the absence of the numbers being
shortened (e.g. by calculating the value or assigning them a name) it is
possible to skip out the dynamic type check completely.  This is because the
types they specify are proved statically by ACL2.  It does, however, require
correct implementation of the handwritten functions - so take care if setting
this to False.
'''
translate_the = True


'''
Printing interactions with the running ACL2 instance can increased the amount
of terminal output substantially but help debug problems. 
'''
print_acl2_interactions = False


'''
Check all necessary paths have been specified and mbe_branch is well-formed.
'''
if any([path is None for path in [acl2Process, outputFolder, unresolvedTypesFile, utilitiesFile]]):
	sys.exit("Please specify file paths in config_files.py")

if mbe_branch not in [':logic', ':exec']:
	sys.exit(f"configuration.py: mbe_branch should be either ':logic' or ':exec', not {mbe_branch}")



