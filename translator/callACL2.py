import socketFuncs
import argparse

import subprocess
import socket
import tomli

'''
The translator requires a running ACL2 instance so macros can be expanded at
translate time.  The functions here manage that process.

Running `python3 callACL2.py` will start an ACL2 process and listen on port
defined in the configuration file for strings to send to it.  Starting the ACL2
process and loading the x86isa project books may take a bit of time - the
server is ready to receive requests after it prints 'Ready'.
'''


def initialiseInternal(acl2Process):
	"""
	Starts the ACL2 process, loads the x86isa project and changes to the
	X86ISA package.  This may take some time.

	Args:
		- None
	Returns:
		- acl2 : subprocess
	"""
	acl2 = subprocess.Popen(acl2Process,
                        shell=True,
                        stdin=subprocess.PIPE,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE
                        )
	print("Waiting for prompt")
	waitForPrompt(acl2)
	resp = interactFromPrompt(b'(include-book "projects/x86isa/tools/execution/top" :ttags :all :dir :system)', acl2)
	print(resp)
	resp = interactFromPrompt(b'(in-package "X86ISA")', acl2)
	print(resp)

	return acl2

def initialiseExternal(acl2Port):
	"""
	Set up the socket on which we will receive incoming requests.

	Args:
		- None
	Returns:
		- s : socket
	"""
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.bind(('localhost', acl2Port))

	return s

def waitForPrompt(acl2):
	"""
	We assume the character sequence '!>` indicates the acl2 prompt and that
	it is ready to receive input.

	We must read a single byte at a time from the file-like subprocess object.
	If we do not, the internal buffering may block us forever.

	Args:
		- acl2 : subprocess
	Returns:
		- None (but waits until at prompt until before doing so)
	"""
	circBuff = [None, None]
	index = 0
	prompt = False
	while not prompt:
		circBuff[index] = acl2.stdout.read(1)
		# print(circBuff[index].decode("utf-8"), end='')
		index = (index + 1) % 2
		prompt = (circBuff == [b'!', b'>']) or (circBuff == [b'>', b'!'])

def interactFromPrompt(command, acl2):
	"""
	Sends a command to the acl2 subprocess and gathers the response.

	As in `waitForPrompt`:
		a) we assume the character sequence '!>' indicates the prompt.
		b) we must only read 1 byte at a time from acl2's stdout

	Args:
		- command : bytes
		- acl2 : socket
	Returns:
		- str
	"""
	# Send command then a newline
	print(f"Sending: {command}")
	acl2.stdin.write(command)
	acl2.stdin.write(b'\n')
	acl2.stdin.flush()

	# Collect output and wait for prompt
	print("Waiting for prompt")
	buff = []
	prompt = False
	while not prompt:
		item = acl2.stdout.read(1)
		buff.append(item)
		prompt = buff[-2:] == [b'!', b'>']

	# Return everything but the prompt
	try:
		print(acl2.stdout.encoding)
	except Exception as e:
		pass
	buff = [b.decode("utf-8") for b in buff] # May be able to use sys.stdout.encoding or acl2.stdout.encoding
	everything = ''.join(buff)
	lines = everything.split('\n')
	toReturn = '\n'.join(lines[:-1])
	return toReturn

def handleRequest(s, acl2):
	"""
	Take a request from the socket, forward it to the acl2 subprocess, gather
	the result and send it back to the socket.

	Args:
		- s : socket
		- acl2 : subprocess
	Returns:
		- Bool (True = sucess, False = failure (probably socket close))
	Effects:
		- Interacts with socket and subprocess
	"""

	# Receive a command from the socket
	buff = socketFuncs.reliableRecv(s)
	print(f"Buff: {buff}")
	if buff == []: return False

	# Convert the buffer: [bytes] -> [string] -> string -> bytes
	buff = [b.decode("utf-8") for b in buff]
	buff = ''.join(buff)
	buff = bytes(buff, "utf-8")

	# Send the request to the ACL2 server
	response = interactFromPrompt(buff, acl2)
	print(f"Response:\n{response}")

	# Send response back
	socketFuncs.reliableSend(response, s)

	return True

def handleManyReqs(s, acl2):
	"""
	Wrapper for handleRequest().  Processes requests from the socket and sends
	the results back until failure.

	Args:
		- s : socket
		- acl2 : subprocess
	Returns:
		- None
	Effects:
		- Interacts with socket and subprocess
	"""
	while True:
		success = handleRequest(s, acl2)
		if not success:
			s.close()
			return

def cleanup(s, acl2):
	s.close()
	try:
		acl2.communicate(input=b'(quit)\n', timeout=5)
	except subprocess.TimeoutExpired:
		acl2.kill()

if __name__ == '__main__':
	"""
	Start the acl2 server.
	"""

	parser = argparse.ArgumentParser(description='Translate ACL2 files into Sail')
	parser.add_argument('-c', '--config', default="config.toml")
	args = parser.parse_args()

	# Load config
	with open(args.config, "rb") as f:
		config = tomli.load(f)

	acl2Process = config.get('acl2_process', 'acl2')
	acl2Port = config.get('acl2_port', 1159)

	# Initialise the acl2 subprocess
	print('Initialising (this may take some time)')
	acl2 = initialiseInternal(acl2Process)

	# An example call to acl2
	resp = interactFromPrompt(b'(cf-spec-gen-fn 8)', acl2)
	print(resp)
	print('Ready')

	# External interface.  Only allow a single connection at once.
	try:
		s = initialiseExternal(acl2Port)
		s.listen(1)
		while True:
			(clientsocket, address) = s.accept()
			handleManyReqs(clientsocket, acl2)
	except KeyboardInterrupt:
		pass
	finally:
		cleanup(s, acl2)
