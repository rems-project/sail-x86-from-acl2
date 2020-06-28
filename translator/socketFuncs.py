import socket

"""
Low level functions for sending and receiving data to/from the ACL2 instance
via sockets.  Roughly follows the techniques from here:
https://docs.python.org/3/howto/sockets.html.

Message structure is:
	- 6 bytes of length.  6 bytes seems enough to cope with longest response
	  from ACL2 instance (although 5 bytes is not).
	- Payload of length bytes
"""

def reliableSend(toSend, s):
	"""
	Args:
		- toSend : str
		- s : socket
	"""
	toSend = "{:06d}{}".format(len(toSend), toSend)
	toSend = bytes(toSend, "utf-8")

	sent = 0
	numToSend = len(toSend)
	while sent < numToSend:
		thisSent = s.send(toSend[sent:])
		sent += thisSent

def reliableRecv(s):
	"""
	Args:
		- s : socket
	Returns:
		- [bytes] - [] if there was an error
	"""
	# Read the length of the packet
	recvd = 0
	lengthBytes = []
	while recvd < 6:
		thisRecvd = s.recv(6 - recvd)
		if thisRecvd == b'':
			s.close()
			return []
		lengthBytes.append(thisRecvd)
		recvd += len(thisRecvd)

	# Convert the read length to an integer
	length = 0
	lengthString = ''.join([b.decode("utf-8") for b in lengthBytes])
	try:
		length = int(lengthString)
	except:
		print(f"Error: length was not an integer - {length}")
		s.close()
		return []

	# Read the payload
	recvd = 0
	buff = []
	while recvd < length:
		thisRecvd = s.recv(length - recvd)
		buff.append(thisRecvd)
		if thisRecvd == b'':
			s.close()
			return
		recvd += len(thisRecvd)

	return buff