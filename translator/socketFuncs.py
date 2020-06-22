import socket

"""
Uses format:
	- 6 bytes of length
	- Payload of length bytes
"""

def reliableSend(toSend, s):
	'''
	Args:
		- toSend : str
		- s : socket
	'''
	toSend = "{:06d}{}".format(len(toSend), toSend)
	toSend = bytes(toSend, "utf-8")
	# print(f"Sending this: {toSend}")

	sent = 0
	numToSend = len(toSend)
	while sent < numToSend:
		thisSent = s.send(toSend[sent:])
		# print(f"thisSent: {thisSent}")
		sent += thisSent

def reliableRecv(s):
	'''
	Args:
		- s : socket
	Returns:
		- [bytes] - [] if there was an error
	'''
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