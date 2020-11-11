from time import process_time

windowSize = 200000
msgCount = 1000000
msgSize = 1024

def createMessage(n):
	return bytearray([n%256 for i in range(msgSize)])
	
def pushMessage(channel, id):
	channel[id%windowSize] = createMessage(id)

#Main
channel = [None] * windowSize
maxPause = 0.0
lastTime = process_time()
for i in range(msgCount):
	pushMessage(channel, i)
	currentTime = process_time()
	iterPause = currentTime - lastTime
	if iterPause > maxPause:
		maxPause = iterPause
	lastTime = currentTime
	
print('Max Pause: '+"{0:.3f}".format(maxPause*1000.0)+' ms')
