const int windowSize = 200_000;
const int msgCount = 1_000_000;
const int msgSize = 1024;

byte[] createMessage(in int n){		
	byte[] msg = new byte[msgSize];
	msg[] = cast(byte) n;	
	return msg;
}

void pushMessage(byte[][int] map, in int id){
	const lowId = id - windowSize;
	map[id] = createMessage(id);
	if (lowId >=0) {
		map.remove(lowId);
	}
}

void main(){
	byte[][int] map;
	for(int i = 0; i < msgCount; i++){
		pushMessage(map, i);
	}
}
