import std.stdio;
import core.time;

const int windowSize = 200_000;
const int msgCount = 1_000_000;
const int msgSize = 1024;

byte[] createMessage(in int n){
	byte[] msg = new byte[msgSize];
        for (int i = 0; i < msgSize; i++)
            msg[i] = cast(byte) (i % 256);
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
        Duration maxPause = Duration.zero;
        auto lastTime = MonoTime.currTime();

	for(int i = 0; i < msgCount; i++){
		pushMessage(map, i);
                auto currentTime = MonoTime.currTime();
                Duration iterPause = currentTime - lastTime;
                if (iterPause > maxPause) maxPause = iterPause;
                lastTime = currentTime;
	}
        writeln(maxPause);
}
