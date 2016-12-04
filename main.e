note
	description: "GC_performance application root class"
	date: "$Date$"
	revision: "$Revision$"

class
	MAIN

inherit
	ARGUMENTS

create
	make

feature createMessage(n: INTEGER): ARRAY[INTEGER_8]
	local
		msg: ARRAY[INTEGER_8]
	do
		create msg.make_filled(n.as_integer_8, 0, msgSize-1)
		Result:= msg
	end

feature pushMessage(map: HASH_TABLE[ARRAY[INTEGER_8],INTEGER]; id: INTEGER)
	local
		lowId : INTEGER
	do
		lowId := id - windowSize
		map.put (createMessage(id), id)
		if lowId>=0 then
			map.remove (lowId)
		end
	end

feature {NONE} -- Initialization
	windowSize : INTEGER = 200_000
	msgCount : INTEGER = 1_000_000
	msgSize : INTEGER = 1024

	make
	-- Run application.
	local

		map : HASH_TABLE[ARRAY[INTEGER_8],INTEGER]
		i : INTEGER
		currentTime, lastTime: TIME
		iterPause, maxPause: TIME_DURATION
	do
		create map.make(0)

		create lastTime.make_now
		create maxPause.make_by_fine_seconds (0.0)

		from i:=0
		until i=msgCount
		loop
			pushMessage(map,i)
			create currentTime.make_now
			iterPause:= currentTime.relative_duration (lastTime)
			if iterPause>maxPause then
				maxPause:= iterPause
			end
			lastTime:= currentTime
			i:= i + 1
		end
		print("Max pause: ")
		print (maxPause.fine_second*1000.0)
		print (" ms %N")
	end

end
