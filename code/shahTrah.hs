module ShahTrah where
	riceCorn = zipWith (^) ([2,2..])([0,1..])
	nthSquare n = last (take n riceCorn) 