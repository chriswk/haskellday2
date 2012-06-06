module Exercise4 where
	every3rd x = [x, (x + 3) ..]
	every5th y = [y, (y + 5) ..]
	every8th x y = (zipWith (+) (every3rd x) (every5th y))