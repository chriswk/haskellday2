module Prime where
	foldi	:: (a -> a -> a) -> a -> [a] -> a
	foldi f z []	= z
	foldi f z (x:xs)	= f x (foldi f z (pairs f xs))
	
	pairs	:: (a -> a -> a) -> [a] -> [a]
	pairs f (x:y:t)	= f x y : pairs f t
	pairs f t	= t
	
	minus (x:xs) (y:ys) = case (compare x y) of
		LT -> x : minus	xs	(y:ys)
		EQ -> 	  minus	xs	ys
		GT -> 	minus (x:xs) ys
	minus	xs	_	= xs
	
	union (x:xs) (y:ys) = case (compare x y) of
		LT -> x : union	xs	(y:ys)
		EQ -> x : union	xs	ys
		GT -> y : union (x:xs) 	ys
	union xs	[] = xs
	union []	ys = ys 
		
	primes = 2 : 3 : ([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes])
		where unionAll s = foldi (\(x:xs) ys -> x : union xs ys) [] s