module MachinPi where
	arccot x unity =
		arccot' x unity 0 start 1 1
			where start = unity `div` x
				arccot' x unity sum xpower n sign | xpower `div` n == 0 = sum
												  | otherwise			=
					arccot' x unity (sum + sign*term) (xpower `div` (x*)) (n+2) (-sign)
						where term = xpower `div` n
						
	machin_pi digits = pi' `div` (10 ^ 10)
		where unity = 10 ^ (digits+10)
			pi' = 4 * (4 * arccot 5 unity - arccot 239 unity)