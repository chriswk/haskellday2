module WhereExample where
	cubeAll list = map cube list
		where cube x = x * x * x