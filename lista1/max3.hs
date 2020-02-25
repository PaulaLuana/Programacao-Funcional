{- 	Defina a função max3 tal que (max3 x y z) é o máximo entre x, y e z. 
	Por exemplo, 
		max3 6 2 4 == 6
		max3 6 7 4 == 7
		max3 6 7 9 == 9						-}

max3 x y z = max x (max y z)
