{- 	Dado três valores a, b e c, escreva uma função iguais3 que retorne quantos dos três são iguais. A resposta pode ser 3 (todos iguais), 2 (dois iguais) ou 0 (todos diferentes)		-}

iguais3 a b c  
    | a == b && b == c = 3  
    | (a == b && b /= c) || (a /= b && a == c) || (a /= b && b == c) = 2 
    | otherwise   = 0
