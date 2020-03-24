{-	Defina a função final tal que (final xs) é uma lista formada pelos n elementos finais de xs. Por exemplo,
	final 3 [2,5,4,7,9,6] == [7,9,6]
	final 2 [2,5,4,7,9,6] == [9,6]
	final 1 [2,5,4,7,9,6] == [6]						-}

final n xs = reverse(take n(reverse xs))

{-	Joãozinho acaba de mudar de escola e a primeira coisa que percebeu na nova escola é que a gangorra  do parquinho não é simétrica, uma das extremidades é mais longa que a outra. Após brincar algumas vezes com um amigo  de mesmo peso, ele percebeu que quando está em uma extremidade, a gangorra se desequilibra para o lado dele (ou seja, ele fica na parte de baixo, e o amigo na parte de cima), mas quando eles trocam de lado, a gangorra se desequilibra para o lado do amigo.

Sem entender a situação, Joãozinho pediu ajuda a outro amigo de outra série, que explicou que o comprimento do lado interfere no equilíbrio da gangorra, pois a gangorra estará equilibrada quando P1 * C1 = P2 * C2 onde P1 e P2 são os pesos da criança no lado esquerdo e direito, respectivamente, e C1 e C2 são os comprimentos da gangorra do  lado esquerdo e direito, respectivamente.

Escreva uma função gangorra que recebe quatro inteiros P1, C1, P2, C2, que são respectivamente, o peso da criança e o comprimento da  gangorra do lado esquerdo e o o peso da criança e o comprimento da gangorra do lado direito.Se a gangorra estiver equilibrada, a função gangorra  devolve 0. Se ela estiver desequilibrada de modo que a criança esquerda esteja na parte de baixo, a função gangorra devolve -1, senão, devolve 1.

Por exemplo,
		gangorra 30 100 60 50 == 0
		gangorra 40 40 38 60 == 1							-}

gangorra p1 c1 p2 c2  
    | (p1 * c1) == (p2 * c2) = 0  
    | (p1 * c1) > (p2 * c2) = -1 
    | otherwise   = 1

{- 	Dado três valores a, b e c, escreva uma função iguais3 que retorne quantos dos três são iguais. A resposta pode ser 3 (todos iguais), 2 (dois iguais) ou 0 (todos diferentes)		-}

iguais3 a b c  
    | a == b && b == c = 3  
    | (a == b && b /= c) || (a /= b && a == c) || (a /= b && b == c) = 2 
    | otherwise   = 0

{- Defina a função interior tal que (interior xs) é uma lista obtida eliminando os extremos da lista xs. Por exemplo, interior [2,5,3,7,3] == [5,3,7]-}

interior list = tail (init list) 

{- 	Defina a função max3 tal que (max3 x y z) é o máximo entre x, y e z. 
	Por exemplo, 
		max3 6 2 4 == 6
		max3 6 7 4 == 7
		max3 6 7 9 == 9						-}

max3 x y z = max x (max y z)

{-	Defina a função neglist xs que computa o número de elementos negativos em uma lista xs.

	neglist [ 1 , 2 , 3 , 4 , 5 ] == 0
	neglist [1 , −3 , −4 ,3 ,4 , −5] == 3
	Dica: usa a função filter e length.						-}

neglist xs = length [x | x <- xs, x < 0]

{- Escreva uma função soma que recebe dois parâmetros e devolve a soma dos dois parâmetros. -}

soma x y = x + y

{-	Defina uma função somaImpares tal que (somaImpares xs) devolve a soma dos elementos 		ímpares de uma lista.

	somaImpares [2,3,1,5] == 9
	somaImpares [1,1,4,2] == 2

	Dica: a função odd, filter.	-}

somaImpares xs = sum [x | x <- xs, odd x] 
