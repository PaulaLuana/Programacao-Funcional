{- Números abundantes são números natural  que a soma dos seus divisores próprios é maior do que ele mesmo. Por exemplo, os divisores próprios de 12 são 1, 2, 3, 4 e 6. A soma dos divisores é 16. Logo, 12 é um número abundante. Por outro lado, o número 5 não é abundante uma vez que ele possui apenas um divisor próprio que é 1.

		abundante 12 == True
		abundante   5 == False		-}

abundante n = sum(divisores n) > n
divisores n1 = [ x | x <- [1..n1-1], n1 `mod` x == 0] 

{- Defina a função abundantesMenores tal que (abundantesMenores n) devolve uma lista de números abundantes menores ou igual a n.

abundantesMenores 50 == [12,18,20,24,30,36,40,42,48]
abundantesMenores 100 == [12,18,20,24,30,36,40,42,48,54,56,60,66,70,72,78,80,84,88,90,96,100]
		-}

abundantesMenores n = [x | x<-[1..n], abundante x == True]

{- Defina a função capitalises que recebe uma lista de caracteres e devolve a uma lista substituindo as letras minúsculas por maiúsculas.

	capitalises "Minority Report" == "MINORITY REPORT"

Dica 1: use a função toUpper
Dica 2: Não esqueça de colocar import Data.Char na primeira linha do arquivo .hs para utilizar a função toUpper do módulo Data.Char		-}

import Data.Char
capitalises xs = map (toUpper) xs

{- Defina a função frequencia :: a -> [a] -> Int tal que (frequencia x xs) devolve o número de ocorrências de x em u. Por exemplo,

	frequencia 2 [1,2,3,2,4] == 2
	frequencia 2 [2,1,2,3,2,4,2] == 4

Considere os seguintes casos:
frequencia a [] =
frequencia a (x:xs)
   | x == a
	| otherwise =		-}

frequencia a [] = 0
frequencia a (x:xs)
   | x == a = 1 + frequencia a xs
   | otherwise = 0 + frequencia a xs

{- Defina uma função inserir :: Ord a => a -> [a] -> [a] tal que (inserir x xs) recebe um elemento x e uma lista ordenada de maneira crescente  devolvendo uma lista ordenada ascendentemente, oriunda da inserção apropriada de x em xs. Por exemplo,

	inserir 3 [2,7,12] == [2,3,7,12]

Dica: Considere os seguintes casos:

		inserir x [] =
		inserir x (y:ys) = 
-}


inserir x [] = [x]
inserir x (y:ys) = if x<=y then x:y:ys else y:inserir x ys

{- Defina a função interseccao :: Eq a => [a] -> [a] -> [a] tal que (interseccao xs ys) devolve a intersecçao dos conjuntos xs e ys seguindo a ordem em que eles aparecem na lista xs. 
Por exemplo,
	interseccao [ 3 , 2 , 5 ] [ 5 , 7 , 3 , 4 ] == [ 3 , 5 ]
	interseccao [ 3 , 2 , 6] [ 5 , 7 , 3 , 4 ] == [ 3  ]
	interseccao [ 8 , 2 , 6] [ 5 , 7 , 3 , 4 ] == [  ]
	interseccao [ 3 , 1 , 4, 6] [ 6, 3, 1, 9 ] == [ 3, 1, 6 ]		-}

interseccao xs1 xs2 = [x | x <- xs1, x `elem` xs2]

{- Defina a função merge :: [a] -> [a] -> [a] tal que (merge xs ys) é uma lista ordenada obtida pela entrelaçamento de duas listas ordenadas xs e ys. Por exemplo,

		merge [2,5,6] [1,3,4] == [1,2,3,4,5,6]
		merge [3,5,6, 8] [1,3,4,5] == [1,3,3,4,5,5,6,8]

Dica: Defina os seguintes casos:

merge xs [] =
merge [] ys =

merge (x:xs) (y:ys)
	| x <= y = 
	| otherwise =		-}

merge xs [] = xs
merge [] ys = ys

merge (x:xs) (y:ys)
   | x <= y = x:merge xs (y:ys)
   | otherwise = y:merge (x:xs) ys

{- Defina uma função remove :: Eq a => a -> [a] -> [a] tal que (remove x xs) devolve uma lista obtida removendo uma ocorrências de x em xs, caso ela exista. Por exemplo,

		remove 2 [2,3,4,5] == [3,4,5]
		remove 2 [1,3,2,4] == [1,3,4]		-}


remove a [] = []
remove a (x:xs)
   | x == a = remove a xs
   | otherwise =  (x:remove a xs)

{- Defina a função somaConsecutivos tal que (somaConsecutivos xs) é a soma dos pares de elementos consecutivos de uma lista xs. Por exemplo, 

		somaConsecutivos [3,1,5,2] == [4,6,7]
		somaConsecutivos [3] == []		-}

pares xs = zip xs (tail xs)
somaConsecutivos xs = [x + y | (x,y) <- pares xs]

{- Defina a função somaMultiplos :: [Integer] -> Integer -> Integer tal que somaMultiplos  xs    n devolve a soma de todos  os múltiplos únicos de algum número da lista  xs  estritamente  menores que  n . Por exemplo, todos os números naturais abaixo de 20, que são múltiplos de 3 ou 5, obtemos 3, 5, 6, 9, 10, 12, 15 e 18. A soma desses múltiplos é 78. 

somaMultiplos [3,5] 20 == 78-}

unique [] = []
unique (x:xs) = if x `elem` xs then unique xs else x:unique xs
multiplos xs n = [x1 | x2 <- xs, x1 <- [1..(n-1)], x1 `mod` x2 == 0]
somaMultiplos xs n = sum(unique (multiplos xs n))

{-	Defina uma função subconjunto :: Eq a => [a] -> [a] -> Bool tal que (subconjunto xs ys) verifica xs é um subconjunto de ys. Por exemplo,

	subconjunto [ 3 , 2 , 3 ] [ 2 , 5 , 3 , 5 ] == True
	subconjunto [ 3 , 2 , 3 ] [ 2 , 5 , 6 , 5 ] == F a l s e

Defina os seguintes casos:

	subconjunto []      ys = 
	subconjunto (x:xs) ys =

Dica: use a função elem do Prelude.		-}


subconjunto []      ys = True
subconjunto (x:xs) ys = if elem x ys == True then subconjunto xs ys else False


{- Escreva uma função todosPrefixos tal que (todosPrefixos xs) devolve uma lista formada com todos os prefixos da lista xs. Um prefixo de uma lista xs é qualquer sequência inicial de elementos da lista xs. Por exemplo,

todosPrefixos [2,3,5,6,7,8] = [[],[2],[2,3],[2,3,5],[2,3,5,6],[2,3,5,6,7],[2,3,5,6,7,8]]
todosPrefixos [2,3,1,5] = [[],[2],[2,3],[2,3,1],[2,3,1,5]]		-}


todosPrefixos xs = [take i xs| i <- [0..n]]
   where n = length xs

{- Escreva uma função todosSufixos tal que (todosSufixos xs) devolve uma lista formada com todos os sufixos da lista xs. Um sufixo de uma lista xs é qualquer sequência final de elementos da lista xs. Por exemplo,

todosSufixos [2,3,5,6,7,8] = [[],[8],[7,8],[6,7,8],[5,6,7,8],[3,5,6,7,8],[2,3,5,6,7,8]]
todosSufixos [2,3,1,5] = [[],[5],[1,5],[3,1,5],[2,3,1,5]]
todosSufixos [2,3,5,6,7,8,5,6,1,2,8,9] = [[],[9],[8,9],[2,8,9],[1,2,8,9],[6,1,2,8,9],[5,6,1,2,8,9],[8,5,6,1,2,8,9],[7,8,5,6,1,2,8,9],[6,7,8,5,6,1,2,8,9],[5,6,7,8,5,6,1,2,8,9],[3,5,6,7,8,5,6,1,2,8,9],[2,3,5,6,7,8,5,6,1,2,8,9]]

Dica: use a função final.		-}


todosSufixos xs = [drop i xs | i <- [n,(n-1)..0]]
   where n = length xs
