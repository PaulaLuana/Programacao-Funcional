{-	Defina a função agrupa :: Eq a => [[a]] -> [[a]] tal que (agrupa xss) é uma lista de listas obtidas agrupando os primeiros elementos, os segundos elementos, de forma que o comprimento das listas dos resultados seja igual a lista mais curta de xss. Por exemplo,

	agrupa [[1..6],[7..9],[10..20]] == [[1,7,10],[2,8,11],[3,9,12]]
	agrupa [ ] == [ ]

Dica: use a função map, head e tail.		-}

temVazio xss = [] `elem` xss
cabecas xss = map (\x -> head x) xss
recorte xss = map (\x -> drop 1 x) xss
agrupa [] = []
agrupa xss = if (temVazio xss) == True then [] else [cabecas xss] ++ agrupa (recorte xss)

{- Defina a função descompacta :: :: [(a, b)] -> ([a], [b]) que transforma uma lista de pares ordenado em um par ordenado onde o primeiro elemento é uma lista dos primeiros componentes dos pares ordenados e o segundo elemento é uma lista dos segundos componentes dos pares ordenados.

	descompacta [(1,2),(3,4),(5,6),(4,5)] == ([1,3,5,4],[2,4,6,5])
	descompacta [(1,2),(3,4),(5,6),(4,5),(5,6)] == ([1,3,5,4,5],[2,4,6,5,6])		-}

descompacta xs = (descompactap xs, descompactas xs)
descompactap xs = map primeiro xs
descompactas xs = map segundo xs
primeiro (a,b) = a
segundo (a,b) = b

{-	Escreva uma função filtrandoCaudas :: [[Int]] -> [[Int]] usando compreensão de listas tal que (caudas xss) devolve uma lista contendo a cauda das listas não vazias

		filtrandoCaudas [[2,3,5,6],[2,5,7,9],[4,5,6,7]] == [[3,5,6],[5,7,9],[5,6,7]]

Dica : use as funções head, tail, null.			-}

filtrandoCaudas xs = map caudas (semcauda xs)
semcauda xs = [x | x<-xs, x/=[]]
caudas [] = []
caudas xs = tail xs

{- Defina uma função filtrandoListas :: [[a]]->[[a]] tal que (filtrandoListas xss) devolve uma lista contendo o maior prefixo do mesmo tamanho de cada lista de xss.

filtrandoListas [[3,2,1],[3,4],[4,3,2,1]] == [[3,2],[3,4],[4,3]]		-}


filtrandoListas xs = map (\x -> take (tam xs) x) xs

tam xs = minimum (tam1 xs)
tam1 xs = map tam2 xs
tam2 x = length x

{- Defina a função intercala :: a -> [a] -> [a] que intercala um elemento entre valores consecutivos numa lista; se a lista tiver menos de dois valores deve ficar inalterada. Exemplos:

			intercala 0 [1..4] = [1,0,2,0,3,0,4]
			intercala ',' " abcd " = "a,b,c,d"
			intercala ',' "a" = "a"
			intercala 1 [ ] = []		-}

intercala x [] = []
intercala x (x1:xs)
   | (length xs) == 0 = [x1]
   | otherwise = [x1] ++ [x] ++ intercala x xs

{- Dado uma lista de números inteiros, definiremos o maior salto como o maior valor das diferenças (em valor absoluto) entre números consecutivos da lista. Por exemplo, dada uma lista [2,5,-3,7]
	
	3 (valor absoluto de 2 - 5)
	8 (valor absoluto de 5 - (-3))
	10 (valor absoluto de -3 - 7)
Portanto o maior salto é 10. Não esta definido o maior salto para uma lista com menos de 2 elementos.	Defina a função maiorSalto :: [Integer] -> Integer tal que maiorSalto xs é o maior salto da lista xs. Por exemplo,

		maiorSalto [1,5] == 4
		maiorSalto [10,-10,1,4,20,-2] == 22		-}

maiorSalto [x] = 0
maiorSalto [] = 0
maiorSalto (x1:x2:xs) = maximum[abs(x1-x2), maiorSalto (x2:xs)]

{- Usando apenas funções da Biblioteca Prelude, escreva a função metadePares :: [Integer] -> [Integer] tal que (metadePares xs) devolve a lista dos elementos que são pares de xs divididos por 2. Por exemplo,

	metadePares [0,-1,3,4,-16,3] == [0,2,-8]		-}

metadePares xs = [x `div` 2 | x<-xs, x `mod` 2 == 0]

{- Usando compreensão de listas,  Escreva a função noIntervalo :: Int -> Int -> [Int] -> [Int] tal que (noIntervalo a b xs) retorna todos os valores de xs que são maiores ou iguais a e menores ou iguais a a b. Por exemplo,

		noIntervalo 5 10 [1..15] == [5,6,7,8,9,10]
		noIntervalo 2 1 [1,2,3,4] == []
		noIntervalo 1 2 [1,2,2,3,4,1] == [1,2,2,1]		-}

noIntervalo a b xs = [x | x<- xs, x >= a && x <= b]

{-	 Num sorteio que distribui prêmios, um participante inicialmente sorteia uma lista de valores inteiros. O número de pontos do participante é o  tamanho da maior sequência de valores consecutivos iguais. Por exemplo, suponhamos que um participante sorteia 11 valores e, nesta ordem, os valores são [30, 30, 30, 30, 40, 40, 40, 40, 40, 30,30]. Então, o participante ganha 5 pontos, correspondentes aos 5 valores 40 consecutivos. 

Note que o participante sorteou 6 valores iguais a 30, mas nem todos são consecutivos.
Escreva uma função pontos :: [Int] -> Int tal que (pontos xs) devolve o número de pontos do participante considerando a lista de valores sorteados xs.

	pontos [30,30,30,40,40,40,40,40,30,30,30] == 5
	pontos [1,1,1,20,20,20,20,3,3,3,3,3,3,3] == 7

Dica: use a função takeWhile :: [a] -> (a->Bool)->[a] e dropWhile :: [a] -> (a->Bool)->[a]	-}

pontos [] = 0
pontos (x:xs) = maximum [(length(takeWhile (==x) xs))+1, (pontos (dropWhile (==x) xs))]

{-	A função remdups remove elementos duplicados adjacentes de uma lista.  Por exemplo,

		remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]	-}

remdups2 [] x = []
remdups2 (x1:v) x = if x1 /= x then [x1] ++ remdups2 v x1 else remdups2 v x

remdups (x:xs) = [x] ++ remdups2 xs x

{- Defina a função temLetraOuDigito :: String -> Bool, usando foldr, que recebe um argumento do tipo String e devolve True, se a string contém algum letra (minúscula ou maiúscula) ou algum dı́gito, e False, caso contrário.

Dica: Use as funções isLetter :: Char -> Bool e isDigit:: Char -> Bool importando o módulo Data.Char adicionando a seguinte instrução import Data.Char		-}

import Data.Char
temLetraOuDigito xss = if tem xss /= "" then True else False
tem xss = [x | x<-xss, isLetter x || isDigit x]


{- O enunciado da função a seguir está no arquivo lista4.pdf -}

chococola n = n + chococola' (n+1)
chococola' n = if n <= 2 then 0 else (n `div` 3) + chococola' (n `div` 3 + n `mod` 3)
