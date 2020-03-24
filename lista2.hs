{- Defina a função fact tal que (fact n) calcula o fatorial de n, ou seja, fact n = 1*2*...*n

fact 1 == 1
fact 2 == 2
fact 3 == 6
fact 4 == 24
fact 5 ==120

Use  a função product 	-}

fact x = product [1..x]

{- Dois times, Cormengo e Flaminthians, participam de um campeonato de futebol, juntamente com outros times. Cada vitória conta três pontos, cada empate um ponto. Fica melhor classificado no campeonato um time que tenha mais pontos. Em caso de empate no número de pontos, fica melhor classificado o time que tiver maior saldo de gols. Se o número de pontos e o saldo de gols forem os mesmos para os dois times então os dois times estão empatados no campeonato.

Escreva uma função campeonato que recebe seis inteiros cv , ce , cs , fv , fe e fs , que são, respectivamente, o número de vitórias do Cormengo, o número de empates do Cormengo, o saldo de gols do Cormengo, o número de vitórias do Flaminthians, o número de empates do Flaminthians e o saldo de gols do Flaminthians. Se Cormengo é melhor classificado que Flaminthians, a funcao classifica devolve a letra 'C', se Flaminthians é melhor classificado que Cormengo, a funcao classificacao devolve a letra 'F', e se os dois times estão empatados a funcao classificacao devolve apenas o caractere '='.

campeonato 10 5 18 11 1 18 == 'C'

campeonato 10 5 18 11 2 18 == '='

campeonato 9 5 -1 10 2 10 == 'F'-}

campeonato cv ce cs fv fe fs
   | ((cv*3)+ce > (fv*3) + fe) = 'C'
   | ((cv*3)+ce < (fv*3) + fe) = 'F'
   | ((cv*3)+ce == (fv*3) + fe) && (cs /= fs) = if cs>fs then 'C' else 'F'
   | otherwise = '='
{- Parte do treinamento de um novo garçom é carregar uma grande bandeja com várias latas de bebidas e copos e entregá-las todas numa mesa do restaurante. Durante o treinamento é comum que os garçons deixem cair as bandejas, quebrando todos os copos.

A Sociedade Brasileira de Copos (SBC) analisou estatísticas do treinamento de diversos garçons e descobriu que os garçons em treinamento deixam cair apenas bandejas que têm mais latas de bebidas que copos. Por exemplo, se uma bandeja tiver 10 latas e 4 copos, certamente o garçom em treinamento a deixará cair, quebrando os 4 copos. Já se a bandeja tiver 5 latas e 6 copos, ele conseguirá entregá-la sem deixar cair.

Escreva uma função coposQuebrados :: [(Int,Int)] -> Int tal que (coposQuebrados xs) devolve o total de copos quebrados por um garçom considerando uma lista de bandejas que o garçom tentou entregar. Cada bandeja é representada por uma tupla (L,C), onde L é o número de latas e C o número de copos. Por exemplo, 

coposQuebrados [(10,5), (6,8),(3,3)] == 5
Observe que, apenas na primeira bandeja, o número de latas é maior que copos.-}

coposQuebrados xs = sum(map maiorPeso xs)
maiorPeso (a,b) = if a>b then b else 0

{- Escreva uma função divisores  tal que (divisores n) devolve uma lista dos número que são divisores de n. 
	
	divisores 15 == [1,3,5,15]
	divisores 74 == [1,2,37,74]

Dica: Utilize a função filter sobre a lista [1..n]-}

divisores n = [ x | x <- [1..n], n `mod` x == 0] 

{- Muitas crianças gostam de decidir todas as disputas através do famoso jogo de Par ou Ímpar. Nesse jogo, um dos participantes escolhe Par e o outro Ímpar. Após a escolha, os dois jogadores mostram, simultaneamente, uma certa quantidade de dedos de uma das mãos. Se a soma dos dedos das mãos dos dois jogadores for par, vence o jogador que escolheu Par inicialmente, caso contrário vence o que escolheu Ímpar.

Escreva uma função (parImpar :: [(Int,Int)] -> Int) tal que (parImpar xs) devolve o número de vitórias do primeiro jogador. Cada partida de Par ou Ímpar é descrita por dois inteiros A e B que representam o número de dedos do primeiro jogador e o número de dedos do segundo jogador, respectivamente. Considere que o primeiro jogador sempre escolhe Par.

parImpar [(2,4),(3,5),(1,0)]  == 2

parImpar [(1,5),(2,1),(1,4),(2,2)] == 2

parImpar [(1,5),(2,3)] == 1		-}


parImpar xs = sum(map ehPar xs)
ehPar (a,b) = if ((a+b) `mod` 2 == 0) then 1 else 0

{- Defina uma função segmento tal que (segmento n m xs) é uma lista dos elementos de xs compreendidos entre as posições m e n. Por exemplo,

segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
segmento 1 2 [3,4,1,2,7,9,0] == [3,4]
segmento 1 3 [3,4,1,2,7,9,0] == [3,4,1]
segmento 1 1 [3,4,1,2,7,9,0] == [3]
segmento 1 4 [3,4,1,2,7,9,0] == [3,4,1,2]
segmento 1 5 [3,4,1,2,7,9,0] == [3,4,1,2,7]
Dica: use a função drop e take -}

segmento n m xs
   | n > 1 = take (m-2) (drop(n-1) xs)
   | otherwise = take(m) xs

{- Defina a função somaQuadrados que recebe um inteiro n como argumento e devolve a soma dos quadrados dos primeiros n inteiros, ou seja, somaQuadrados n == 1^2 + 2^2 + .. + n^2
Exemplos
		somaQuadrados 1 == 1
		somaQuadrados 2 == 5
		somaQuadrados 3 == 14
		somaQuadrados 150 == 1136275 	-}
somaQuadrados n = sum(map (quadrado) [1..n])
quadrado n = n * n

{- As cadeias de DNA e RNA são uma sequência de nucleotı́deos. Os quatro nucleotı́deos encontrados no DNA são adenina (A), citosina (C), guanina (G) e timina (T). Os quatro nucleotı́deos encontrados no RNA são adenina (A), citosina (C), guanina (G) e uracila (U). Dada uma sequência de DNA, sua sequência de RNA transcrita é formada substituindo cada nucleotı́deo por seu complemento:

G -> C
C -> G
T -> A
A -> U

Defina uma função toRNA :: String -> String tal que (toRNA xs) devolve a cadeia de RNA formada a partir da cadeia-molde de DNA, xs. Por exemplo, toRNA "ACGTGGTCTTAA"== "UGCACCAGAAUU"		-}
toRNA xs = [letra x | x <- xs]

letra x
   | x == 'G' = 'C'
   | x == 'C' = 'G'
   | x == 'T' = 'A'
   | otherwise = 'U'

{- Todos devem conhecer o jogo Zerinho ou Um (em algumas regiões também conhecido como Dois ou Um), utilizado para determinar um  ganhador entre três ou mais jogadores. Para quem não conhece, o jogo funciona da seguinte maneira. Cada jogador escolhe um valor entre  zero ou um; a um comando (geralmente um dos competidores anuncia em voz alta ?Zerinho ou... Um!?), todos os participantes mostram o valor escolhido, utilizando uma das mãos: se o valor escolhido foi um, o competidor mostra o dedo indicador estendido; se o valor escolhido foi zero,  mostra a mão com todos os dedos fechados. O ganhador é aquele que tiver escolhido um valor diferente de todos os outros; se não há um jogador  com valor diferente de todos os outros (por exemplo todos os jogadores escolhem zero, ou um grupo de jogadores escolhe zero e outro grupo  escolhe um), não há ganhador.

Escreva uma função zeroUm que recebe três inteiros  a ,  b  e  c , que são, respectivamente, o valor escolhido por Alice, o valor escolhido por Beto e  o valor escolhido por Clara, cada valor é zero ou um. Se o vencedor é Alice, a função zeroUm devolve 'A', se o vencedor é Beto, a função zeroUm devolve'B', se o vencedor é Clara, a função zeroUm devolve 'C' e se não há  vencedor, a função de '*' (asterisco).		-}

zeroUm a b c
   | (a == 1 && b == 0 && c == 0) || (a == 0 && b == 1 && c == 1) = 'A'
   | (a == 0 && b == 1 && c == 0) || (a == 1 && b == 0 && c == 1) = 'B'
   | (a == 0 && b == 0 && c == 1) || (a == 1 && b == 1 && c == 0) = 'C'
   | otherwise   = '*'



{- Os enunciados das funções a seguir, estão no arquivo lista2.pdf -}

aproximae n = sum[1/(fromIntegral (fact x)) | x <- [0..n]]
calcPi n = sum[(4*(-1)^(x))/(fromIntegral (2*x+1)) | x <- [0..n]]
