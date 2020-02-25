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
