import Data.Char
import GHC.Float

-- 1ª questão --

menorDeDois x y = min x y

-- 2ª questão --

menorDeTres x y z = min x(min y z)

-- 3ª questão --

factorial x = product[x, x-1 .. 1]

-- 4ª questão --

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- 5ª questão --

elemento i xs = xs !! i

-- 6ª questão --

pertence x xs = elem x xs

-- 7ª questão --

total xs = sum[1 | _ <- xs]

-- 8ª questão --

maior [] = error "nao existe maximo para lista vazia"
maior [x] = x
maior (x:xs)
   | x > maiorzao = x
   | otherwise = maiorzao
   where maiorzao = maior xs

-- 9ª questão --

frequencia x [] = 0
frequencia x [xs] = if(x==xs) then 1 else 0
frequencia x1 (x:xs) = (frequencia x1 [x]) + (frequencia x1 xs)

-- 10ª questão --

unico x xs = (frequencia x xs) == 1

-- 11ª questão --

maioresQue x xs = filter (>x) xs

-- 12ª questão --

concat' [] [] = []
concat' xs xss = xs ++ xss

-- 13ª questão --

calda xs = tail xs

-- 14ª questão --

corpo xs = init xs

-- 15ª questão --

unique [] = []
unique (x:xs) = if(x `elem` xs) then x:unique ([y | y<-xs, y/=x]) else x:unique xs

-- 16ª questão --
menores n xs = aux (take n xs) (drop n xs)
aux xs [] = xs
aux xs1 (x2:xs2) = if((maximum xs1)>x2) then (apago xs1) ++ [x2] else aux xs1 xs2
apago xs = [x | x<-xs, x/=maior]
   where maior = maximum xs

-- 17ª questão --

alter' 0 = []
alter' n = if(n>0) then alter' (-n) ++ [-n] else alter' (-(n+1)) ++ [-n]


-- 18ª questão --

reverso xs = reverse xs

-- 19ª questão --

divide xs n = (take n xs,drop n xs)

-- 20ª questão --

intercal [] xs = xs
intercal xs [] = xs
intercal (x:xs) (x1:xss) = (x:x1:intercal xs xss)

-- 21ª questão --

uniao xss xss2 = unique(unique xss ++ unique xss2)

-- 22ª questão --

intersec xs xs1 = unique([x | x<-xs, x `elem` xs1])

-- 23ª questão --

sequencia n np = take n [np..]

-- 24ª questão --

inserir x (z:xs)
   | z < x = z:inserir x xs
   | otherwise = x:z:xs

-- 25ª questão --

isSorted [x] = True
isSorted (x: xs)
   | x > head xs = False
   | otherwise = isSorted xs

-- 27ª questão --

--rotEsq 0 xs = xs
--rotEsq n xs = 

-- 30 --

titulo' :: String -> String

titulo' [] = []
-- titulo' [x] = [toLower x]
titulo' (x:xs)
    | (x == ' ') && (head xs /= ' ') = " " ++ [toUpper (head xs)] ++ titulo' (tail xs)
    | x == ' ' = " " ++ titulo' xs 
    | otherwise = [toLower x] ++ titulo' xs

titulo :: String -> String

titulo [] = []
titulo [x] = [toUpper x]
titulo xs = [toUpper (head xs)] ++ titulo' (tail xs)

aux' n
   | n < 4 = []
   | otherwise = [y | y <- [2..(n-1)], n `mod` y == 0]

primo 1 = False
primo n = if(length (aux' n) == 0) then True else False




























