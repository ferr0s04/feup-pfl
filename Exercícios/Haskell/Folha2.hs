import Data.List
import Data.Char ( ord, chr )

-- Exercício 2.2 -> Função que intercala um valor entre os elementos de uma lista
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' valor (x:xs) = x : valor : intersperse' valor xs


-- Exercício 2.3 -> Máximo divisor comum
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)


-- Exercício 2.4 -> Ordenação de listas pelo método de inserção
-- a) -> Definir a função 'insert'
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
    | x <= y = x : y : ys -- se x for o menor valor de todos
    | otherwise = y : insert' x ys -- se y for o menor valor de todos

-- b) -> Definir a função 'isort'
isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)


-- Exercício 2.5 -> Ordenação de listas pelo método de seleção
-- a) -> Definir a função 'minimum' (calcula o menor valor de uma lista não vazia)
minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:xs)
    | x <= minimum' xs = x
    | otherwise = minimum' xs

-- b) -> Definir a função 'delete' (remove a primeira ocorrência de um valor na lista)
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (y:ys)
    | x == y = ys
    | otherwise = y : delete' x ys

-- c) -> Definir a função 'ssort' (selection sort), usando as funções anteriores
ssort' :: Ord a => [a] -> [a]
ssort' [] = []
ssort' xs = m : ssort' (delete' m xs)
    where m = minimum' xs


-- Exercício 2.6 -> Expressão que calcula a soma dos quadrados dos inteiros de 1 a 100
quadrados100 :: Integer
quadrados100 = sum [x^2 | x <- [1..100]] -- 338350


-- Exercício 2.9 -> Divisores próprios de um número
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n], n `mod` x == 0]


-- Exercício 2.12 -> Função que testa se um número é primo. Usa a função do ex. 2.9 para obter os divisores do número
primo :: Integer -> Bool
primo n = divprop n == [1, n]


-- Exercício 2.15 -> Cifra de César
cifrar :: Int -> String -> String
cifrar x y = map (cifrarLetra x) y

-- Função auxiliar -> cifra uma só letra
cifrarLetra :: Int -> Char -> Char
cifrarLetra x y
    | y == ' ' = ' '
    | ord y + x <= ord 'Z' = chr (ord y + x) 
    | otherwise = chr (ord 'A' + mod(x + ord y - ord 'A') (ord 'Z' - ord 'A' + 1))


-- Exercício 2.20 -> Definir a função 'transpose' (transposta de uma matriz)
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:xs) = transpose' xs
transpose' ((x:xs):ys) = (x : map head ys) : transpose' (xs : map tail ys)


-- Exercício 2.21 -> Função que obtém os algarismos de um número
algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)


-- Função auxiliar -> Obtém os algarismos na ordem inversa
algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = (n `mod` 10) : algarismosRev (n `div` 10)


-- Exercício 2.24 -> Ordenação de listas pelo método merge sort
-- a) -> Definir a função 'merge' (junta 2 listas ordenadas numa lista também ordenada)
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys)
    | x <= y = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys

-- b) -> Definir a função 'msort' (merge sort), usando a função 'merge' anterior
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort esq) (msort dir)
    where (esq, dir) = metades xs

-- Função auxiliar -> divide uma lista em duas metades ordenadas (ver Folha 1)
metades :: Ord a => [a] -> ([a], [a])
metades xs = (ssort'(take metade xs), ssort'(drop metade xs))
    where metade = length xs `div` 2
