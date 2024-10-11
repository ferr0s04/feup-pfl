import Data.List
import Data.Char

-- Exercício 3.1 -> como [f x | x <- xs, p x] se pode escrever com "map" e "filter"
-- Resposta: map f (filter p xs)


-- Exercício 3.2 -> Função para converter dígitos num inteiro, usando foldl
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0
-- [1, 2, 3, 4] -> 1 -> 1*10 + 2 -> 12*10 + 3 -> 123*10 + 4 -> 1234


-- Exercício 3.3 -> Definição recursiva de zipWith
-- zipWith f xs ys = [f x y | (x, y) <- zip xs ys]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] -- Primeira lista vazia
zipWith' _ _ [] = [] -- Segunda lista vazia
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


-- Exercício 3.4 -> Definir "isort" (ver folha 2) usando foldr e insert
isort'' :: Ord a => [a] -> [a]
isort'' xs = foldr insert [] xs


-- Exercício 3.7 -> Escrever definições não recursivas
-- a) -> (++) :: [a] -> [a] -> [a], usando foldr
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

-- b) -> concat :: [[a]] -> [a], usando foldr
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- c) -> reverse :: [a] -> [a], usando foldr
reverseR :: [a] -> [a]
reverseR = foldr (\x acc -> acc ++ [x]) []

-- d) -> reverse :: [a] -> [a], usando foldl
reverseL :: [a] -> [a]
reverseL = foldl (\acc x -> [x] ++ acc) []

-- e) -> elem :: Eq a => a -> [a] -> Bool, usando any
elem' :: Eq a => a -> [a] -> Bool
elem' n (x:xs) = any (==n) (x:xs)


-- Exercício 3.8
-- a) -> Função para decompôr uma string numa lista de palavras (separadas por espaços)
palavras :: String -> [String]
palavras s = case dropWhile isSpace s of
    "" -> []
    s' -> p : palavras s''
        where (p, s'') = break isSpace s'

-- b) -> Função que junta uma lista de palavras numa string (e separa-as com espaços)
despalavras :: [String] -> String
despalavras [] = ""
despalavras [x] = x
despalavras (x:xs) = x ++ ' ' : despalavras xs

-- words e unwords são inversas?
{-
Para serem inversas, aplicando words e depois unwords deve retornar a mesma string que a inicial.
O mesmo também se aplica para a lista de strings, aplicando unwords e depois words.

Nestes casos, nem sempre a string inicial é a mesma que a final.
Exemplo:
s = "olá   mundo"
Após words -> ["olá", "mundo"]
Após unwords -> "olá mundo"

As strings inicial e final são diferentes ("olá   mundo" /= "olá mundo").

Logo, words e unwords NÃO são inversas!
-}


-- Exercício 3.10 -> Fatorização em números primos usando listas infinitas
-- Definição da aula teórica
primos :: [Int]
primos = crivo [2..]

crivo :: [Int] -> [Int]
crivo (p:xs) = p : crivo [x | x <- xs, x`mod`p /= 0]

factores :: Int -> [Int]
factores n = factores n primos
    where
        factores 1 _ = []
        factores n (x:xs)
            | n `mod` x == 0 = x : factores (n `div` x) (x:xs)
            | otherwise = factores n xs