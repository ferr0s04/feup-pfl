-- Exercício 1.3
metades :: [a] -> ([a], [a])
metades xs = (take metade xs, drop metade xs)
    where metade = length xs `div` 2

{- Output
metades [1, 2, 3, 4] = ([1, 2], [3, 4])
metades [1, 2, 3, 4, 5] = ([1, 2], [3, 4, 5])
Nota: para listas de tamanho ímpar, a segunda lista fica com mais um elemento que a primeira.
-}


-- Exercício 1.4
-- a)
last' :: [a] -> a
last' xs = head (reverse xs)

last'' :: [a] -> a
last'' xs = head (drop i xs) -- 'drop i xs' retorna uma lista, não um elemento só
    where i = length xs - 1

-- b)
init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

init'' :: [a] -> [a]
init'' xs = drop i xs
    where i = length xs - 1


-- Exercício 1.7
{-
Dica: usar ':t' no ghci para obter o tipo da expressão
a) ['a', 'b', 'c'] -> [a], [Char]
b) ('a', 'b', 'c') -> (Char, Char, Char)
c) [(False, '0'), (True, '1')] -> [(Bool, Char)]
d) ([False, True], ['0', '1']) -> ([Bool], [Char])
e) [tail, init, reverse] -> [[a] -> [a]]
f) [id, not] -> [Bool -> Bool]
-}

-- Exercício 1.8
{-
a) segundo :: [a] -> a
b) trocar :: (a, b) -> (b, a)
c) par :: a -> b -> (a, b)
d) dobro :: Num a => a -> a
e) metade :: Num a => a -> a
f) minuscula :: Ord a => a -> Bool
g) intervalo :: Ord a => a -> a -> a -> Bool
h) palindromo :: Eq a => [a] -> Bool
i) twice :: (a -> a) -> a -> a
-}

-- Exercício 1.12
xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' True False = True
xor' False True = True

-- Exercício 1.16
converte :: Int -> String
converte n
    | n < 0 || n >= 1000000 = "número fora dos limites"
    | n < 1000 = centenas n
    | n == 1000 = "mil"
    | otherwise = let (m, r) = divMod n 1000
        in (if m > 1 then centenas m ++ " " else "") ++ "mil" ++ if r > 0 then " " ++ centenas r else ""

-- Funções auxiliares
unidades :: Int -> String -- Números inferiores a 100
unidades n
    | n < 0 = "Número negativo!"
    | n < 20 = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez", "onze", "doze", "treze", "catorze", "quinze", "dezasseis", "dezassete", "dezoito", "dezanove"] !! n
    | n < 100 = let (d, u) = divMod n 10
        in case d of
            2 -> "vinte" ++ if u > 0 then " e " ++ unidades u else ""
            3 -> "trinta" ++ if u > 0 then " e " ++ unidades u else ""
            4 -> "quarenta" ++ if u > 0 then " e " ++ unidades u else ""
            5 -> "cinquenta" ++ if u > 0 then " e " ++ unidades u else ""
            6 -> "sessenta" ++ if u > 0 then " e " ++ unidades u else ""
            7 -> "setenta" ++ if u > 0 then " e " ++ unidades u else ""
            8 -> "oitenta" ++ if u > 0 then " e " ++ unidades u else ""
            9 -> "noventa" ++ if u > 0 then " e " ++ unidades u else ""

centenas :: Int -> String -- Números inferiores a 1000
centenas n
    | n < 100 = unidades n
    | n == 100 = "cem"
    | otherwise = let (c, u) = divMod n 100
        in case c of
            1 -> "cento" ++ if u > 0 then " e " ++ unidades u else ""
            2 -> "duzentos" ++ if u > 0 then " e " ++ unidades u else ""
            3 -> "trezentos" ++ if u > 0 then " e " ++ unidades u else ""
            4 -> "quatrocentos" ++ if u > 0 then " e " ++ unidades u else ""
            5 -> "quinhentos" ++ if u > 0 then " e " ++ unidades u else ""
            6 -> "seiscentos" ++ if u > 0 then " e " ++ unidades u else ""
            7 -> "setecentos" ++ if u > 0 then " e " ++ unidades u else ""
            8 -> "oitocentos" ++ if u > 0 then " e " ++ unidades u else ""
            9 -> "novecentos" ++ if u > 0 then " e " ++ unidades u else ""