-- Mini-Teste 1 2021-22

-- Exercício 1 - Máximo de uma lista
maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (x:xs)
    | x > next = x
    | otherwise = next
    where
        next = maxpos xs

-- maxpos [-1,-2,-3,4,-5] -> 4


-- Exercício 2 - Duplicar os valores nas posições ímpares de uma lista
dups :: [a] -> [a]
dups [] = []
dups (x:xs) = x : (case xs of
    [] -> []
    (y:ys) -> x : y : dups ys)

-- dups "abcdef" -> "aabccdeef"


-- Exercício 3 - Linguagem dos Ps: duplicar cada vogal e colocar um 'p' entre elas
transforma :: String -> String
transforma [] = []
transforma (x:xs)
    | x `elem` ['a', 'e', 'i', 'o', 'u'] = x : 'p' : x : transforma xs
    | otherwise = x : transforma xs

-- transforma "ola, mundo!" -> "opolapa, mupundopo!"