type Species = (String, Int)
type Zoo = [Species]

myZoo = [("Macaco", 100 :: Int), ("Elefante", 85 :: Int), ("Ave", 164 :: Int)]

isEndangered :: Species -> Bool
isEndangered (_, n) = n <= 100


updateSpecies :: Species -> Int -> Species
updateSpecies (name, old) new = (name, new)


filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (x:xs) f = if f x then x:filterSpecies xs f else filterSpecies xs f

countAnimals :: Zoo -> Int
countAnimals z = sum (map snd z)

substring :: (Integral a) => String -> a -> a -> String
substring s start end = [s !! i | i <- [fromIntegral start..fromIntegral end]]

hasSubstr :: String -> String -> Bool
hasSubstr [] _ = False
hasSubstr _ [] = True
hasSubstr str sub
    | take (length sub) str == sub = True -- Verifica se o início da string corresponde à substring
    | otherwise = hasSubstr (tail str) sub -- Senão, vai avançando e verificando no resto da string


sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr z sub = ([s | s <- z, hasSubstr (fst s) sub], [s | s <- z, not (hasSubstr (fst s) sub)]) -- Usa a função anterior. Usa fst s para ir buscar o nome da espécie


rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : zipWith (+) rabbits (tail rabbits)

rabbitYears :: (Integral a) => a -> Int
rabbitYears n = length ([x | x <- take (fromIntegral n) rabbits, x < n])


data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram

myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf str) = 0
dendroWidth (Node d1 w d2) = w*2 + div (max (dendroWidth d1) (dendroWidth d2)) 2

dendroInBounds :: Dendrogram -> Int -> [String] -- Com erros
dendroInBounds (Leaf str) limit
    | limit >= 0 = [str]
    | otherwise = []
dendroInBounds (Node d1 w d2) limit = dendroInBounds d1 (limit - w) ++ dendroInBounds d2 (limit - w)

