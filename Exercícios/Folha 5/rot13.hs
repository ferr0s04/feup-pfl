import Data.List
import Data.Char

-- Exercício 5.3 -> Cifra de César de 13 posições

-- Cifra de César (folha 2)
cifrar :: Int -> String -> String
cifrar x y = map (cifrarLetra x) y

-- Função auxiliar -> cifra uma só letra
cifrarLetra :: Int -> Char -> Char
cifrarLetra x y
    -- Letras maiúsculas
    | isUpper y && ord y + x <= ord 'Z' = chr (ord y + x)
    | isUpper y = chr (ord 'A' + mod (x + ord y - ord 'A') (ord 'Z' - ord 'A' + 1))
    -- Letras minúsculas
    | isLower y && ord y + x <= ord 'z' = chr (ord y + x)
    | isLower y = chr (ord 'a' + mod (x + ord y - ord 'a') (ord 'z' - ord 'a' + 1))
    -- Espaços, símbolos...
    | otherwise = y

main :: IO ()
main = do
    input <- getContents
    putStrLn $ cifrar 13 input