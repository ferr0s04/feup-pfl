import Data.List
-- Exercício 5.2 -> Programa para inverter uma linha de input

main :: IO ()
main = do
    input <- getContents
    putStrLn $ reverse input