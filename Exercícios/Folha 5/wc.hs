import Data.List

-- Exercício 5.1 -> Programa para a função "wc" do Unix (nº linhas, palavras e bytes)
main :: IO()
main = do
    input <- getContents
    let nLines = length (Data.List.lines input)
        nWords = length (Data.List.words input)
        nBytes = length input -- O comprimento da string é igual ao seu número de bytes
    putStrLn $ show nLines ++ " " ++ show nWords ++ " " ++ show nBytes