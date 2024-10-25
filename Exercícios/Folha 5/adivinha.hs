import Data.List
import Data.Char

-- Exercício 5.4 -> Adivinhar uma palavra
adivinha :: String -> IO ()
adivinha p = loop [] 0
    where
        loop adivinhas tentativas = do
            let estado = revelarLetra p adivinhas
            putStrLn estado
            if estado == p then putStrLn ("Adivinhou em " ++ show tentativas ++ " tentativas") -- A palavra foi adivinhada totalmente
            else do
                putStrLn "? "
                letra <- getLine
                let l = head letra
                if l `elem` p then loop (l : adivinhas) (tentativas + 1)
                else do
                    putStrLn "Não ocorre"
                    loop adivinhas (tentativas + 1)

-- Função auxiliar -> mostra as letras já adivinhadas
revelarLetra :: String -> String -> String
revelarLetra pal lAdivinhadas = [if l `elem` lAdivinhadas then l else '-' | l <- pal]

main :: IO ()
main = do
    input <- getLine
    adivinha input