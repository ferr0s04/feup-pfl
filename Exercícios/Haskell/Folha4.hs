import Data.List
data Arv a = Vazia | No a (Arv a) (Arv a)

-- Exercício 4.1 -> Função recursiva que soma os valores de uma árvore binária
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No val esq dir) = val + sumArv esq + sumArv dir


-- Exercício 4.2 -> Função para listar os elementos duma árvore, por ordem decrescente
-- Função definida nas aulas teóricas
listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

-- Função nova -> Basta trocar a ordem da listagem (assumimos que a árvore está ordenada)
listarDec :: Arv a -> [a]
listarDec Vazia = []
listarDec (No x esq dir) = listarDec dir ++ [x] ++ listarDec esq


-- Exercício 4.3
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x _ _) = [x] -- Nível 0 (raiz da árvore)
nivel n (No _ esq dir) = nivel (n - 1) esq ++ nivel (n - 1) dir


-- Exercício 4.5 -> Função que aplica uma função a cada valor de uma árvore
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

-- Funções de teste
-- Soma mais 1 a um inteiro
soma :: Int -> Int
soma n = n + 1

-- Imprime a árvore (GHCi)
imprimirArvore :: Show a => Arv a -> IO ()
imprimirArvore Vazia = return ()
imprimirArvore (No x esq dir) = do
    imprimirArvore esq
    putStrLn (show x)
    imprimirArvore dir

arvore1t = mapArv soma arvore1
-- imprimirArvore arvore1t...


-- Exercício 4.6
-- a) -> Função para obter o valor mais à direita duma árvore (maior valor)
-- Função definida na aula teórica -> Valor mais à esquerda duma árvore (menor valor)
maisEsq :: Arv a -> a
maisEsq (No x Vazia _) = x
maisEsq (No _ esq _) = maisEsq esq

-- Função nova -> Valor mais à direita
maisDir :: Arv a -> a
maisDir (No x _ Vazia) = x
maisDir (No _ _ dir) = maisDir dir

-- b)
remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia -- não ocorre
remover x (No y Vazia dir) -- um descendente
    | x==y = dir
remover x (No y esq Vazia) -- um descendente
    | x==y = esq
remover x (No y esq dir) -- dois descendentes
    | x<y = No y (remover x esq) dir
    | x>y = No y esq (remover x dir)
    | x==y = 
        let z = maisDir esq 
        in No z (remover z esq) dir

{- 
imprimirArvore (remover 2 arvore1) 
3
5
11
13
19
23
-}

-- Exercício 4.7 -> Definir eval, show e size para o seguinte tipo
data Expr = Lit Integer
          | Op Ops Expr Expr
          | If BExp Expr Expr

data Ops = Add | Sub | Mul | Div | Mod

-- Ex 4.8
data BExp = BoolLit Bool
          | And BExp BExp
          | Not BExp
          | Equal Expr Expr
          | Greater Expr Expr


-- Eval
eval' :: Expr -> Integer
eval' (Lit n) = n
eval' (Op Add num1 num2) = eval' num1 + eval' num2
eval' (Op Sub num1 num2) = eval' num1 - eval' num2
eval' (Op Mul num1 num2) = eval' num1 * eval' num2
eval' (Op Div num1 num2) = eval' num1 `div` eval' num2
eval' (Op Mod num1 num2) = eval' num1 `mod` eval' num2
eval' (If b e1 e2) = if bEval b then eval' e1 else eval' e2 -- Ex 4.8

instance Show Expr where
    show (Lit n) = show n
    show (Op Add num1 num2) = show num1 ++ " + " ++ show num2
    show (Op Sub num1 num2) = show num1 ++ " - " ++ show num2
    show (Op Mul num1 num2) = show num1 ++ " * " ++ show num2
    show (Op Div num1 num2) = show num1 ++ " / " ++ show num2
    show (Op Mod num1 num2) = show num1 ++ " mod " ++ show num2
    show (If b num1 num2) = "if " ++ show b ++ " then " ++ show num1 ++ " else " ++ show num2 -- Ex 4.8

-- Exercício 4.8 -> Adição de expressões condicionais
bEval :: BExp -> Bool
bEval (BoolLit b) = b
bEval (And b1 b2) = bEval b1 && bEval b2
bEval (Not b) = not (bEval b)
bEval (Equal e1 e2) = eval' e1 == eval' e2
bEval (Greater e1 e2) = eval' e1 > eval' e2

instance Show BExp where
    show (BoolLit b) = show b
    show (And b1 b2) = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
    show (Not b) = "not " ++ show b
    show (Equal e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (Greater e1 e2) = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"


-- Árvores de teste
-- Árvore 1 (média): 
--        11
--      /    \
--     3      19
--   /   \   /   \
--  2     5 13    23

arvore1 :: Arv Int
arvore1 = No 11
            (No 3 (No 2 Vazia Vazia) (No 5 Vazia Vazia))
            (No 19 (No 13 Vazia Vazia) (No 23 Vazia Vazia))

-- Árvore 2 (maior):
--        10
--      /    \
--     4      18
--   /   \   /   \
--  1     6 15    20
--       / \
--      5   8

arvore2 :: Arv Int
arvore2 = No 10
            (No 4 (No 1 Vazia Vazia) 
                   (No 6 (No 5 Vazia Vazia) (No 8 Vazia Vazia)))
            (No 18 (No 15 Vazia Vazia) (No 20 Vazia Vazia))

-- Árvore 3 (um só elemento): 
--   7

arvore3 :: Arv Int
arvore3 = No 7 Vazia Vazia

-- Árvore 4 (vazia):
arvore4 :: Arv Int
arvore4 = Vazia

