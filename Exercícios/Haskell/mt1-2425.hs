-- Mini-Teste 1 2024-25

type Match = ((String,String), (Int,Int))
type MatchDay = [Match]
type League = [MatchDay]

myLeague :: League
myLeague = [
  [(("Porto","Sporting"),(2,2)),(("Benfica","Vitoria SC"),(4,0))],
  [(("Porto","Benfica"),(5,0)),(("Vitoria SC","Sporting"),(3,2))],
  [(("Vitoria SC","Porto"),(1,2)),(("Sporting","Benfica"),(2,1))]
  ]

-- Exercício 1 - Vencedor de um jogo (ou empate)
winner :: Match -> String
winner ((team1, team2), (score1, score2))
    | score1 > score2 = team1
    | score1 < score2 = team2
    | otherwise = "draw"

-- winner (("Porto","Benfica"),(5,0)) -> "Porto"


-- Exercício 2 - Pontuação de uma dada equipa e num dado dia
matchDayScore :: String -> MatchDay -> Int
matchDayScore team matchDay = case filter (isTeamInMatch team) matchDay of -- Filtra os jogos em que a equipa jogou no dia
    [] -> 0 -- A equipa não jogou nesse dia
    (match:_) -> matchScore team match

-- Função auxiliar que verifica se uma equipa jogou num determinado jogo
isTeamInMatch :: String -> Match -> Bool
isTeamInMatch team ((team1, team2), _) = team == team1 || team == team2

-- Função auxiliar que calcula os pontos da equipa num determinado jogo
matchScore :: String -> Match -> Int
matchScore team ((team1, team2), (score1, score2))
    | team == team1 && score1 > score2 = 3
    | team == team2 && score1 < score2 = 3
    | (team == team1 || team == team2) && score1 == score2 = 1
    | otherwise = 0

-- matchDayScore "Porto" (myLeague !! 0) -> 1


leagueScore :: String -> League -> Int
leagueScore t = foldr (\d acc -> matchDayScore t d + acc) 0

sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond [x] _ = [x]
sortByCond l cmp = merge (sortByCond l1 cmp) (sortByCond l2 cmp) cmp
  where (l1 ,l2) = splitAt (div (length l) 2) l

merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) cmp
  | cmp x y = x:(merge xs (y:ys) cmp)
  | otherwise = y:(merge (x:xs) ys cmp)

-- Exercício 3 - Ranking de equipas ordenadas por ordem decrescente de pontos na liga
ranking :: League -> [(String,Int)]
ranking [] = []
ranking l =
    let teams = myNub (concatMap (\day -> [team1 | ((team1, _), _) <- day] ++ [team2 | ((_, team2), _) <- day]) l)
        scores = [(team, leagueScore team l) | team <- teams]
    in sortByCond scores compareRankings

-- Função auxiliar que compara duas equipas (usar em sortByCond)
compareRankings :: (String, Int) -> (String, Int) -> Bool
compareRankings (team1, score1) (team2, score2)
    | score1 > score2 = True
    | score1 < score2 = False
    | otherwise = team1 < team2 -- Em caso de empate, ordem alfabética

-- Implementação do nub (não é permitido usar módulos)
myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub (x:xs) = x : myNub (filter (/= x) xs)

-- ranking myLeague -> [("Porto",7),("Sporting",4),("Benfica",3),("Vitoria SC",3)]


-- Exercício 4 - Número de dias com pelo menos um empate
numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws l = length (filter hasDrawMatch l)

-- Função auxiliar que determina se um dia tem pelo menos um empate
hasDrawMatch :: MatchDay -> Bool
hasDrawMatch = any isDrawMatch

-- Função auxiliar que determina se um jogo acabou num empate
isDrawMatch :: Match -> Bool
isDrawMatch (_, (score1, score2)) = score1 == score2

-- numMatchDaysWithDraws myLeague -> 1


-- Exercício 5 - Equipas, em cada dia, que ganharam com pelo menos 3 golos de diferença
bigWins :: League -> [(Int,[String])]
bigWins l = [(i, [winner | ((team1, team2), (score1, score2)) <- matchDay, abs (score1 - score2) >= 3,
    let winner = if score1 > score2 then team1 else team2]) | (i, matchDay) <- zip [1..] l]

-- bigWins myLeague -> [(1,["Benfica"]),(2,["Porto"]),(3,[])]


-- Exercício 6 - Sequências de pelo menos 2 vitórias consecutivas (equipa, índice 1a vitória, índice última vitória)
winningStreaks :: League -> [(String, Int, Int)]
winningStreaks league = concat [findStreaks team league | team <- teams]
  where
    teams = myNub [team | matchDay <- league, ((team1, team2), _) <- matchDay, team <- [team1, team2]]

-- Função auxiliar para procurar sequências de vitórias consecutivas para uma equipa
findStreaks :: String -> League -> [(String, Int, Int)]
findStreaks team league = [(team, s, e) | (s, e) <- streaks]
  where
    winningDays = [i | (i, matchDay) <- zip [1..] league, didWin team matchDay]
    streaks = consecutiveWins winningDays

-- Função auxiliar que verifica se uma equipa ganhou num dia
didWin :: String -> MatchDay -> Bool
didWin team matchDay
    | matchDayScore team matchDay == 3 = True
    | otherwise = False

-- Função auxiliar que procura duas vitórias consecutivas de uma equipa
consecutiveWins :: [Int] -> [(Int, Int)]
consecutiveWins [] = []
consecutiveWins (x:xs) = processStreak x x xs
  where
    processStreak start current [] = [(start, current) | current > start]
    processStreak start current (y:ys)
      | y == current + 1 = processStreak start y ys
      | current > start = (start, current) : processStreak y y ys
      | otherwise = processStreak y y ys

-- winningStreaks myLeague -> [("Porto",2,3)]


type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

gTest1 :: RoadMap
gTest1 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest2 :: RoadMap
gTest2 = [("0","1",4),("2","3",2)]

-- Exercício 7 - Cidades adjacentes a uma determinada cidade
adjacent :: RoadMap -> City ->[(City,Distance)]
adjacent rm c = [(city, distance) | (start, city, distance) <- rm, start == c]
    ++ [(start, distance) | (start, city, distance) <- rm, city == c]

-- adjacent gTest1 "0" -> [("1",10),("2",15),("3",20)]


-- Exercício 8 - Se existe um caminho entre duas dadas cidades -> usar DFS
areConnected :: RoadMap -> City -> City -> Bool
areConnected rm orig dest = dfs orig [] where
    dfs current visited
        | current == dest = True -- Se chegámos ao destino
        | current `elem` visited = False -- A cidade já foi visitada
        | otherwise = any (\(next, _) -> dfs next (current : visited)) (adjacent rm current) -- DFS recursivo: adiciona a cidade atual às visitadas e pesquisa uma cidade qualquer que seja adjacente à atual

-- areConnected gTest1 "0" "3" -> True
-- areConnected gTest2 "0" "3" -> False


data KdTree = Empty | Node Char (Int,Int) KdTree KdTree deriving (Eq,Show)

tree1 :: KdTree
tree1 = Node 'x' (3,3) (Node 'y' (2,2) Empty Empty) (Node 'y' (4,4) Empty Empty)

tree2 :: KdTree
tree2 = Node 'x' (3,3) (Node 'y' (2,2) (Node 'x' (1,1) Empty Empty) Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)

-- Exercício 9 - Adicionar um ponto à árvore
insert :: (Int,Int) -> KdTree -> KdTree
insert p Empty = Node 'x' p Empty Empty -- Se for uma árvore vazia, adicionar o ponto na root
insert p@(x, y) (Node axis (px, py) left right)
    | p == (px, py) = Node axis (px, py) left right -- Caso o ponto já exista na árvore, retornar a árvore inalterada
    | axis == 'x' =
        if x < px then Node 'x' (px, py) (insert p left) right
            else Node 'x' (px, py) left (insert p right)
    | axis == 'y' =
        if y < py then Node 'y' (px, py) (insert p left) right
            else Node 'y' (px, py) left (insert p right)

-- insert (3,2) tree1 -> Node 'x' (3,3) (Node 'y' (2,2) Empty Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)
