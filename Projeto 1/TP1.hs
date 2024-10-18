import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [c | (c1, c2, _) <- roadMap, c <- [c1, c2]]


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm


distance :: RoadMap -> City -> City -> Maybe Distance
distance rm city1 city2 = case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm of
    [(_, _, d)] -> Just d
    _           -> Nothing

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent rm city = [(c2, d) | (c1, c2, d) <- rm, c1 == city] ++ [(c1, d) | (c1, c2, d) <- rm, c2 == city]


pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance rm (c1:c2:cs) = 
    case distance rm c1 c2 of
        Nothing -> Nothing  -- Se não há estrada entre c1 e c2, retorna Nothing
        Just d  -> case pathDistance rm (c2:cs) of
            Nothing -> Nothing  -- Propaga o Nothing se houver uma desconexão mais adiante
            Just dist -> Just (d + dist)  -- Soma a distância e continua com o resto do caminho


-- Conta as conexões de cada cidade em um RoadMap
cityDegrees :: RoadMap -> [(City, Int)]
cityDegrees rm = [(city, countConnections city rm) | city <- Data.List.nub (cities rm)]

-- Conta quantas vezes uma cidade aparece nas estradas
countConnections :: City -> RoadMap -> Int
countConnections city rm = length [() | (c1, c2, _) <- rm, c1 == city || c2 == city]

-- Encontra o maior grau entre as cidades
maxDegree :: [(City, Int)] -> Int
maxDegree degrees = foldl1 max (map snd degrees)

-- Função principal rome
rome :: RoadMap -> [City]
rome rm =
  let degrees = cityDegrees rm
      maxDeg = maxDegree(degrees)
  in [city | (city, deg) <- degrees, deg == maxDeg]


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = let
    cityList = cities rm
    reachableFrom c = Data.List.nub (dfs rm [c])
  in all (\city -> reachableFrom city == cityList) cityList

dfs :: RoadMap -> [City] -> [City]
dfs _ [] = []
dfs rm (c:cs) = c : dfs rm (adjacentCities Data.List.\\ (c:cs))
  where adjacentCities = [city | (city, _) <- adjacent rm c]


-- BFS helper for shortest paths without Maybe
bfsPaths :: RoadMap -> [[City]] -> City -> [Path]
bfsPaths _ [] _ = []  -- If no more paths to explore, return empty list
bfsPaths rm (path:paths) target
  | head path == target = path : bfsPaths rm paths target
  | otherwise = bfsPaths rm (paths ++ [nextCity:path | (nextCity, _) <- adjacent rm (head path), nextCity `notElem` path]) target

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end
  | start == end = [[start]]  -- A path from a city to itself
  | otherwise = bfsPaths rm [[start]] end


-- Create a distance matrix for the cities
createDistanceArray :: RoadMap -> [City] -> Data.Array.Array (Int, Int) (Distance)
createDistanceArray rm cs =
    Data.Array.array ((0, 0), (n - 1, n - 1)) [((i, j), dist) | i <- [0..n-1], j <- [0..n-1], let dist = fromMaybe (maxBound :: Distance) (distance rm (cs !! i) (cs !! j))]
  where
    n = length cs
    fromMaybe def m = case m of
        Nothing -> def
        Just x  -> x


-- Helper function to get minimum by length
minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy _ [] = error "Empty list"
minimumBy f xs = foldl1 (\x y -> if f x < f y then x else y) xs


-- TSP using dynamic programming approach
travelSales :: RoadMap -> Path
travelSales rm = 
    let cs = cities rm
        n = length cs
        distArray = createDistanceArray rm cs
        memo = Data.Array.array ((0, 0), (2 ^ n - 1, n - 1)) [((s, i), []) | s <- [0..2 ^ n - 1], i <- [0..n - 1]]
        result = tsp 0 1
    in if null result then [] else result ++ [cs !! 0]  -- Return to starting city

  where
    -- Recursive TSP function
    tsp :: Int -> Int -> [City]
    tsp pos visited
      | visited == (2 ^ n - 1) = [cs !! pos]  -- All cities visited, return the current city
      | otherwise = minimumBy (comparing length) $ filter (not . null) [step next | next <- [0..n-1], next /= pos, not (testBit visited next)]
      where
        step next =
            let dist = distArray ! (pos, next)
                subPath = tsp next (setBit visited next)
            in if null subPath then [] else (cs !! pos) : subPath


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
