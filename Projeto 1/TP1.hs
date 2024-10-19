import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
data AdjPointers = Place City [(AdjPointers, Distance)]


-- Function to get the list of all cities
cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [c | (c1, c2, _) <- roadMap, c <- [c1, c2]]


-- Function to check if two cities are directly connected
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm


{- Function to calculate the distance between two directly connected cities.

It returns Nothing if the cities are not directly connected.
-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm city1 city2 = case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm of
    [(_, _, d)] -> Just d
    _           -> Nothing


-- Function that returns the list of cities that are directly connected with a specific city
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent rm city = [(c2, d) | (c1, c2, d) <- rm, c1 == city] ++ [(c1, d) | (c1, c2, d) <- rm, c2 == city]


-- Function to calculate the sum of distances in a path. The function returns Nothing if there are two cities that are not directly connected.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance rm (c1:c2:cs) = 
    case distance rm c1 c2 of
        Nothing -> Nothing
        Just d  -> case pathDistance rm (c2:cs) of
            Nothing -> Nothing
            Just dist -> Just (d + dist)


-- Auxiliary function that counts the number of connections for a specific city
countConnections :: City -> RoadMap -> Int
countConnections city rm = length [() | (c1, c2, _) <- rm, c1 == city || c2 == city]


-- Function that returns the city (or cities) with the most connections
rome :: RoadMap -> [City]
rome rm =
  let degrees = [(city, countConnections city rm) | city <- Data.List.nub (cities rm)]
      maxDegree = foldl1 max (map snd degrees)
  in [city | (city, deg) <- degrees, deg == maxDegree]


-- TODO - WITH ERRORS
-- Function that checks if the roadmap graph is strongly connected
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm =
  let cityList = cities rm
  in not (null cityList) && (length (dfs rm (head cityList) []) == length cityList)

-- TODO - WITH ERRORS
-- Auxiliary function for DFS
dfs :: RoadMap -> City -> [City] -> [City]
dfs rm c visited
  | c `elem` visited = visited -- If already visited, return the visited list
  | otherwise =
      let newVisited = c : visited
          adjacentCities = [city | (city, _) <- adjacent rm c] -- Extract only cities
      in foldl (\acc city -> dfs rm city newVisited) newVisited adjacentCities


-- Auxiliary function that uses BFS to find shortest paths
bfsPaths :: RoadMap -> [[City]] -> City -> [Path]
bfsPaths _ [] _ = []
bfsPaths rm (path:paths) target
  | head path == target = path : bfsPaths rm paths target
  | otherwise = bfsPaths rm (paths ++ [nextCity:path | (nextCity, _) <- adjacent rm (head path), nextCity `notElem` path]) target


-- Function to find all shortest paths
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end
  | start == end = [[start]]  -- A path from a city to itself
  | otherwise = bfsPaths rm [[start]] end


-- Auxiliary function to create a distance matrix for the cities
distanceMatrix :: RoadMap -> [City] -> AdjMatrix
distanceMatrix rm cs =
    Data.Array.array ((0, 0), (n - 1, n - 1)) [((i, j), distance rm (cs !! i) (cs !! j)) | i <- [0..n-1], j <- [0..n-1]]
  where
    n = length cs


-- Auxiliary function to find the minimum path by length
minimumByLength :: [[City]] -> [City]
minimumByLength [] = []
minimumByLength (x:xs) = foldl minPath x xs
  where
    minPath a b = if length a < length b then a else b


-- TODO - WITH ERRORS
-- Function that returns a solution for TSP using dynamic programming
travelSales :: RoadMap -> Path
travelSales rm =
  let cs = cities rm
      n = length cs
      distMatrix = distanceMatrix rm cs
  in if n == 0 || not (isStronglyConnected rm)
     then []
     else (tsp cs distMatrix n 0 1) ++ [cs !! 0]

  where
    tsp :: [City] -> AdjMatrix -> Int -> Int -> Int -> Path
    tsp cs distArray n pos visited
      | visited == (2 ^ n - 1) = [cs !! pos]
      | otherwise =
        let nextCities = [next | next <- [0..n-1], next /= pos, not (Data.Bits.testBit visited next)]
        in minimumByLength [step next | next <- nextCities]

      where
        step next =
          case distArray Data.Array.! (pos, next) of
            Just d -> let subPath = tsp cs distArray n next (Data.Bits.setBit visited next)
                      in if null subPath then [] else (cs !! pos) : subPath
            Nothing -> []


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
