module Day9
    ( day9
    ) where

import Lib
import Data.List.Split (splitOn)
import Data.List (groupBy, permutations, sortOn)
import qualified Data.Map as Map

type DistanceMap = Map.Map String (Map.Map String Int)

parseLines :: [String] -> DistanceMap
parseLines xs = Map.fromList $ map ( \ x -> (fst (head x), Map.fromList (map snd x))) $ groupBy ( \a b -> fst a == fst b) $ sortOn fst $ concatMap parseLine xs
    where
        parseLine s = parseLine' (splitOn " " s)
        parseLine' [a, "to", b, "=", c] = parseLine'' a b (read c::Int)
        parseLine'' a b c = [(a, (b, c)), (b, (a, c))]

distance :: DistanceMap -> [String] -> Int
distance m = distance' 0
    where
        distance' n [_] = n
        distance' n (a : b : cs) = distance' (n + ((m Map.! a) Map.! b)) (b : cs)

day9 :: IO ()
day9 = do
    input <- slurpLines "day9.txt"
    let distanceMap = parseLines input
    let paths = map ( \x -> (distance distanceMap x, x)) $ permutations $ Map.keys distanceMap
    let answer1 = fst $ head $ sortOn fst paths
    print $ "part 1: " ++ (show answer1)
    let answer2 = fst $ head $ reverse $ sortOn fst paths
    print $ "part 2: " ++ (show answer2)
