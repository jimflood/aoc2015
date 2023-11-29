module Day15
    ( day15
    ) where

import Lib
import Data.List.Split (splitOneOf)
import Data.List (permutations, sortOn)
import qualified Data.Map as Map
import Debug.Trace

data Properties = Properties { ingredient :: String, capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int } deriving (Show)

type PropertiesMap = Map.Map String Properties

type Recipe = [(String, Int)]

parseLines :: [String] -> PropertiesMap
parseLines xs = Map.fromList $ map (\x -> (ingredient x, x)) $ map parseLine xs
    where
        parseLine s = tokens $ splitOneOf " :," s
        tokens [a, "", "capacity", b, "", "durability", c, "", "flavor", d, "", texture, e, "", "calories", f] = tokens' [a, b, c, d, e, f]
        tokens' (a : bs) = tokens'' a (map (\x -> read x::Int) bs)
        tokens'' a [b, c, d, e, f] = Properties { ingredient = a, capacity = b, durability = c, flavor = d, texture = e, calories = f }

g :: Int -> Int -> [[Int]] -> [[Int]]
g 0 sz acc = filter ( \ x -> sum x == sz) $ sequence acc
g n sz acc = g (n - 1) sz ([1..sz] : acc)

measures :: [String] -> Int -> [Recipe]
measures ps tsp = [zip ps r | r <- (g (length ps) tsp [])]

score :: PropertiesMap -> Recipe -> (Int, Int)
score pm r = report $ map ( \ x -> max x 0) $ combine $ map ( \ (k, c) -> score' (pm Map.! k) c) r
    where
        score' p c = [capacity p * c, durability p * c, flavor p * c, texture p * c, calories p * c]
        combine xs = foldl (\ a b -> zipWith (+) a b) (head xs) (tail xs)
        report ys = (product $ init ys, last ys)

day15 :: IO ()
day15 = do
    pm <- parseLines <$> slurpLines "day15.txt"
    let recipes = measures (Map.keys pm) 100
    let scores =  map ( \r -> score pm r) recipes
    let answer1 = maximum $ map fst scores
    print $ "part 1: " ++ (show answer1)
    let answer2 = maximum $ map fst $ filter (\ (a, b) -> b == 500) scores
    print $ "part 2: " ++ (show answer2)
