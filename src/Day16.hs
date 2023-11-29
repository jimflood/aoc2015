module Day16
    ( day16
    ) where

import Lib
import Data.List.Split (splitOneOf)
import Data.List (permutations, sortOn)
import qualified Data.Map as Map

type Compounds = Map.Map String Int

type AuntMap = Map.Map Int Compounds

parseLines :: [String] -> AuntMap
parseLines xs = Map.fromList $ map parseLine xs
    where
        parseLine s = tokens $ splitOneOf " :," s
        -- Sue 9: pomeranians: 3, goldfish: 10, trees: 10
        tokens ["Sue", a, "", b, "", c, "", d, "", e, "", f, "", g] = (read a::Int, Map.fromList [(b, read c::Int), (d, read e::Int), (f, read g::Int)])

tape :: Compounds
tape = Map.fromList [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

match1 :: Compounds -> Bool
match1 m = length (Map.differenceWith (\ a b -> if a == b then Nothing else Just 1) m (Map.intersection tape m)) == 0

match2 :: Compounds -> Bool
match2 m = length (Map.differenceWithKey (\ k a b -> if match2' k a b then Nothing else Just 1) m (Map.intersection tape m)) == 0
    where
        match2' "cats" a b = a > b
        match2' "trees" a b = a > b
        match2' "pomeranians" a b = a < b
        match2' "goldfish" a b = a < b
        match2' _ a b = a == b

solve :: (Compounds -> Bool) -> AuntMap -> Int
solve f aunts = head $ Map.keys $ fst $ Map.partition f aunts

day16 :: IO ()
day16 = do
    aunts <- parseLines <$> slurpLines "day16.txt"
    let answer1 = solve match1 aunts
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve match2 aunts
    print $ "part 2: " ++ (show answer2)
