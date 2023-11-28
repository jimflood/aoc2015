module Day13
    ( day13
    ) where

import Lib
import Data.List.Split (splitOneOf)
import Data.List (groupBy, permutations, sortOn)
import qualified Data.Map as Map

type HappinessMap = Map.Map String (Map.Map String Int)

parseLines :: [String] -> HappinessMap
parseLines xss = mapify $ map parseLine xss
    where
        parseLine s = parseLine' (splitOneOf " ." s)
        parseLine' [a, "would", b, c, "happiness", "units", "by", "sitting", "next", "to", d, ""] = parseLine'' a b (read c::Int) d
        parseLine' _ = error "Nope"
        parseLine'' x "gain" n y = (x, (y, n))
        parseLine'' x "lose" n y = (x, (y, -n))
        parseLine'' _ _ _ _ = error "Nope2"
        mapify ys = Map.fromList $ map ( \ a -> (fst (head a), Map.fromList (map snd a))) $ groupBy (\ x y -> fst x == fst y) (sortOn fst ys)

solve :: HappinessMap -> Int
solve hm = maximum $ solve' $ permutations (Map.keys hm)
    where
        solve' ps = map ( \ p -> solve'' (zip p (tail p ++ [head p]))) ps
        solve'' ts = solve''' $ ts ++ (map (\ (a, b) -> (b, a)) ts)
        solve''' xs = sum $ map ( \ (a, b) -> (hm Map.! a) Map.! b) xs

addMe :: HappinessMap -> HappinessMap
addMe hm = addMe' (Map.toList hm) (Map.keys hm)
    where
        addMe' ms ks = Map.fromList $ (fromMe ks) ++ (toMe ms)
        fromMe ks = [("me", Map.fromList (map ( \ k -> (k, 0)) ks))]
        toMe ms = map ( \ (k, v) -> (k, Map.insert "me" 0 v)) ms

day13 :: IO ()
day13 = do
    hm <- parseLines <$> slurpLines "day13.txt"
    let answer1 = solve hm
    print $ "part 1: " ++ (show answer1)
    let modifiedHm = addMe hm
    let answer2 = solve modifiedHm
    print $ "part 2: " ++ (show answer2)
