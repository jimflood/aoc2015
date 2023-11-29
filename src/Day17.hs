module Day17
    ( day17
    ) where

import Lib

solve :: Int -> [Int] -> [[Int]]
solve sz cs = filter ( \ a -> sum a == sz) $ solve' [] cs
    where
        solve' acc [] = acc
        solve' acc (x : xs) = solve' (filter ( \ a -> sum a <= sz) ((map (\ a -> x : a) acc) ++ acc ++ ([[x]]))) xs

day17 :: IO ()
day17 = do
    capacities <- (map ( \ x -> read x::Int)) <$> slurpLines "day17.txt"
    let combinations = solve 150 capacities
    let answer1 = length $ combinations
    print $ "part 1: " ++ (show answer1)
    let minSize = minimum $ map length combinations
    let answer2 = length $ filter ( \ x -> length x == minSize) combinations
    print $ "part 2: " ++ (show answer2)
