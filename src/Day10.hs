module Day10
    ( day10
    ) where

import Data.List (group)
import Debug.Trace

step :: String -> String
step xs = concatMap ( \x -> (show (length x)) ++ [head x]) $ group xs

solve :: Int -> String -> String
solve limit = solve' 0
    where
        solve' n x
            | n == limit = x
            | otherwise = solve' (n + 1) (step x)

day10 :: IO ()
day10 = do
    let input = "1113222113"
    let answer1 = length (solve 40 input)
    print $ "part 1: " ++ (show answer1)
    let answer2 = length (solve 50 input)
    print $ "part 2: " ++ (show answer2)


