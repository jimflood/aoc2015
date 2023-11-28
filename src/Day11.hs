module Day11
    ( day11
    ) where

import Data.Char

increment :: String -> String
increment s = increment' ((length s) - 1) s
    where
        increment' n xs = increment'' (take n xs) (drop n xs)
            where
                increment'' xs (y : ys)
                    | y == 'z' = increment' (n - 1) (xs ++ ['a'] ++ ys)
                    | otherwise = xs ++ [chr (ord y + 1)] ++ ys

solve :: String -> String
solve s = solve' (increment s)
    where
        solve' newS
            | good newS = newS
            | otherwise = solve' (increment newS)

-- Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
-- Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
-- Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
good :: String -> Bool
good xs = (straight xs) && (legal xs) && (pairs xs)

straight :: String -> Bool
straight (a : b : c : ds)
    | (ord a == ord b - 1) && (ord b == ord c - 1) = True
    | otherwise = straight (b : c : ds)
straight _ = False

legal :: String -> Bool
legal [] = True
legal ('i' : xs) = False
legal ('o' : xs) = False
legal ('l' : xs) = False
legal (x : xs) = legal xs

pairs :: String -> Bool
pairs (a : b : cs)
    | a == b = pairs' cs
    | otherwise = pairs (b : cs)
        where
            pairs' (a : b : cs)
                | a == b = True
                | otherwise = pairs' (b : cs)
            pairs' _ = False
pairs _ = False        

day11 :: IO ()
day11 = do
    let input = "vzbxkghb"
    let answer1 = solve input
    print $ "part 1: " ++ answer1
    let answer2 = solve answer1
    print $ "part 2: " ++ answer2


