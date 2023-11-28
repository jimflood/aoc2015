module Day3
    ( day3
    ) where

import Data.List (nub)

type House = (Int, Int)

deliver :: House -> Char -> House
deliver x '^' = (fst x, snd x - 1)
deliver x 'v' = (fst x, snd x + 1)
deliver x '>' = (fst x + 1, snd x)
deliver x '<' = (fst x - 1, snd x)
deliver _ _ = error "nope"

part1 :: String -> Int
part1 = part1' [(0, 0)] 
    where
        part1' xs [] = length $ nub xs
        part1' (x : xs) (y : ys) = part1' ((deliver x y) : x : xs) ys

part2 :: String -> Int
part2 = part2' [(0, 0)] [(0, 0)]
    where
        part2' xs ys [] = length (nub (xs ++ ys))
        part2' (x : xs) ys (z : zs) = part2'' ((deliver x z) : x : xs) ys zs
        part2'' xs ys [] = length (nub (xs ++ ys))
        part2'' xs (y : ys) (z : zs) = part2' xs ((deliver y z) : y : ys) zs

day3 :: IO ()
day3 = do
    input <- readFile "day3.txt"
    let answer1 = part1 input
    print $ "part 1: " ++ (show answer1)
    let answer2 = part2 input
    print $ "part 2: " ++ (show answer2)
