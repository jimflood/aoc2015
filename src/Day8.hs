module Day8
    ( day8
    ) where

import Lib
import Data.Char
import Debug.Trace

data State = Reset | Quoted

solve :: String -> Int
solve i = solve' Reset 0 0 i
    where
        solve' :: State -> Int -> Int -> String -> Int
        solve' s c m (x : xs)
            | isSpace x = solve' s c m xs
        solve' Reset c m [] = c - m
        solve' _ c m [] = error "Oops"
        solve' Reset c m ('"' : xs) = solve' Quoted (c + 1) m xs
        solve' Quoted c m ('"' : xs) = solve' Reset (c + 1) m xs
        solve' Quoted c m ('\\' : '"' : xs) = solve' Quoted (c + 2) (m + 1) xs
        solve' Quoted c m ('\\' : '\\' : xs) = solve' Quoted (c + 2) (m + 1) xs
        solve' Quoted c m ('\\' : 'x' : a : b : xs) = solve' Quoted (c + 4) (m + 1) xs
        solve' Quoted c m (_ : xs) = solve' Quoted (c + 1) (m + 1) xs
        solve' _ _ _ _ = error "Ruh-roh"

solve2 :: String -> Int
solve2 i = solve2' Reset 0 0 i
    where
        solve2' :: State -> Int -> Int -> String -> Int
        solve2' s c m (x : xs)
            | isSpace x = solve2' s c m xs
        solve2' Reset c m [] = m - c
        solve2' _ c m [] = error "Oops"
        solve2' Reset c m ('"' : xs) = solve2' Quoted (c + 1) (m + 3) xs
        solve2' Quoted c m ('"' : xs) = solve2' Reset (c + 1) (m + 3) xs
        solve2' Quoted c m ('\\' : '"' : xs) = solve2' Quoted (c + 2) (m + 4) xs
        solve2' Quoted c m ('\\' : '\\' : xs) = solve2' Quoted (c + 2) (m + 4) xs
        solve2' Quoted c m ('\\' : 'x' : a : b : xs) = solve2' Quoted (c + 4) (m + 5) xs
        solve2' Quoted c m (_ : xs) = solve2' Quoted (c + 1) (m + 1) xs
        solve2' _ _ _ _ = error "Ruh-roh"

day8 :: IO ()
day8 = do
    input <- readFile "day8.txt"
    let answer1 = solve input
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 input
    print $ "part 2: " ++ (show answer2)
