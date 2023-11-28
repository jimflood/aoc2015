module Day5
    ( day5
    ) where

import Lib

has_three_vowels :: String -> Bool
has_three_vowels s = (vowel_count 0 s) >= 3
    where
        vowel_count n [] = n
        vowel_count n (x: xs)
            | x `elem` ['a', 'e', 'i', 'o', 'u'] = vowel_count (n + 1) xs
            | otherwise = vowel_count n xs

appears_twice_in_a_row :: String -> Bool
appears_twice_in_a_row (x : y : ys)
    | x == y = True
    | otherwise = appears_twice_in_a_row (y : ys)
appears_twice_in_a_row _ = False

does_not_contain :: String -> Bool
does_not_contain (x : y : ys)
    | [x, y] `elem` ["ab", "cd", "pq", "xy"] = False
    | otherwise = does_not_contain (y : ys)
does_not_contain _ = True

nice1 :: String -> Bool
nice1 s = (has_three_vowels s) && (appears_twice_in_a_row s) && (does_not_contain s)

appears_twice :: String -> Bool
appears_twice (x : y : ys)
    | repeats [x, y] ys = True
    | otherwise = appears_twice (y : ys)
    where
        repeats s (x : y : ys)
            | s == [x, y] = True
            | otherwise = repeats s (y : ys)
        repeats _ _ = False
appears_twice _ = False

one_between :: String -> Bool
one_between (x : y : z : zs)
    | x == z = True
    | otherwise = one_between (y : z : zs)
one_between _ = False

nice2 :: String -> Bool
nice2 s = (appears_twice s) && (one_between s)

day5 :: IO ()
day5 = do
    input <- slurpLines "day5.txt"
    let answer1 = length $ filter id $ map nice1 input
    print $ "part 1: " ++ (show answer1)
    let answer2 = length $ filter id $ map nice2 input
    print $ "part 2: " ++ (show answer2)
