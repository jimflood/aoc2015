module Day1
    ( day1
    ) where

part1 :: String -> Integer
part1 = part1' 0
    where
        part1' :: Integer -> String -> Integer
        part1' acc [] = acc
        part1' acc ('(' : xs) = part1' (acc + 1) xs
        part1' acc (')' : xs) = part1' (acc - 1) xs

part2 :: String -> Integer
part2 = part2' 0 0
    where
        part2' :: Integer -> Integer -> String -> Integer
        part2' (-1) n _ = n
        part2' acc n [] = error "nope"
        part2' acc n ('(' : xs) = part2' (acc + 1) (n + 1) xs
        part2' acc n (')' : xs) = part2' (acc - 1) (n + 1) xs

day1 :: IO ()
day1 = do
    input <- readFile "day1.txt"
    let answer1 = part1 input
    print $ "part 1: " ++ (show answer1)
    let answer2 = part2 input
    print $ "part 2: " ++ (show answer2)
