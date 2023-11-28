module Day2
    ( day2
    ) where

import Lib
import Data.List.Split (splitOn)
import Data.List (sort)

type Dimensions = (Int, Int, Int)

type Areas = (Int, Int, Int)

parseLines :: [String] -> [Dimensions]
parseLines = map parseLine
    where
        parseLine :: String -> Dimensions
        parseLine xs = parseLine' $ map ( \x -> read x::Int) (splitOn "x" xs)
        parseLine' :: [Int] -> Dimensions
        parseLine' [a, b, c] =  (a, b, c)

areas :: Dimensions -> Areas
areas (a, b, c) = (a * b, a * c, b * c)

paper :: Dimensions -> Int
paper d = paper' $ areas d
    where
        paper' (a, b, c) = a + a + b + b + c + c + (minimum [a, b, c])

ribbon :: Dimensions -> Int
ribbon (a, b, c) = ribbon' $ sort [a, b, c]
    where
        ribbon' [x, y, z] = x + x + y + y + (x * y * z)

day2 :: IO ()
day2 = do
    input <- slurpLines "day2.txt"
    let dimensions = parseLines input
    let answer1 = sum (map paper dimensions)
    print $ "part 1: " ++ (show answer1)
    let answer2 = sum (map ribbon dimensions)
    print $ "part 2: " ++ (show answer2)
