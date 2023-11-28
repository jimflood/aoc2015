module Day6
    ( day6
    ) where

import Lib
import Data.List.Split (splitOneOf)
import qualified Data.Set as Set
import qualified Data.Map as Map

data Instruction = TurnOn (Int, Int, Int, Int) | TurnOff (Int, Int, Int, Int) | Toggle (Int, Int, Int, Int) deriving (Show) 

execute :: [Instruction] -> Set.Set (Int, Int)
execute xs = foldl execute' Set.empty xs
    where
        execute' s (TurnOn (x1, y1, x2, y2)) = Set.union (lamps x1 y1 x2 y2) s
        execute' s (TurnOff (x1, y1, x2, y2)) = Set.difference s (lamps x1 y1 x2 y2)
        execute' s (Toggle (x1, y1, x2, y2)) = Set.union (Set.difference s (lamps x1 y1 x2 y2)) (Set.difference (lamps x1 y1 x2 y2) s)
        lamps x1 y1 x2 y2 = Set.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]]

execute2 :: [Instruction] -> Map.Map (Int, Int) Int
execute2 xs = foldl execute' Map.empty xs
    where
        execute' m (TurnOn (x1, y1, x2, y2)) = plusOne m (lamps x1 y1 x2 y2)
        execute' m (TurnOff (x1, y1, x2, y2)) = minusOne m (lamps x1 y1 x2 y2)
        execute' m (Toggle (x1, y1, x2, y2)) = plusTwo m (lamps x1 y1 x2 y2)
        lamps x1 y1 x2 y2 = Set.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]]
        plusOne m ks = foldl ( \ a b -> Map.alter plusOne' b a) m ks
        plusOne' (Just a) = Just (a + 1)
        plusOne' Nothing = Just 1
        plusTwo m ks = foldl ( \ a b -> Map.alter plusTwo' b a) m ks
        plusTwo' (Just a) = Just (a + 2)
        plusTwo' Nothing = Just 2
        minusOne m ks = foldl ( \ a b -> Map.alter minusOne' b a) m ks
        minusOne' (Just a)
            | a > 1 = Just (a - 1)
            | otherwise = Nothing
        minusOne' Nothing = Nothing

parseLines :: [String] -> [Instruction]
parseLines xs = map parseLine xs
    where
        parseLine :: String -> Instruction
        parseLine s = parseLine' (splitOneOf " ," s)
        parseLine' ["turn", "on", a, b, "through", c, d] = TurnOn (rectOf a b c d)
        parseLine' ["turn", "off", a, b, "through", c, d] = TurnOff (rectOf a b c d)
        parseLine' ["toggle", a, b, "through", c, d] = Toggle (rectOf a b c d)
        parseLine' _ = error "nope"
        rectOf a b c d = (read a::Int, read b::Int, read c::Int, read d::Int)

day6 :: IO ()
day6 = do
    input <- slurpLines "day6.txt"
    let instructions = parseLines input
    let answer1 = length $ execute instructions
    print $ "part 1: " ++ (show answer1)
    let answer2 = Map.foldr ( \ a b -> b + a) 0 (execute2 instructions)
    print $ "part 2: " ++ (show answer2)
