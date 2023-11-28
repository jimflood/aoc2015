module Day12
    ( day12
    ) where

import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

data State = Reset | Quoted | Number deriving (Show)

parse :: String -> [Int]
parse i = parse' Reset [] [] i
    where
        parse' Quoted acc [] ('"' : xs) = parse' Reset acc [] xs
        parse' Quoted acc [] (_ : xs) = parse' Quoted acc [] xs
        parse' Reset acc [] ('"' : xs) = parse' Quoted acc [] xs
        parse' Reset acc [] ('-' : xs) = parse' Number acc ['-'] xs
        parse' Reset acc [] (x : xs)
            | isDigit x = parse' Number acc [x] xs
        parse' Number acc ys (x : xs)
            | isDigit x = parse' Number acc (x : ys) xs
            | otherwise = parse' Reset ((read (reverse ys)::Int) : acc) [] xs
        parse' Reset acc [] (_ : xs) = parse' Reset acc [] xs
        parse' Reset acc [] [] = acc
        parse' s acc ys xs = error ("Ruh-roh: " ++ (show s) ++ " " ++ (show acc) ++ " " ++ (show ys) ++ " " ++ (show xs))

parse2 :: String -> [((Int, Set.Set String), Int)]
parse2 i = parse2' 1 1 [] ['i'] Map.empty [] [[]] [] i
    where
        parse2' :: Int -> Int -> [Int] -> [Char] -> (Map.Map Int (Set.Set [Char])) -> [((Int, [Int]), Int)] -> [[((Int, [Int]), Int)]] -> [Char] -> String -> [((Int, Set.Set String), Int)]
        parse2' c n gn s p g acc w (x : xs)
            | isSpace x = parse2' c n gn s p g acc w xs
        parse2' c n gn ('"' : ':' : s) p g acc w ('"' : xs) = parse2' (c + 1) n gn s (Map.insert (head gn) (Set.insert (reverse w) (p Map.! (head gn))) p) g acc [] xs
        parse2' c n gn ('"' : s) p g acc _ ('"' : xs) = parse2' (c + 1) n gn s p g acc [] xs
        parse2' c n gn ('"' : s) p g acc w (x : xs) = parse2' (c + 1) n gn ('"' : s) p g acc (x : w) xs
        parse2' c n gn s p g acc [] ('"' : xs) = parse2' (c + 1) n gn ('"' : s) p g acc [] xs
        parse2' c n gn s p g acc [] (':' : xs) = parse2' (c + 1) n gn (':' : s) p g acc [] xs
        parse2' c n gn s p g acc [] (',' : xs) = parse2' (c + 1) n gn s p g acc [] xs
        parse2' c n gn (':' : s) p g acc [] ('{' : xs) = parse2' (c + 1) n (c : gn) ('{' : s) (Map.insert c Set.empty p) g ([] : acc) [] xs
        parse2' c n gn s p g acc [] ('{' : xs) = parse2' (c + 1) n (c : gn) ('{' : s) (Map.insert c Set.empty p) g ([] : acc) [] xs
        parse2' c n gn (':' : s) p g acc [] ('[' : xs) = parse2' (c + 1) n gn ('[' : s) p g acc [] xs
        parse2' c n gn s p g acc [] ('[' : xs) = parse2' (c + 1) n gn ('[' : s) p g acc [] xs
        parse2' c n gn ('{' : s) p g acc [] ('}' : xs) = parse2' (c + 1) n (tail gn) s p ((head acc) ++ g) (tail acc) [] xs
        parse2' c n gn ('[' : s) p g acc [] (']' : xs) = parse2' (c + 1) n gn s p g acc [] xs
        parse2' c n gn s p g acc w (x : xs)
            | (x == '-' && w == []) || isDigit x = parse2' (c + 1) n gn s p g acc (x : w) xs
        parse2' c n gn (':' : s) p g acc (w : ws) xs = parse2' (c + 1) (n + 1) gn s p g ((((n, gn), (read (reverse (w : ws))::Int)) : (head acc)) : (tail acc)) [] xs
        parse2' c n gn s p g acc (w : ws) xs = parse2' (c + 1) (n + 1) gn s p g ((((n, gn), (read (reverse (w : ws))::Int)) : (head acc)) : (tail acc)) [] xs
        parse2' c n [] ['i'] p g [[]] [] [] = map (\ ((a, b), c) -> ((a, collect b p), c)) g
        parse2' _ _ _ s _ _ acc w xs = error ("Ruh-roh s=" ++ s ++ " acc=" ++ (show acc) ++ " w=" ++ w ++ " xs=" ++ xs)
        collect gn p = foldl (\ a b -> Set.union (Map.findWithDefault Set.empty b p) a) Set.empty gn

day12 :: IO ()
day12 = do
    input <- readFile "day12.txt"
    let answer1 = sum (parse input)
    print $ "part 1: " ++ (show answer1)
    let answer2 = sum $ map snd $ filter ( \x -> Set.notMember "red" (snd (fst x))) (parse2 input)
    print $ "part 2: " ++ (show answer2)
