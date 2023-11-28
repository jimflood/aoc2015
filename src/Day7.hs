module Day7
    ( day7
    ) where

import Lib
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (isDigit)
import Debug.Trace
import qualified Data.Bits as Bits
import Data.Maybe

data Instruction = Provide String String | And String String String | Or String String String | Not String String | LShift String String String | RShift String String String deriving (Show)

type CircuitMap = Map.Map String Instruction

type SignalMap = Map.Map String Int

parseLines :: [String] -> [Instruction]
parseLines xs = map parseLine xs
    where
        parseLine :: String -> Instruction
        parseLine s = parseLine' (splitOn " " s)
        parseLine' [a, "->", b] = Provide a b
        parseLine' [a, "AND", b, "->", c] = And a b c
        parseLine' [a, "OR", b, "->", c] = Or a b c
        parseLine' ["NOT", a, "->", b] = Not a b
        parseLine' [a, "LSHIFT", b, "->", c] = LShift a b c
        parseLine' [a, "RSHIFT", b, "->", c] = RShift a b c
        parseLine' s = error ("nope: " ++ (show s))

outputWire :: Instruction -> String
outputWire (Provide _ x) = x
outputWire (And _ _ x) = x
outputWire (Or _ _ x) = x
outputWire (Not _ x) = x
outputWire (LShift _ _ x) = x
outputWire (RShift _ _ x) = x

assemble :: [Instruction] -> CircuitMap
assemble xs = foldl ( \ a b -> Map.insert (outputWire b) b a) Map.empty xs

resolve :: Int -> CircuitMap -> SignalMap -> SignalMap
resolve n cm sm
    | n == (length sm) = error "LOOP"
    | (length cm) == (length sm) = sm
    | otherwise = resolve (length sm) cm (pass cm sm)

pass :: CircuitMap -> SignalMap -> SignalMap
pass cm sm = Map.mapMaybeWithKey pass' cm 
    where
        pass' k a
            | Map.member k sm = Just (sm Map.! k)
            | otherwise = compute sm a

compute :: SignalMap -> Instruction -> Maybe Int
compute sm (Provide a b) = do
    x <- signal a sm
    Just x
compute sm (And a b c) = do
    x <- signal a sm
    y <- signal b sm
    Just (x Bits..&. y)
compute sm (Or a b c) = do
    x <- signal a sm
    y <- signal b sm
    Just (x Bits..|. y)
compute sm (Not a b) = do
    x <- signal a sm
    Just ((Bits.complement x) Bits..&. 0xffff)
compute sm (LShift a b c) = do
    x <- signal a sm
    y <- signal b sm
    Just ((Bits.shiftL x y) Bits..&. 0xffff)
compute sm (RShift a b c) = do
    x <- signal a sm
    y <- signal b sm
    Just (Bits.shiftR x y)
compute _ _ = error "Nope"

signal :: String -> SignalMap -> Maybe Int
signal k sm
    | isDigit (head k) = Just (read k::Int)
    | otherwise = Map.lookup k sm

day7 :: IO ()
day7 = do
    input <- slurpLines "day7.txt"
    let instructions = parseLines input
    let circuitMap = assemble instructions
    let answer1 = (resolve (-1) circuitMap Map.empty) Map.! "a"
    print $ "part 1: " ++ (show answer1)
    let modifiedCircuitMap = Map.insert "b" (Provide (show answer1) "b") circuitMap
    let answer2 = (resolve (-1) modifiedCircuitMap Map.empty) Map.! "a"
    print $ "part 2: " ++ (show answer2)
