module Day14
    ( day14
    ) where

import Lib
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Map as Map

data Spec = Spec { kms :: Int, fly :: Int, rest :: Int } deriving (Show)

type SpecMap = Map.Map String Spec

type Distance = Int

type Flying = Int

type Resting = Int

type Reindeer = (Distance, Either Flying Resting)

type ReindeerMap = Map.Map String Reindeer

type ScoreMap = Map.Map String Int

parseLines :: [String] -> SpecMap
parseLines xss = Map.fromList (map parseLine xss)
    where
        parseLine s = parseLine' (splitOn " " s)
        parseLine' [a, "can", "fly", b, "km/s", "for", c, "seconds,", "but", "then", "must", "rest", "for", d, "seconds."] = parseLine'' a b c d
        parseLine' _ = error "Huh?"
        parseLine'' a b c d = (a, Spec { kms=read b::Int, fly=read c::Int, rest=read d::Int })

start :: SpecMap ->  ReindeerMap
start sm = Map.fromList $ map ( \ (k, v) -> (k, (0, Left (fly v)))) (Map.toList sm)

step :: SpecMap -> ReindeerMap -> ReindeerMap
step sm rm = Map.mapWithKey step' rm
    where
        step' k (d, Left flying) 
            | flying > 1 = (d + (kms (sm Map.! k)), Left (flying - 1))
            | otherwise = (d + (kms (sm Map.! k)), Right (rest (sm Map.! k)))
        step' k (d, Right resting)
            | resting > 1 = (d, Right (resting - 1))
            | otherwise = (d, Left (fly (sm Map.! k)))

run :: SpecMap -> ReindeerMap -> Int -> ReindeerMap
run sm rm secs = run' 0 rm
    where
        run' n newRm
            | n == secs = newRm
            | otherwise = run' (n + 1) (step sm newRm)

run2 :: SpecMap -> ReindeerMap -> Int -> ScoreMap
run2 sm rm secs = run2' 0 rm (Map.map ( \ _ -> 0) rm)
    where
        run2' n newRm points
            | n == secs = points
            | otherwise = run2'' (step sm newRm) points
                where
                    run2'' newNewRm newPoints = run2' (n + 1) newNewRm (score newNewRm newPoints (maximum (map (fst . snd) (Map.toList newNewRm))))
                    score m p mx = Map.mapWithKey update p
                        where
                            update k a
                                | fst (m Map.! k) == mx = (a + 1)
                                | otherwise = a

day14 :: IO ()
day14 = do
    sm <- parseLines <$> slurpLines "day14.txt"
    let results = run sm (start sm) 2503
    let answer1 = fst $ snd $ head $ reverse $ sortOn (fst . snd) $ Map.toList results
    print $ "part 1: " ++ (show answer1)
    let results2 = run2 sm (start sm) 2503
    let answer2 = maximum $ map snd $ Map.toList results2
    print $ "part 2: " ++ (show answer2)
