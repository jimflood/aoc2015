module Day18
    ( day18
    ) where

import Lib
import qualified Data.Set as Set

type Bulb = (Int, Int)

type BulbSet = Set.Set Bulb

type Grid = ((Int, Int), BulbSet)

parseLines :: [String] -> Grid
parseLines xs = (bounds, gridify)
    where
        bounds = (length (head xs), length xs)
        gridify = Set.fromList $ concatMap ( \ (y, t) -> [(x, y) | x <- map fst t]) $ zip [0..] $ map (\ x -> filter ( \ a -> snd a == '#') x) $ map ( \ x -> zip [0..] x) xs

neighbors :: Bulb -> [Bulb]
neighbors (x, y) = [(nx, ny) | nx <- [(x - 1)..(x + 1)], ny <- [(y - 1)..(y + 1)], (nx, ny) /= (x, y)]

domain :: Grid -> [Bulb]
domain g = [(x, y) | x <- [0..(fst (fst g) - 1)], y <- [0..(snd (fst g) - 1)]]

state :: Grid -> Bulb -> Bool
state g a = Set.member a (snd g)

step :: BulbSet -> Grid -> Grid
step stuckOn g = (fst g, Set.fromList (filter newState (domain g)))
    where
        newState (x, y) = neighborOnCount == 3 || (state g (x, y) && neighborOnCount == 2) || Set.member (x, y) stuckOn
            where
                neighborOnCount = length $ filter ( \ a -> state g a || Set.member a stuckOn) (neighbors (x, y))

run :: BulbSet -> Int -> Grid -> Grid
run _ 0 g = g
run stuckOn n g = run stuckOn (n - 1) (step stuckOn g)

corners :: Grid -> BulbSet
corners g = corners' (fst (fst g)) (snd (fst g))
    where
        corners' x y = Set.fromList [(0, 0), (x - 1, 0), (0, y - 1), (x - 1, y - 1)]

-- draw :: Grid -> String
-- draw g = concatMap ( \ y -> (concatMap ( \ x -> if Set.member (x, y) (snd g) then "#" else ".") [0..(fst (fst g) - 1)]) ++ "\n") [0..(snd (fst g) - 1)]

day18 :: IO ()
day18 = do
    grid <- parseLines <$> slurpLines "day18.txt"
    let numberOfSteps = 100
    let answer1 = length $ snd (run Set.empty numberOfSteps grid)
    print $ "part 1: " ++ (show answer1)
    let answer2 = length $ snd (run (corners grid) numberOfSteps grid)
    print $ "part 2: " ++ (show answer2)
