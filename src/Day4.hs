
module Day4
    ( day4
    ) where

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.UTF8 as BL

hash :: String -> Int -> String
hash s n = show $ md5 (BL.fromString (s ++ (show n)))

solve :: String -> String -> Int
solve k s = part1' 1
    where
        part1' :: Int -> Int
        part1' n = part1'' $ take (length k) (hash s n)
            where
                part1'' s
                    | s == k = n
                    | otherwise = part1' (n + 1)

day4 :: IO ()
day4 = do
    let input = "iwrupvqb"
    let answer1 = solve "00000" input
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve "000000" input
    print $ "part 2: " ++ (show answer2)
