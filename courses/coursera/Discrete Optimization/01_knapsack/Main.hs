module Main where

import Knapsack(solve)


s2i :: String -> Integer
s2i = read

i2s :: Integer -> String
i2s = show

readInput :: String -> [Integer]
readInput input = map s2i (words input)

writeOutput :: [Integer] -> String
writeOutput = tail . concatMap ((' ':) . i2s)


main = do
    input <- getContents
    putStr $ writeOutput $ solve $ readInput input