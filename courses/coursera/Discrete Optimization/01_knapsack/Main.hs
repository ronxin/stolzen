module Main where

import DynamicProgramming(dpSolve)


s2i :: String -> Integer
s2i = read

i2s :: Integer -> String
i2s = show

readInput :: String -> [Integer]
readInput input = map s2i (words input)

join :: [Integer] -> String
join = tail . concatMap ((' ':) . i2s)

writeOutput :: (Integer, [Integer]) -> String
writeOutput (x, out) = i2s x ++ " 1\n" ++ join out


main = do
    input <- getContents
    putStr $ writeOutput $ dpSolve $ readInput input