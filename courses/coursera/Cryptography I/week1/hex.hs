module Hex where

alphabet = "0123456789abcdef"
letter :: Int -> Char
letter pos = alphabet !! pos


intToString' :: Int -> Int -> String
intToString' posix num = case (num `divMod` posix) of 
    (0, m) -> [(letter m)]
    (d, m) -> (letter m) : (intToString' posix d)     

intToString :: Int -> Int -> String
intToString posix num = rev (intToString' posix num)

rev' :: [a] -> [a] -> [a]
rev' [] res = res
rev' (x:s) res = rev' s (x : res)

rev :: [a] -> [a]
rev l = rev' l []

-- toHex :: [Int] -> String
