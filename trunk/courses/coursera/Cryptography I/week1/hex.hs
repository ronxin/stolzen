module Hex where

import Data.List
import Data.Maybe
import Data.Char
import Data.Bits

alphabet = "0123456789abcdef"

-- returns character for given numerical value
letter :: Int -> Char
letter = (alphabet !!)
-- letter = digitToInt

-- returns numerical value for given character
value :: Char -> Int
value char = x where (Just x) = elemIndex char alphabet
-- value = intToDigit


{-- helper function for stringToInt
parameters
-- posix, 
-- index of currently processed element, 
-- accumulated value
-- reversed list to parse
--}
stringToInt' :: Int -> Int -> Int -> String -> Int
stringToInt' _ _ acc [] = acc
stringToInt' posix currentIndex acc (x:xs) = stringToInt' posix (currentIndex + 1) (acc + ele * posix ^ currentIndex) xs
    where ele = value x

-- converts from string representation to numerical
stringToInt :: Int -> String -> Int
stringToInt posix val = stringToInt' posix 0 0 (reverse val)

hexToInt :: String -> Int
hexToInt = stringToInt 16


-- helper function for intToString
-- returns reversed string representation of the number
-- parameters: posix, number to convert
intToString' :: Int -> Int -> String
intToString' posix num = case (num `divMod` posix) of 
    (0, m) -> [(letter m)]
    (d, m) -> (letter m) : (intToString' posix d)


-- needed len, char to use for padding, string to pad
ensureLen :: Int -> Char -> String -> String
ensureLen len ch input | actualLen > len = error "ensureLen: list is longer than needed"
                       | actualLen < len = (replicate (len - actualLen) ch) ++ input
                       | otherwise = input
                where actualLen = (length input)

fitToHexByte = ensureLen 2 '0'

-- converts from numerical representation to string
intToString :: Int -> Int -> String
intToString posix num = reverse (intToString' posix num)


intToHex :: Int -> String
intToHex = intToString 16


-- makes sure the output string is 2-chars long and padded with 0
intToHexByte :: Int -> String
intToHexByte = fitToHexByte . intToHex

-- elements per bucket, list
buckets :: Int -> [a] -> [[a]]
buckets _ [] = []
buckets amt list = (ensureLen head) : (buckets amt tail)
    where (head, tail) = splitAt amt list
          ensureLen l = if (length l) == amt then l
                        else error "buckets: can't split into buckets"

-- posix, places per number, input
stringListToInt :: Int -> Int -> String -> [Int]
stringListToInt posix places input = map (stringToInt posix) (buckets places input)

hexByteStringToInt :: String -> [Int]
hexByteStringToInt = stringListToInt 16 2

-- hexByteStringToInt "6c73d5240a948c86981bc294814d"
-- [108,115,213,36,10,148,140,134,152,27,194,148,129,77]


-- converts string into ASCII 
stringToAscii :: String -> [Int]
stringToAscii input = map ord input


map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f l1 l2 = map (uncurry f) (zip l1 l2) 

-- element-wise xor for list
listXor :: [Int] -> [Int] -> [Int]
listXor = map2 xor


-- actual assignment
cipherText = hexByteStringToInt "09e1c5f70a65ac519458e7e53f36"
message = stringToAscii "attack at dawn"

key = listXor cipherText message

newMessage = stringToAscii "attack at dusk"
newCipherText = listXor key newMessage

ctHex = concatMap intToHexByte newCipherText
-- toHex :: [Int] -> String
