module Hex (
	hexByteStringToInt,
	stringToAscii,
	listXor,
	writeIntListToHexString,
  map2,
  decryptBinary
) where

import Data.List
import Data.Maybe
import Data.Char
import Data.Bits

alphabet = "0123456789abcdef"

-- returns character for given numerical toValue
toLetter :: Int -> Char
toLetter = (alphabet !!)
-- toLetter = digitToInt

-- returns numerical value for given character
toValue :: Char -> Int
toValue char = x where (Just x) = elemIndex char alphabet
-- toValue = intToDigit


{-- helper function for stringToInt
parameters
-- posix, 
-- index of currently processed element, 
-- accumulated toValue
-- reversed list to parse
--}
stringToInt' :: Int -> Int -> Int -> String -> Int
stringToInt' _ _ acc [] = acc
stringToInt' posix currentIndex acc (x:xs) = stringToInt' posix (currentIndex + 1) (acc + ele * posix ^ currentIndex) xs
    where ele = toValue x

-- converts from string representation to numerical
stringToInt :: Int -> String -> Int
stringToInt posix val = stringToInt' posix 0 0 (reverse val)

hexToInt :: String -> Int
hexToInt = stringToInt 16

binToInt :: String -> Int
binToInt = stringToInt 2


-- helper function for intToString
-- returns reversed string representation of the number
-- parameters: posix, number to convert
intToString' :: Int -> Int -> String
intToString' posix num = case (num `divMod` posix) of 
    (0, m) -> [(toLetter m)]
    (d, m) -> (toLetter m) : (intToString' posix d)


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

intToBin :: Int -> String
intToBin = intToString 2

intToBinByte = (ensureLen 8 '0') . intToBin

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

asciiToString :: [Int] -> String
asciiToString input = map chr input


map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f l1 l2 = map (uncurry f) (zip l1 l2)

-- element-wise xor for list
listXor :: [Int] -> [Int] -> [Int]
listXor = map2 xor


charxor :: Char -> Char -> Char
charxor a b = chr ((ord a) `xor` (ord b))



-- function that writes byte list to hex string
writeIntListToHexString = concatMap intToHexByte


hexXor :: String -> String -> String
hexXor s1 s2 = writeIntListToHexString $ h1 `listXor` h2
  where h1 = hexByteStringToInt s1
        h2 = hexByteStringToInt s2

-- cipher text, key -> result
decrypt :: String -> [Int] -> String
decrypt hex key = decryptBinary (hexByteStringToInt hex) key

decryptBinary :: [Int] -> [Int] -> String
decryptBinary bytes key = asciiToString (key `listXor` bytes)

