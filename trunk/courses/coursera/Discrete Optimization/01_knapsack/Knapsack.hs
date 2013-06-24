module Knapsack (groupItems, solve, ItemNumber, Weight, Value, Item) where

import Test

type ItemNumber = Integer
type Weight = Integer
type Value = Integer
type Item = (ItemNumber, Value, Weight)


groupItems :: [Integer] -> [Item]
groupItems lst = groupItems' [0..] lst

groupItems' :: [Integer] -> [Integer] -> [Item]
groupItems' _ [] = []
groupItems' (i:indexes) (v:w:items) = (i, v, w) : (groupItems' indexes items)


solve :: (Integer -> Weight -> [Item] -> (Integer, [Integer])) -> [Integer] -> (Integer, [Integer])
solve solver (n:k:rest) = solver n k (groupItems rest)

