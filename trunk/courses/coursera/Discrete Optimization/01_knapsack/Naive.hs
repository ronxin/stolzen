module Naive (solve) where

import Test
import Knapsack

-- n, K, (index-number, value, weight) tuples
knapsackNaive :: Integer -> Weight -> [Item] -> (Integer, [Integer])
knapsackNaive _ _ [] = (0, [])
knapsackNaive n k ((number, value, weight):rest) = 
    if (weight <= k) then 
        if (solution_when_taking + value > solution_when_not) then
            taking
        else
            not_taking
    else not_taking
        where
            (solution_when_taking, prev_items_with) = knapsackNaive n (k - weight) rest
            (solution_when_not, prev_items_without) = knapsackNaive n k rest
            not_taking = (solution_when_not, 0 : prev_items_without)
            taking = (solution_when_taking + value, 1 : prev_items_with)

test_kn4_naive = do
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = knapsackNaive n k items
    let expected = (19, [0,0,1,1])
    assertEquals actual expected
