module DynamicProgramming (dpSolve) where 

import Knapsack
import Test


dpSolve :: [Integer] -> (Integer, [Integer])
dpSolve = solve dynKnapsackBuild

{-
For building optimal solution uses a helper function knapsackPutItem'
algo: we decide what is better at each step
    if it's better not to take an item, then previous optimal solution is used
    if we take, then the new optimal solution is calculated:
        value of the current item + value of the optimal knapsack without this item,
        and with capacity (k - weight of the current item)

    for faster calculation when to take an item, the special list is created by padding the previous 
    optimal solutions with zeros. This way we can easily access the previous optimal solution for 
    weight := weight - current item's weight

Parameters: capacity, previous optimal solution, current item to add
-}
knapsackPutItem :: Integer -> [Value] -> Item -> [Value]
knapsackPutItem k prev item@(_, _, w) =
    knapsackPutItem' [0..k] prev prev_shifted item
        where prev_shifted = replicate (fromIntegral w) 0 ++ prev


{- 
Helper function that actually builds the next optimal solution
Parameters: current weight, value if taking, value if not taking, current item
-}
knapsackPutItem' :: [Weight] -> [Value] -> [Value] -> Item -> [Value]
knapsackPutItem' _ [] _ _ = []
knapsackPutItem' (k:weighs) (value_if_not_taking:tail1) (prev_value_if_taking:tail2) 
                    item@(number, current_value, weight) =
    best_solution : (knapsackPutItem' weighs tail1 tail2 item)
        where
            best_solution = 
                if (weight <= k) then 
                    max (current_value + prev_value_if_taking) value_if_not_taking 
                else 
                    value_if_not_taking


dynKnapsack :: Integer -> Value -> [Item] -> [[Value]]
dynKnapsack n k items = dynKnapsack' n k items [initial_solution]
    where initial_solution = replicate (fromIntegral (k + 1)) 0


dynKnapsack' :: Integer -> Value -> [Item] -> [[Value]] -> [[Value]]
dynKnapsack' _ _ [] solution = solution
dynKnapsack' n k (item:rest) solution@(previous:_) = 
    dynKnapsack' n k rest (current_solution : solution)
    where current_solution = knapsackPutItem k previous item


dynKnapsackOptimal :: Integer -> Value -> [Item] -> Value
dynKnapsackOptimal n k items = last $ head $ dynKnapsack n k items

dynKnapsackBuild :: Integer -> Weight -> [Item] -> (Integer, [Integer])
dynKnapsackBuild n k items = (optimal_solution, traceBack table items)
    where 
        optimal_solution = dynKnapsackOptimal n k items
        table = dynKnapsack n k items



{-
builds solution by tracing back

since the table is already reversed, reversing each column inside it for quicker access via head
also reverting items, as they are in the correct order

once  we're done, reverting the result so the user gets the answer in the right order

Parameters: reversed table, list of items
-}
traceBack :: [[Value]] -> [Item] -> [Integer]
traceBack table items = reverse $ traceBack' reversed_table (reverse items)
    where reversed_table = map reverse table

{-
Helper function for traceBack

Parameters: reversed table with each column also reverted, reverted list of items
-}
traceBack' :: [[Value]] -> [Item] -> [Integer]
traceBack' (line1:[]) _ = []
traceBack' list@((el:_):rest) it@((_, _, w):items) = 
    if (el == next_el) then
        0 : traceBack' rest items
    else 
        1 : traceBack' (map (drop weight) rest) items
    where next_el = head (head rest)
          weight = fromIntegral w





test0 = do
    let k = 11
    let item = (0, 8, 4)
    let initial_solution = (replicate (fromIntegral (k + 1)) 0)
    let actual = knapsackPutItem k initial_solution item
    let expected = (replicate 4 0) ++ (replicate 8 8) 
    assertEquals actual expected

test1 = do
    let k = 11
    let item = (1, 10, 5)
    let previous = (replicate 4 0) ++ (replicate 8 8) 
    let actual = knapsackPutItem k previous item
    let expected = [0,0,0,0,8,10,10,10,10,18,18,18]
    assertEquals actual expected

test2 = do
    let k = 11
    let item = (3, 15, 8)
    let previous = [0,0,0,0,8,10,10,10,10,18,18,18]
    let actual = knapsackPutItem k previous item
    let expected = [0,0,0,0,8,10,10,10,15,18,18,18]
    assertEquals actual expected

test3 = do
    let k = 11
    let item = (3, 4, 3)
    let actual = knapsackPutItem k [0,0,0,0,8,10,10,10,15,18,18,18] item
    let expected = [0,0,0,4,8,10,10,12,15,18,18,19]
    assertEquals actual expected

test_kn4_table = do
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = dynKnapsack n k items
    let expected = [
            [0,0,0,4,8,10,10,12,15,18,18,19],
            [0,0,0,0,8,10,10,10,15,18,18,18],
            [0,0,0,0,8,10,10,10,10,18,18,18],
            [0,0,0,0,8,8,8,8,8,8,8,8],
            [0,0,0,0,0,0,0,0,0,0,0,0]]
    assertEquals actual expected

test_kn4_optimal = do
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = dynKnapsackOptimal n k items
    assertEquals actual 19

test_kn4_traceback = do
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let table = [
            [0,0,0,4,8,10,10,12,15,18,18,19],
            [0,0,0,0,8,10,10,10,15,18,18,18],
            [0,0,0,0,8,10,10,10,10,18,18,18],
            [0,0,0,0,8,8,8,8,8,8,8,8],
            [0,0,0,0,0,0,0,0,0,0,0,0]]
    let expected = [0,0,1,1]
    let actual = traceBack table items
    assertEquals actual expected

test_kn4 = do
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = dynKnapsackBuild n k items
    let expected = (19, [0,0,1,1])
    assertEquals actual expected

test_kn4_dyna = do
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = dynKnapsackBuild n k items
    let expected = (19, [0,0,1,1])
    assertEquals actual expected





tests = [test0, test1, test2, test3, test_kn4_optimal, test_kn4_optimal, test_kn4_traceback]
run_all = runL tests