module BranchAndBound where

import Debug.Trace

import Knapsack
import Test
import Data.Maybe
import Data.List





--           calc_opt          -> items  -> k      -> acc_val> acc_opt> acc_items
knapsack' :: ([Item] -> Value) -> [Item] -> Weight -> Value -> Value -> [Item] -> Maybe (Value, Value, [Item])
knapsack' _ [] k acc_value acc_opt acc_items 
    | k > 0 = Just (acc_value, acc_opt, acc_items)
    | otherwise = Nothing

knapsack' calc_opt ((item@(_, value, weight)):rest) k acc_value acc_opt acc_items
    | weight <= k = traceShow ("left") $ left
    | new_opt < left_opt = traceShow ("pruning", new_opt, left_opt) $   Nothing
    | weight > k = traceShow ("right", new_opt, left_opt) $   right
    | otherwise = traceShow ("otherwise", new_opt, left_opt) $ Nothing
    where 
        left =
            knapsack'' calc_opt rest (k - weight) (acc_value + value) acc_opt (item : acc_items)
        left_opt = 
            if (isNothing left) then acc_opt else (let Just (_, s, _) = left in s)
        new_opt = 
            calc_opt (rest ++ acc_items)
        right =
            knapsack'' calc_opt rest k acc_value new_opt acc_items

knapsack calc_opt k items = knapsack'' calc_opt items k 0 optimistic_solution []
    where optimistic_solution = calc_opt items

-- debug
knapsack'' calc_opt items k acc_value acc_opt acc_items = 
    traceShow (items, k, acc_value, acc_opt, acc_items, "->", result) $ result
        where result = knapsack' calc_opt items k acc_value acc_opt acc_items

knapsackTakeAllOpt = knapsack takeAll


takeAll :: [Item] -> Value
takeAll items = foldr ((+) . second) 0 items
    where second (_, x, _) = x


test_optimisticSolution_takeAll = do
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let expected = 8 + 10 + 15 + 4
    let actual = takeAll items
    assertEquals actual expected 

test_lastStep_takeLeft = do
    let capacity = 5
    let item@(_, item_value, item_weight) = (3, 4, 3)
    let acc_val = 10; acc_opt = 20
    let acc_items = [(2, 15, 8)]
    let Just (actual_value, actual_opt, actual_solution) = knapsack' takeAll [item] capacity acc_val acc_opt acc_items
    let expected_value = acc_val + item_value
    let expected_opt = acc_opt
    let expected_solution = item : acc_items
    assertEquals expected_value actual_value
    assertEquals expected_opt actual_opt
    assertEquals actual_solution expected_solution 

test_lastStep_cantFit_getNothing = do
    let capacity = 2
    let item@(_, item_value, item_weight) = (3, 4, 3)
    let acc_val = 10; acc_opt = 20
    let acc_items = [(2, 15, 8)]
    let result = knapsack' takeAll [item] capacity acc_val acc_opt acc_items
    assertTrue $ isNothing result

test_lastStep_takeRight = do
    let capacity = 2
    let item@(_, item_value, item_weight) = (2, 15, 8)
    let acc_val = 18; acc_opt = 33
    let acc_items = [(0, 8, 4), (1, 10, 5)]
    let Just (actual_value, actual_opt, actual_solution) = knapsack' takeAll [item] capacity acc_val acc_opt acc_items
    let expected_value = acc_val
    let expected_opt = acc_opt - item_value
    let expected_solution = acc_items
    assertEquals expected_value actual_value
    assertEquals expected_opt actual_opt
    assertEquals actual_solution expected_solution 

test_fromLecture = do
    let capacity = 10 
    let items@[item0, _, item2] = [(0, 45, 5), (1, 48, 8), (2, 35, 3)]
    let Just (actual_cost, actual_opt, actual_items) = knapsackTakeAllOpt capacity items
    assertEquals actual_cost 80
    assertEquals actual_opt 80
    let expected_solution = [item0, item2]
    assertEquals (sort actual_items) expected_solution

test_k4_left = do -- taking the first
    let k = 7 
    let items = [(1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let acc_items = [(0, 8, 4)]
    let acc_val = 8
    let acc_opt = 37
    let result = knapsack'' takeAll items k acc_val acc_opt acc_items
    let Just (actual_cost, actual_opt, actual_items) = result
    assertEquals actual_items ((1, 10, 5) : acc_items)
    assertEquals actual_cost 18
    assertEquals actual_opt 18

test_k4_right = do -- not taking the first
    let k = 11
    let items = [(1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let acc_items = []
    let acc_val = 0
    let max_from_left = 18
    let acc_opt = max_from_left
    let result = knapsack'' takeAll items k acc_val acc_opt acc_items
    let Just (actual_cost, actual_opt, actual_items) = result
    assertEquals actual_cost 19
    assertEquals actual_opt 18
    assertEquals actual_items [(3, 4, 3), (2, 15, 8)]

test_kn4 = do
    let k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let Just (actual_cost, _, actual_items) = knapsackTakeAllOpt k items
    assertEquals actual_cost 19
    assertEquals [(2, 15, 8), (3, 4, 3)] actual_items




tests = [test_optimisticSolution_takeAll, test_lastStep_takeLeft, 
         test_lastStep_cantFit_getNothing, test_fromLecture,
         test_lastStep_takeRight]
run_all = runL tests