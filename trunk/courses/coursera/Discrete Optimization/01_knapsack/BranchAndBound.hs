module BranchAndBound (bbSolve) where

import Debug.Trace

import Knapsack
import Test
import Data.Maybe
import Data.List


bbSolve :: [Integer] -> (Integer, [Integer])
bbSolve = solve bbSolve'

{- main solver -}
bbSolve' :: Integer -> Weight -> [Item] -> (Integer, [Integer])
bbSolve' total k items = build total solution
    where solution = knapsack3 k items

{- builds ready-to-submit output from the solution -}
build :: Integer -> Maybe (Value, Value, [Item]) -> (Integer, [Integer])
build total (Just (best, _, items)) = (best, table total items)
build total Nothing = (0, replicate (fromIntegral total) 0)


{- building 0/1 table (taking/not taking) from the list of items -}
table :: Integer -> [Item] -> [Integer]
table total items = table' 0 total (sort items)

table' :: Integer -> Integer -> [Item] -> [Integer]
table' cnt total [] = replicate times 0
    where times = fromIntegral (total - cnt)
table' cnt total items@(item:rest) 
    | cnt == number = 1 : (table' (cnt + 1) total rest)
    | otherwise = 0 : (table' (cnt + 1) total items)
    where (number, _, _) = item

{- optimistic solution when we relax capacity constraint -}
takeAll :: Weight -> [Item] -> Value
takeAll _ items = foldr ((+) . second) 0 items
    where second (_, x, _) = x



{- optimistic solution when we do integrity relaxation -}
takeGreedy :: Weight -> [Item] -> Value
takeGreedy k items = takeGreedy' k sorted 0
    where sorted = sortBy itemComparator items

takeGreedy' _ [] acc = acc
takeGreedy' k (item:rest) acc  
    | k >= weight = takeGreedy' (k - weight) rest (acc + value)
    | otherwise = acc + (toInteger (round ((fromIntegral k) * perKg)))
    where (_, value, weight) = item
          perKg = value_per_weight item


value_per_weight :: Item -> Double
value_per_weight (_, v, w) = (fromIntegral v) / (fromIntegral w)

itemComparator :: Item -> Item -> Ordering
itemComparator i1 i2 = compare (-val1) (-val2)
    where val1 = value_per_weight i1 
          val2 = value_per_weight i2


{- for pring-debugging -}
-- debug
--knapsack' calc_opt items k best acc_val acc_opt acc_items = 
--    traceShow (items, k, best, acc_val, acc_opt, acc_items, "->", result) $ result
--        where result = knapsack calc_opt items k best acc_val acc_opt acc_items
knapsack' = knapsack


{- actual depth-first branch and bound algo -}
knapsack :: (Weight -> [Item] -> Value) -> [Item] -> Weight -> Value -> Value -> Value -> [Item] -> Maybe (Value, Value, [Item])
knapsack _ [] k best acc_val acc_opt acc_items 
    | k < 0 || best > acc_opt = Nothing
    | otherwise = Just (acc_val, acc_opt, acc_items)

knapsack calc_opt items k best acc_val acc_opt acc_items = 
    if (k < 0) then Nothing else 
    if (weight <= k) then
        if (new_opt <= new_best) then
            left
        else 
            something
    else 
        something
    where item:rest = items
          (_, value, weight) = item
          left = knapsack' calc_opt rest (k - weight) best (acc_val + value) acc_opt (item : acc_items)
          new_opt = calc_opt k (rest ++ acc_items)
          best_from_left = let Just (_, s, _) = left in s
          new_best = if isNothing left then best else max best best_from_left
          right = knapsack' calc_opt rest k new_best acc_val new_opt acc_items
          something = if (isNothing right) then left else right


{- wrapper -}
knapsack2 calc_opt k items = knapsack' calc_opt items k max_best 0 optimistic_solution []
    where optimistic_solution = calc_opt k items
          max_best = -1

knapsack1 = knapsack2 takeAll
knapsack3 = knapsack2 takeGreedy



test_optimisticSolution_takeAll = do
    print "test_optimisticSolution_takeAll"
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let expected = 8 + 10 + 15 + 4
    let actual = takeAll 0 items
    assertEquals actual expected 

test_lastStep_takeLeft = do
    print "test_lastStep_takeLeft"
    let capacity = 5
    let item@(_, item_value, item_weight) = (3, 4, 3)
    let acc_val = 10; acc_opt = 20
    let acc_items = [(2, 15, 8)]
    let result = knapsack' takeAll [item] capacity 0 acc_val acc_opt acc_items
    let Just (actual_value, actual_opt, actual_solution) = result
    let expected_value = acc_val + item_value
    let expected_opt = acc_opt
    let expected_solution = item : acc_items
    assertEquals expected_value actual_value
    assertEquals expected_opt actual_opt
    assertEquals actual_solution expected_solution 

test_lastStep_takeRight = do
    print "test_lastStep_takeRight"
    let capacity = 2
    let item@(_, item_value, item_weight) = (2, 15, 8)
    let acc_val = 18; acc_opt = 33
    let acc_items = [(0, 8, 4), (1, 10, 5)]
    let result = knapsack' takeAll [item] capacity 0 acc_val acc_opt acc_items
    let Just (actual_value, actual_opt, actual_solution) = result
    let expected_value = acc_val
    let expected_opt = acc_opt - item_value
    let expected_solution = acc_items
    assertEquals expected_value actual_value
    assertEquals expected_opt actual_opt
    assertEquals actual_solution expected_solution 


test_lastStep_cantFit_getNothing = do
    print "test_lastStep_cantFit_getNothing"
    let capacity = -2
    let result = knapsack' takeAll [] capacity 0 0 0 []
    assertTrue $ isNothing result



test_notTaking_bestIsBetter = do
    print "test_notTaking_bestIsBetter"
    let capacity = 2
    let item = (3, 4, 3)
    let acc_val = 10; acc_opt = 20
    let acc_items = [(2, 15, 8)]
    let best = 30
    let result = knapsack' takeAll [item] capacity best acc_val acc_opt acc_items
    assertTrue $ isNothing result


test_takingRight_bestIsNotBetter = do
    print "test_takingRight_bestIsNotBetter"
    let capacity = 2
    let item@(_, val, _) = (3, 4, 3)
    let acc_val = 10; acc_opt = 20
    let acc_items = [(2, 15, 8)]
    let best = 10
    let result = knapsack' takeAll [item] capacity best acc_val acc_opt acc_items
    let Just (actual) = result
    let expected_opt = takeAll capacity acc_items
    let expected = (acc_val, expected_opt, acc_items)
    assertEquals actual expected



test_fromLecture_prunedRight = do
    print "test_fromLecture_prunedRight"
    let capacity = 10
    let item = (2, 35, 3)
    let best = 80
    let acc_val = 0
    let acc_opt = 35
    let result = knapsack' takeAll [item] capacity best acc_val acc_opt []
    assertTrue $ isNothing result


test_fromLecture_best_already_found_prune = do
    print "test_fromLecture_best_already_found_prune"
    let capacity = 10
    let items = [(1, 48, 8), (2, 35, 3)]
    let best = 80
    let acc_val = 0
    let acc_opt = 83
    let result = knapsack' takeAll items capacity best acc_val acc_opt []
    assertTrue (isNothing result)


test_one_item_updates_best = do
    print "test_one_item_facade"
    let capacity = 5
    let item@(_, val, _) = (2, 35, 3)
    let best = -1
    let acc_val = 45
    let acc_opt = 80
    let acc_items = [(2, 35, 3), (0, 45, 5)]
    let Just result = knapsack' takeAll [item] capacity best acc_val acc_opt acc_items
    assertEquals result (acc_val + val, acc_opt, item : acc_items)

test_one_item_facade = do
    print "test_one_item_facade"
    let capacity = 5
    let item@(_, val, _) = (2, 35, 3)
    let Just result = knapsack1 capacity [item]
    assertEquals result (val, val, [item])


test_fromLecture = do
    print "test_fromLecture"
    let capacity = 10 
    let items@[item0, _, item2] = [(0, 45, 5), (1, 48, 8), (2, 35, 3)]
    let Just (actual_cost, actual_opt, actual_items) = knapsack1 capacity items
    assertEquals actual_cost 80
    assertEquals actual_opt 80
    let expected_solution = [item0, item2]
    assertEquals (sort actual_items) expected_solution

test_kn4 = do
    print "test_kn4"
    let k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let Just actual = knapsack1 k items
    let expected = (19, 19, [(3, 4, 3), (2, 15, 8)])
    assertEquals actual expected

test_greedy = do
    print "test_greedy"
    let actual = takeGreedy 10 [(0, 45, 5), (1, 48, 8), (2, 35, 3)]
    assertEquals 92 actual

test_kn4_greedy = do
    print "test_kn4_greedy"
    let k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let Just actual = knapsack3 k items
    let expected = (19, 19, [(3, 4, 3), (2, 15, 8)])
    assertEquals actual expected

test_table = do
    print "test_table"
    let total = 5
    let items = [(2, 3, 1), (0, 4, 1)]
    let expected = [1, 0, 1, 0, 0]
    let actual = table total items
    assertEquals expected actual

test_kn4_sol = do
    print "test_kn4_sol"
    let n = 4; k = 11
    let items = [(0, 8, 4), (1, 10, 5), (2, 15, 8), (3, 4, 3)]
    let actual = bbSolve' n k items
    let expected = (19, [0,0,1,1])
    assertEquals actual expected

tests = [test_optimisticSolution_takeAll, test_lastStep_takeLeft, test_lastStep_takeRight, 
         test_lastStep_cantFit_getNothing, test_notTaking_bestIsBetter, 
         test_takingRight_bestIsNotBetter, test_fromLecture_prunedRight, 
         test_fromLecture_best_already_found_prune, test_one_item_updates_best,
         test_one_item_facade, test_fromLecture, test_kn4, test_greedy,
         test_table, test_kn4_sol]


run_all = runL tests
