module Test (
      run
    , runL
    , assertEquals
    , assertTrue
    , assertFalse
) where




runL tests = mapM run tests

run test = do 
        test
        return "OK"

assertEquals :: Eq a => Show a => a -> a -> IO ()
assertEquals actual expected 
    | actual == expected = return ()
    | otherwise = error $ "assertEquals: " ++ (show actual) ++ " /= " ++ (show expected)

assertTrue :: Bool -> IO ()
assertTrue value 
    | True = return ()
    | False = error $ "assertTrue: expected True got False"

assertFalse :: Bool -> IO ()
assertFalse value 
    | False = return ()
    | True = error $ "assertFalse: expected False got True"