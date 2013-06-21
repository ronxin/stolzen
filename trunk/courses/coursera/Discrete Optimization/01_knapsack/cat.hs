
import Control.Monad  
import System.Environment

--join :: String -> [String] -> String
--join _ [] = []
--join _ (x:[]) = x
--join sep (x:xs) = x ++ (sep ++ (join sep xs) )

--joiner = join ", "

main = do  
    args <- getArgs
    if (null args) then 
        catStd 
    else 
        catFiles args

catStd :: IO ()
catStd = do
    contents <- getContents  
    putStr contents

catFiles :: [String] -> IO ()
catFiles files = do
    forM_ files (\f -> do
        contents <- readFile f
        putStr contents)