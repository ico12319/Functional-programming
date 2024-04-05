--Define a function that accepts a number and returns the tuple (x, y) 
--where x is the sum of the digits on even indices 
--of the number and y - the sum of the ones on odd indices

import Data.List
import Data.Char

main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)


checkNumber :: Int -> (Int,Int)
checkNumber x = (sumWithPred even , sumWithPred odd)
 where
    digits =  map digitToInt $ show x
    sumWithPred predicate = sum $ map snd $ filter (predicate.fst) $  zip [0..] digits