import Data.Char

main :: IO()
main = do

    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)
    print $ checkNumber 444 == (8,4) --myTest



checkNumber :: Int -> (Int, Int)
checkNumber num = (sumWithPredicate even, sumWithPredicate odd)
  where
    digits = map digitToInt $ show num
    sumWithPredicate predicate = sum $ map snd $ filter (predicate . fst) $ zip [0..] digits