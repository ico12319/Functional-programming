import Data.Char

main :: IO()
main = do

    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)



checkNumber :: Int -> (Int,Int)
checkNumber num = (sumEven,sumOdd)
 where
    sumEven = sum $ map digitToInt $ map snd $ filter (even . fst) $ zip [0..] $ show num
    sumOdd = sum $ map digitToInt $ map snd $ filter (odd . fst) $ zip [0..] $ show num
