main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990
    


sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + (sumDigits $ div n 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish) k
 where
    helper :: Int -> Int -> Int -> Int
    helper rStart rFinish divisor
     | rStart > rFinish = 0
     | mod (sumDigits rStart) divisor == 0 = rStart + helper (rStart + 1) rFinish divisor
     | otherwise = helper (rStart + 1) rFinish divisor