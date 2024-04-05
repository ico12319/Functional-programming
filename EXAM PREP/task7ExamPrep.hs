
import Data.Char

main :: IO()
main = do
    print $ reverseOrdSuff 37563
    print $ reverseOrdSuff 32763
    print $ reverseOrdSuff 32666




digitsCount :: Int -> Int
digitsCount num = helper num 0
 where
    helper :: Int -> Int -> Int
    helper num count
     | num == 0 = count 
     | otherwise = helper (div num 10) (count + 1) 



rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)



reverseOrdSuff :: Int -> Int
reverseOrdSuff num = helper num 0 0 0
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper num currMax rMax iters
     | iters == (digitsCount num) - 1 = div (rev num) (10 ^ rMax)
     | mod num 10 < mod num 100 = helper (div num 10) (currMax + 1) (max currMax rMax) (iters + 1)
     | otherwise = helper (div num 10) 0 (max currMax rMax) (iters + 1)
