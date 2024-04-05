--Define a function that reverses a non-negative number.
import Data.List
import Data.Char

main :: IO()
main = do
    print $ rev' 1  == 1
    print $ rev' 123  == 321
    print $ rev' 987654321 == 123456789  

rev :: Int -> Int 
rev =  read . reverse . show  


rev' :: Int -> Int
rev' n = helper 0 n
 where
    helper :: Int -> Int -> Int
    helper result number
     | number == 0 =result
     | otherwise = helper (result * 10 + mod number 10) (div number 10)