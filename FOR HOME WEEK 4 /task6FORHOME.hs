import Data.Char

main :: IO()
main = do

    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime x = [1, x] == filter (\y -> mod x y == 0) [1 .. x]


toList :: Int -> [Int]
toList = map digitToInt . show

containsDigit :: Int -> Int -> Bool
containsDigit n t = elem t (toList n)


sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n t = sum $ take n $ filter (\x -> containsDigit x t && isPrime x) [1..]
