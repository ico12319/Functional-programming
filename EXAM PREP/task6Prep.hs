--Define a function that returns the sum of the first n prime numbers that contain a digit d.


main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462



isPrime :: Int -> Bool
isPrime x = [1,x] == filter(\d -> mod x d == 0) [1..x]

containsDigit :: Int -> Int -> Bool 
containsDigit num digit = elem (head (show digit)) (show num)

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes x y = sum $ take x (filter(\z -> isPrime z && containsDigit z y) [1..])