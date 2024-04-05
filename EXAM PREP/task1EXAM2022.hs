-- Да се дефинира функция primesProd :: Int -> Int,
-- която за дадено неотрицателно
-- цяло число 𝑥 връща произведението на простите числа,
-- по-малки от √𝑥. Да се
-- реализира линейно рекурсивен процес.

main :: IO()
main = do
    print $ primesProd 9 == 2
    print $ primesProd 12 == 6
    print $ primesProd 1200 == 200560490130
    print $ isPrime 12
    print $ isPrime 11
    print $ isPrime 47
    


isPrime :: Int -> Bool
isPrime n = [1,n] == filter (\d -> mod n d == 0) [1..n]


primesProd :: Int -> Int
primesProd n
 | n < 0 = error "The number should be a positive Integer"
 | otherwise = helper 1
  where
    helper :: Int -> Int
    helper currNum
     | currNum * currNum >= n = 1
     | isPrime currNum = currNum * helper (currNum + 1) 
     | otherwise = helper(currNum + 1)