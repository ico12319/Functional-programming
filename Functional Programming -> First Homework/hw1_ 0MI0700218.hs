main :: IO()
main = do
    print $ numStepCombinations 2 == 2
    print $ numStepCombinations 3 == 3
    print $ numStepCombinations 100 == 573147844013817084101

    print $ maxPersistenceMinSum 273 392 == 355
    print $ maxPersistenceMinSum 1000 2000 == 1679
    print $ maxPersistenceMinSum 55 105 == 77
    print $ maxPersistenceMinSum 195 756 == 679
    print  $ maxPersistenceMinSum 2 85 == 77

-- Task 1


numStepCombinations :: Integer -> Integer
numStepCombinations 0 = 1
numStepCombinations 1 = 1
numStepCombinations 2 = 2
numStepCombinations n = helper 2 1 1 n
 where
    helper :: Integer -> Integer -> Integer -> Integer -> Integer
    helper d x y stepsLeft
     | stepsLeft == 0 = y
     | otherwise = helper (d+x) d x (stepsLeft-1)


-- Task 2

multiplyDigits :: Int -> Int
multiplyDigits n
 | n < 10 = n
 | otherwise = mod n 10 * multiplyDigits (div n 10)


validPersistance :: Int -> Int
validPersistance n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper num count
     | num < 10 = count
     | otherwise = helper (multiplyDigits num) (count + 1)


sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)


maxPersistenceMinSum :: Int -> Int -> Int 
maxPersistenceMinSum x y = helper x y x
 where
    helper :: Int -> Int -> Int -> Int  -- "Some parts of the function were written with the help of 0MI0700220"
    helper start end result
     | start == end = result
     | validPersistance result < validPersistance (start + 1) = helper (start + 1) end (start + 1)
     | validPersistance result == validPersistance (start + 1) && sumDigits result > sumDigits (start + 1) = helper (start + 1) end (start + 1)
     | otherwise = helper (start + 1) end result

