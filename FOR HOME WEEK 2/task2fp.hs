main :: IO()
main = do

    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521


containsDigit :: Int -> Int -> Bool
containsDigit x y = helper x y False
 where
    helper :: Int -> Int -> Bool -> Bool
    helper 0 _ _ = False
    helper num digit isFound
     | isFound == True = True
     | mod num 10 == digit = helper (div num 10) digit True
     | otherwise = helper (div num 10) digit isFound


countDigitsOccur :: Int -> Int -> Int
countDigitsOccur n m = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 count
     | m == 0 = count + 1
     | otherwise = count
    helper num count
     | mod num 10 == m = helper (div num 10) (count + 1)
     | otherwise = helper (div num 10) count

concatTimesDigit :: Int -> Int -> Int -> Int
concatTimesDigit x y z = helper x y z
 where
    helper :: Int -> Int -> Int -> Int
    helper num _ 0 = num
    helper num digit count = helper (num * 10 + digit) digit (count - 1)


sortN :: Int -> Int
sortN n = helper n 0 9 
 where
    helper :: Int -> Int -> Int -> Int
    helper num result currDigit
     | currDigit == 0 && containsDigit num 0 == True = result * 10
     | currDigit == 0 = result
     | otherwise = helper num (concatTimesDigit result currDigit (countDigitsOccur num currDigit)) (currDigit - 1)