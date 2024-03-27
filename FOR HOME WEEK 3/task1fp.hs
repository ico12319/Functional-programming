main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

 -- First solution by me not handling the cases with the zeros because of the rev function
rev :: Int -> Int
rev n
 | n < 10 = n
 | otherwise = helper n 0
  where
    helper :: Int -> Int -> Int
    helper num result
     | num < 10 = result * 10 + num
     | otherwise = helper (div num 10) (result * 10 + (mod num 10))

containsDigit :: Int -> Int -> Bool
containsDigit 0 0 = True
containsDigit 0 _ = False
containsDigit x y
 | mod x 10 == y = True
 | otherwise = containsDigit (div x 10) y 


 --removeFirstOccurrence :: Int -> Int -> Int
--removeFirstOccurrence x y
-- | --containsDigit x y == False = x
-- | --otherwise = helper x y 0 False (containsDigit x 0)
 -- where
   -- helper :: Int -> Int -> Int -> Bool -> Bool -> Int
    --helper number digit result isEncountered containsZero
--     | --number == 0 = rev result
--     | --number == 0 && containsZero = rev result * 10
--     | --mod number 10 == digit && (isEncountered == False) = helper (div number 10) digit result True containsZero
--     | --otherwise = helper (div number 10) digit (result* 10 + (mod number 10)) isEncountered containsZero


-- Second solution

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence 0 _ = 0
removeFirstOccurrence n d
 | mod n 10 == d = div n 10
 | otherwise = 10 * (removeFirstOccurrence (div n 10) d) + mod n 10
