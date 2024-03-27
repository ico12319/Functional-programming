main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdG 12 144 == 12
    print $ myGcdG 18 288 == 18 --myTest

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
    print $ myGcdPM 15 225 == 15
    print $ myGcdPM 43 89 == 1 --myTest

myGcdG :: Int -> Int -> Int
myGcdG n d
    | n == 0 = d
    | d == 0 = n
    | otherwise = myGcdG d $ mod n d

myGcdPM :: Int -> Int -> Int
myGcdPM 0 d = d
myGcdPM n 0 = n
myGcdPM n d = myGcdPM d $ mod n d

