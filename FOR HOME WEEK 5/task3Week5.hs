main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False  
    print $ isArithmetic [1,5,12,4,6] == False --myTest

isArithmetic :: [Int] -> Bool
isArithmetic [] = False
isArithmetic [_] = True
isArithmetic [x,y] = True
isArithmetic (x:y:z:xs) = (y-x) == (z-y) && isArithmetic (y:z:xs)