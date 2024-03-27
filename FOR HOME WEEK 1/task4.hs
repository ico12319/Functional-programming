main :: IO()
main = do
    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000
    print $ sumCubesNoPow 20 3 == 8027
    print $ sumCubesNoPow 15 2 == 3383 --myTest

    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000  
    print $ sumCubesPow 40 10 == 65000
    print $ sumCubesPow 14 5 == 2869 --myTest

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow x y = x * x * x + y * y * y

sumCubesPow :: Int -> Int -> Int
sumCubesPow x y = x^3 + y^3