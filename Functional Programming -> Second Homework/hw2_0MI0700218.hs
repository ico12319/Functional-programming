import Data.List
import Data.Char


main :: IO()
main = do
    -- tests for task 1
    print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    print $ warmerAfter [21,22,23] == [1,1,0]
    print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

    -- tests for task 2
    print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
    print $ (setupRobots [-2, 0, 2] "RLL") 2  == [-2, 0, 0]
    print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17] 


 -- Task 1
warmerAfter :: [Double] -> [Int]
warmerAfter [] = []
warmerAfter temps = map (\ind -> helper (temps !! ind) (drop (ind + 1) temps)) [0..length temps - 1]
  where
    helper :: Double -> [Double] -> Int
    helper _ [] = 0
    helper currentTemperature (x:xs)
     | x > currentTemperature = 1
     | null xs = 0
     | otherwise = 1 + helper currentTemperature xs

 -- Task 2
setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots positions directions = moveRobots
  where
    moveRobots :: Int -> [Int]
    moveRobots secNum = sort $ calculateNewPositions secNum
     where
        calculateNewPositions :: Int -> [Int]
        calculateNewPositions numSec = map (\(pos, dir) -> pos + numSec * (if dir == 'R' then 1 else -1)) (zip positions directions)

   