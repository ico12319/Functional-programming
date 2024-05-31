import Data.Char
import Data.List

main :: IO ()
main = do

    print $ fn (>= 11) == [[1,2,0,0],[5,6,0,0],[9,10,0,0]]
    print $ fn (> 20) == [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
    print $ fn (== 5) == [[0,2,3,4],[0,6,7,8],[0,10,11,12]]

fn = resetMatrix [[1,2,3,4],[5,6,7,8],[9,10,11,12]]



makeZeroes :: [Int] -> [Int]
makeZeroes xs = map(\ x -> 0) xs


resetMatrix :: [[Int]] -> ((Int -> Bool) -> [[Int]])
resetMatrix matrix = (\f -> transpose $ map (\ ss -> if elem 0 ss then makeZeroes ss else ss ) $ map (\ms -> map (\el -> if f (el) == True then 0 else el) ms) $ transpose matrix)