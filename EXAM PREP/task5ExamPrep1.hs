
import Data.List

main :: IO()
main = do
    print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5]  == False
    print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5]  == True
    print $ dominates (\x -> x + 1) (\x -> x * 4) [1, 0, 1, 4, 5] == False

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g = head . sort . map (\x -> abs (f x) >= abs (g x))