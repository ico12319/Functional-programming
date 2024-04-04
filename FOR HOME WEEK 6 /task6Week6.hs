import Data.Char
import Data.List

main :: IO()
main = do

    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
    print $ (getOddCompositionValue [(\x -> x + 14) , (\x -> x - 5) , (\x -> x*8)]) 4 == 46 --myTest


getOddCompositionValue :: [Integer -> Integer] -> (Integer -> Integer)
getOddCompositionValue = foldr1 (.) . foldr (\(ind, f) acc -> if odd ind then f : acc else acc) [] . zip [1..]


