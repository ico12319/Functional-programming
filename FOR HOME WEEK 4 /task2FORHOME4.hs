import Data.List

main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True


isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM x xs = (not $ null xs) && ((x == head xs) || isPresentRecNonPM x (tail xs))


isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM x xs = (x == head xs) || isPresentRecPM x (tail xs)


isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem