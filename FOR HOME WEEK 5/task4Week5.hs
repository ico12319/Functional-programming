main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ specialSum 100 777 == 23995 --myTest


containsDigit :: Int -> Int -> Bool
containsDigit x y = elem (head (show y)) (show x)

specialSum :: Int -> Int -> Int
specialSum x y = sum (filter (\n -> mod n 4 == 1 && containsDigit n 6) [min x y..max x y])