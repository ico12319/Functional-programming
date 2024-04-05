main :: IO()
main = do
    print $ findSum 1 100 == 195 --myTest


containsDigit :: Int -> Int -> Bool
containsDigit num digit
 | num == 0 && digit == 0 = True
 | num == 0 && digit /= 0 = False 
 | mod num 10 == digit = True
 | otherwise = containsDigit (div num 10) digit

findSum :: Int -> Int -> Int
findSum start end = sum [x | x <- [min start end..max start end], mod x 4 == 1 && containsDigit x 6]