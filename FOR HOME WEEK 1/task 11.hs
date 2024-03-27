main :: IO()
main = do

    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 6784 == 4876 --myTest




rev :: Int -> Int
rev num
 | num < 0 = error "The number should be a postive Integer!"
 | otherwise = reverseDigit num 0
  where
  reverseDigit :: Int -> Int -> Int
  reverseDigit 0 curr = curr
  reverseDigit num curr = reverseDigit (div num 10)  (curr*10 + mod num 10)
