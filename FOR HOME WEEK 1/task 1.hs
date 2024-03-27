main :: IO ()
main = do
  print $ countDigitsIter 12345 == 5
  print $ countDigitsIter 123 == 3
  print $ countDigitsIter 456789 == 6 --myTest

  print $ countDigitsRec 12345 == 5
  print $ countDigitsRec 123 == 3
  print $ countDigitsRec 4356 == 4 --myTest
  
countDigitsIter :: Int -> Int
countDigitsIter n
 | n < 0 = error "The number should be a postive Integer"
 | otherwise = helper n 0
 where
    helper :: Int -> Int -> Int
    helper leftover res
     | leftover < 10 = res + 1
     | otherwise = helper (div leftover 10) $ res + 1


countDigitsRec :: Int -> Int
countDigitsRec 0 = 0
countDigitsRec n
 | n < 0 = error "The number should be a postive Integer"
 | otherwise = 1 + countDigitsRec (div n 10) 