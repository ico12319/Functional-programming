main :: IO()
main = do   
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1 
    print $ snail 10 0 0 == 0 --myTest

snail :: Int -> Int -> Int -> Int
snail height day night = daysNeeded 0 0
  where
    daysNeeded :: Int -> Int -> Int
    daysNeeded climbed days
      | climbed >= height = days 
      | climbed + day >= height = days + 1
      | otherwise = daysNeeded (climbed + day - night)  (days + 1)