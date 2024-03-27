main :: IO()
main = do

    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ sqAvg 16 4 == 136
    print $ sqAvg 14 2 == 100 --myTest
sqAvg :: Int -> Int -> Double
sqAvg x y = (fromIntegral $ x^2+y^2) / 2