main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10
    print $ growingPlant 2 1 8 == 7 --myTest


growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = daysNeeded 0 0
    where
    daysNeeded::Int->Int->Int
    daysNeeded grow days
        | grow >= desiredHeight = days
        | grow + upSpeed >= desiredHeight = days+1
        | otherwise = daysNeeded (grow + upSpeed - downSpeed) (days+1)
    