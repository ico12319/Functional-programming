

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]
    print $ getVolumes [(3, 7), (1,12), (4,7)]  == [197.92, 37.7, 351.86] -- my test



type Cylinder = (Double, Double)


roundTwoDig :: Double -> Double
roundTwoDig n = (fromIntegral $ round $ n * 100) / 100


getVolumes :: [Cylinder] -> [Double]
getVolumes = map (\(r,h) -> roundTwoDig(pi * r ^ 2 * h))