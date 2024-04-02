

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] -- == [785.4, 157.08, 125.66, 62.83]



type Cylinder = (Double, Double)

getVolumes :: [Cylinder] -> [Double]
getVolumes xs = map (\(r,h) -> 3.14 * r ^ 2 * h) xs 