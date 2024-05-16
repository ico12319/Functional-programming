main :: IO()
main = do

    print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998
    print $ myPoly [1.0, 2.0 ..] 5 3 == 24.0 --myTest



myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> foldr1 (*) [(x - xy) | (xy, _) <- take y $ zip xs [1..]])