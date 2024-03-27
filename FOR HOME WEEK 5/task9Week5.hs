main :: IO()
main = do
    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)
    print $ dotProduct (3, 0, 4) (1, -1, 0) == 3 --myTest

    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)
    print $ crossProduct (1, 0, 0) (0, 1, 0) == (0, 0, 1) --myTest

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053
    print $ magnitude (0, 0, 0) == 0.0 --myTest

type Vector a = (a, a, a)

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct (x,y,z) (d,s,g) = x * d + y * s + z * g


magnitude :: (Integral a) => Vector a -> Double
magnitude (x,y,z) = sqrt(fromIntegral (x ^ 2 + y ^ 2 + z ^ 2))

crossProduct :: (Num a) => Vector a -> Vector a -> Vector a
crossProduct (x,y,z) (d,s,g) = (y * g - z * s, z * d - x * g, x * s - y * d)