main :: IO()
main = do

    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    print $ (sumExpr (12+) [0, 13, 3, 5]) 2 == 408 --myTest



sumExpr :: Num a => (a -> a) -> [a] -> (a -> a)
sumExpr f ys = (\x -> sum [y * f (x ^ n) | (y, n) <- zip ys [1..]])
