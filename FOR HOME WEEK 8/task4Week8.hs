main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0


data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read, Ord)


area :: Floating a => Shape a -> a
area (Circle r) = pi * (r * r)
area (Rectangle sideA sideB) = sideA * sideB
area (Triangle sideA sideB sideC) = sqrt (s * (s - sideA) * (s - sideB) * (s - sideC))
 where
    s = (sideA + sideB + sideC) / 2
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r


getAreas :: Floating a => [Shape a] -> [a]
getAreas = map area


maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a
maxArea = foldl1 (\acc shape -> if area shape > area acc then shape else acc)
