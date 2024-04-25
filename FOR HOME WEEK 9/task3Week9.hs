

import Data.List

main::IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)
    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)
    print $ getClosestDistance [(ThreeD 2 8 5), (ThreeD 1 12 3), (ThreeD 5 22 54), (ThreeD 1 2 4), (ThreeD 0 2 7), (ThreeD 12 8 7)] == (3.1622776601683795,ThreeD 1.0 2.0 4.0,ThreeD 0.0 2.0 7.0) --myTest



data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq)

distance :: Floating a => Point a -> Point a -> a
distance (TwoD x1 y1) (TwoD x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
distance _ _ = error "Points must be of the same dimension"

getClosestDistance :: (Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance [] = error "Empty list"
getClosestDistance [_] = error "Min two points"
getClosestDistance (x:y:xs) = foldl findMinDistance (distance x y, x, y) pairs
 where
    pairs = [(distance x y, x, y) | (x, id1) <- zip (x:y:xs) [0..], (y, id2) <- zip (x:y:xs) [0..], id1 < id2]
    findMinDistance dy@(minDist, _, _) (dist, x, y)
     | dist < minDist = (dist, x, y)
     | otherwise = dy