main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1
    print $ (applyN (\x -> x + 3) 3) 2 == 11 --myTest


applyN :: (a -> a) -> Int -> (a -> a)
applyN f 0 = \x -> x
applyN f n = \x -> f $ applyN f (n-1) x

