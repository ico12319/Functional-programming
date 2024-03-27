main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
    print $ (switchSum (\x -> x - 3) (\x -> x ^ 2) 1) 2 == -1 --myTest

switchSum :: (Num a) => (a -> a) -> (a -> a) -> Int -> a -> a
switchSum _ _ 0 _ = 0
switchSum h g c x = h x + switchSum g h (c-1) (h x)