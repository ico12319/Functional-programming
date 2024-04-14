main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]


type Node = (Int, Int, Int)

listLeaves :: [Node] -> [Int]
listLeaves bTree = filter (\x -> not (elem x parentValues)) childValues
 where
    parentValues = map (\(x, _, _) -> x) bTree
    childValues = concatMap (\(_, y, z) -> [y, z]) bTree