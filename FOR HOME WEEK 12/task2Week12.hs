main :: IO()
main = do

    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
    print $ convert secondTree == Node 23 (Node 32 (Node 34 Nil Nil) (Node 31 Nil Nil)) (Node 7 Nil (Node 4 Nil Nil)) --myTest





convert :: BTree -> BTree
convert tree = fst $ helper tree 0
 where
    helper :: BTree -> Int -> (BTree,Int)
    helper Nil value = (Nil,value)
    helper (Node v l r) value = (Node newV newL newR, finalSum)
     where
        (newR, rSum) = helper r value
        newV = v + rSum
        (newL, finalSum) = helper l newV



tree :: BTree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))


secondTree :: BTree
secondTree = Node 16 (Node 1 (Node 2 Nil Nil) (Node 8 Nil Nil)) (Node 3 Nil (Node 4 Nil Nil))


data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)