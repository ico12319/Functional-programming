main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11
    print $ cone numberBTree == True
    print $ cone myTree == True --myTest
    print $ levelSum myTree 2 == 153 --myTest




cone :: BTree -> Bool
cone tree = helper tree 0 (-1)
 where
    helper :: BTree -> Int -> Int -> Bool
    helper tree level pSum 
     | currSum == 0 = True
     | pSum >= 0 && currSum <= pSum = False
     | otherwise = helper tree (level + 1) currSum
      where
        currSum = levelSum tree level



levelSum :: BTree -> Int -> Int
levelSum Nil _ = 0
levelSum (Node value left right) 0 = value
levelSum (Node _ left right) n = levelSum left (n - 1) + levelSum right (n - 1)


data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)


numberBTree :: BTree
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))


myTree :: BTree
myTree = Node 7 (Node 0 (Node 1 Nil Nil) (Node 78 Nil Nil)) (Node 23 Nil (Node 74 Nil Nil))


