import Data.List
main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False
    print $ leavesAreEqual t5 t6 == False --myTest





leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual bt1 bt2 = sort (helper bt1) == sort (helper bt2)
 where
    helper :: BTree -> [Int]
    helper Nil = []
    helper (Node value Nil Nil) = [value]
    helper (Node _ left right) = helper left ++ helper right



t1 :: BTree
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 :: BTree
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

t5 :: BTree
t5 = Node 16 (Node 1 (Node 2 Nil Nil) (Node 8 Nil Nil)) (Node 3 Nil (Node 4 Nil Nil))

t6 :: BTree
t6 = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))



data BTree = Nil | Node Int BTree BTree 
 deriving (Show, Eq)