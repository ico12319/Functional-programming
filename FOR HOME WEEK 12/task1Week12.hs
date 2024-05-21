import Data.List

main :: IO()
main = do
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7 == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)
    print $ rangedSum thirdTree 1 2 == 3 -- myTest



rangedSum :: BinaryTree -> Int -> Int -> Int
rangedSum tree l r = helper tree (min l r) (max l r)
 where
    helper :: BinaryTree -> Int -> Int -> Int
    helper Nil _ _ = 0
    helper (Node value left right) l r
     | value < l = helper right l r
     | value > r = helper left l r
     | otherwise = value + helper left l r + helper right l r


firstTree :: BinaryTree
firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

secondTree :: BinaryTree
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

thirdTree :: BinaryTree
thirdTree = Node 16 (Node 1 (Node 2 Nil Nil) (Node 8 Nil Nil)) (Node 3 Nil (Node 4 Nil Nil))


data BinaryTree = Nil | Node Int BinaryTree BinaryTree
 deriving (Show,Eq)