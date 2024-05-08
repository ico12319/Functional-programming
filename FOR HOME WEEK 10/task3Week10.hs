import Data.List


main :: IO()
main = do

    print $ ordered t1 == True
    print $ ordered t2 == False
    print $ ordered myTestTree == False --myTest


data Tree a = Nil | Node (a, a) (Tree a) (Tree a) 
 deriving (Show, Eq)



traverseDFS :: (Ord a) => Tree a -> [(a, a)]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right




ordered :: (Ord a) => Tree a -> Bool
ordered Nil = True
ordered tree = sort (traverseDFS tree) == reverse (traverseDFS tree)




t1 :: Tree Int
t1 = Node (3, 10) (Node (5, 8)(Node (6, 7) Nil Nil)(Node (4, 9) Nil Nil))(Node (2, 12)Nil(Node (1, 15) Nil Nil))

t2 :: Tree Int
t2 = Node (3, 10)(Node (5, 8)(Node (6, 7) Nil Nil)(Node (7, 9) Nil Nil))(Node (2, 12)Nil(Node (1, 15) Nil Nil))

myTestTree :: Tree Int
myTestTree = Node (3, 2)(Node (1, 12)(Node (7, 3) Nil Nil)(Node (1, 6) Nil Nil))(Node (6, 33)Nil(Node (1, 15) Nil Nil))