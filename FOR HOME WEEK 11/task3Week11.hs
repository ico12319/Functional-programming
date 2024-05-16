main :: IO()
main = do

    print $ maxDepthBlueNode t1 == 2
    print $ maxDepthBlueNode t2 == 3 -- myTest


data Color = Red | Green | Blue 
 deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree 
 deriving (Show)


maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree = helper tree 0
 where
    helper :: Tree -> Int -> Int
    helper Empty _ = 0
    helper (Node color left right) currDepth
     | color == Blue = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
     | otherwise = max (helper left (currDepth + 1)) (helper right (currDepth + 1))



t1 :: Tree
t1 = Node Blue(Node Red (Node Green Empty Empty) Empty)(Node Red(Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)



t2 :: Tree
t2 = Node Blue(Node Red(Node Blue Empty(Node Green Empty Empty))(Node Green(Node Blue Empty Empty)Empty))(Node Green(Node Red Empty(Node Blue Empty (Node Green Empty Empty)))Empty)
