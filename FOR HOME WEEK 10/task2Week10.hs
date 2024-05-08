main :: IO()
main = do

    print $ minDepthGreenNode colorTree == 2   
    print $ minDepthGreenNode myTestTree == 2 --myTest





data Color = Red | Green | Blue 
 deriving (Show, Eq)
data Tree = Nil | Node Color Tree Tree 
 deriving (Show, Eq)





minDepthGreenNode :: Tree -> Int
minDepthGreenNode tree = helper tree 0
 where
    helper Nil _ = 1000000 --don't know what value to return
    helper (Node color left right) depth
     | color == Green = depth
     | otherwise = min (helper left (depth + 1)) (helper right (depth + 1))





colorTree :: Tree
colorTree = Node Blue(Node Red(Node Green Nil Nil)Nil)(Node Red(Node Blue(Node Green Nil Nil)Nil)(Node Red Nil Nil))


myTestTree :: Tree
myTestTree = Node Blue(Node Red Nil(Node Green Nil Nil))(Node Red(Node Blue Nil(Node Green Nil Nil))Nil)