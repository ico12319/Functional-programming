main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ isGraceful t3 == False --myTest


data NTree a = Nil | Node a [NTree a] deriving (Show)


isGraceful :: Integral a => NTree a -> Bool
isGraceful Nil = True
isGraceful (Node _ []) = True
isGraceful (Node parent children) = all (\(Node child subChildren) -> even (abs (parent - child)) && isGraceful (Node child subChildren)) children


t1 :: NTree Int
t1 = Node 1[ Node 3 [], Node 5 [], Node 7 [], Node 9 []]


t2 :: NTree Int
t2 = Node 7[Node 9[ Node 5 [], Node 2 []]]


t3 :: NTree Int
t3 = Node 10[ Node 5 [], Node 3 [], Node 7 []]