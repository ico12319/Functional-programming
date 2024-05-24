main ::IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True






isBoring :: (Eq a) => NTree a -> Bool
isBoring NullT = True
isBoring (Node value children) = all hasSame children && all isBoring children
 where
    hasSame NullT = True
    hasSame (Node val _) = val == value




data NTree a = NullT | Node a [(NTree a)]
 deriving (Show,Eq)
    

t1 :: NTree Int
t1 = Node 10 [Node 10 [Node 10 [NullT], Node 8 [Node 10 [NullT]], Node 2 [NullT]], Node 10 [Node 11 [NullT], Node 10 [NullT], Node 6 [NullT]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [NullT], Node 's' [NullT], Node 's' [NullT]]