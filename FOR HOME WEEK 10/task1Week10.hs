import Data.Char

main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False
    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))
    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))
    print $ sumLeaves numberBTree == 119
    print $ average numberBTree == 16.22
    print $ average myTest == 3.0 --myTest


roundTwoDigits :: Double -> Double
roundTwoDigits = (/ 100) . fromIntegral . round . (100 *)

height :: BTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)


areEqual :: Eq a => BTree a -> BTree a -> Bool
areEqual Nil Nil = True
areEqual (Node x1 left1 right1) (Node x2 left2 right2) = x1 == x2 && areEqual left1 left2 && areEqual right1 right2
areEqual _ _ = False


setLevels :: BTree a -> BTree (Int, a)
setLevels tree = helper 0 tree
 where
    helper _ Nil = Nil
    helper level (Node x left right) = Node (level, x) (helper (level+1) left) (helper (level+1) right)


mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)


sumLeaves :: Num a => BTree a -> a
sumLeaves tree = helper tree
 where
    helper Nil = 0
    helper (Node x Nil Nil) = x
    helper (Node _ left right) = helper left + helper right



average :: BTree Int -> Double
average tree = roundTwoDigits (fromIntegral sum / fromIntegral count)
 where
    (sum, count) = sumAndCount tree
    
    sumAndCount Nil = (0, 0)
    sumAndCount (Node x left right) = (x + sumL + sumR, 1 + countL + countR)
     where
        (sumL, countL) = sumAndCount left
        (sumR, countR) = sumAndCount right


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))


myTest :: BTree Int
myTest = Node 3 (Node 2 Nil Nil) (Node 5 (Node 1 Nil Nil) (Node 4 Nil Nil))
