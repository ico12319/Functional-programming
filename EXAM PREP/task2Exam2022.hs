
import Data.List


main :: IO()
main = do
    print $ (pad [['S', ' ', 'U'],['F', 'M', 'I'],['F', ' ', 'P']]) '|'--  == ["|||||","|S U|","|FMI|","|F P|","|||||"]
    print $ (pad [[1, 2, 3], [4, 5, 6], [7, 8, 9]]) 0 -- == [[0,0,0,0,0],[0,1,2,3,0],[0,4,5,6,0],[0,7,8,9,0],[0,0,0,0,0]]
    print $ (pad [[1, 2, 3, 4], [4, 5, 6, 7], [7, 8, 9, 10]]) 99  -- == [[99,99,99,99,99,99],[99,1,2,3,4,99],[99,4,5,6,7,99],[99,7,8,9,10,99],[99,99,99,99,99,99]]



pad :: [[a]] -> (a -> [[a]])
pad xss = (\x -> [getFirstLast x] ++ map(\xs -> [x] ++ xs ++ [x]) xss ++ [getFirstLast x])
 where
    getFirstLast n = replicate (length (head xss) + 2) n
