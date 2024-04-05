


main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[1,-4],[1]] -- == 2
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]]  -- == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] -- == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] -- == 45



sumUnique xss =  sum (concat (map (\xs -> filter (\x -> length (filter (==x) xs) == 1) xs) xss))