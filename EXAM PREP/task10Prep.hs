--A snail crawls up a column. 
--During the day it crawls up some distance. 
--During the night it sleeps,
 --so it slides down for some distance (less than it crawls up during the day).
--Calculate the number of days the snail will need to reach the top of the column.
main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1


snail :: Int -> Int -> Int -> Int
snail height crawl slides = helper 0
 where
    helper :: Int -> Int
    helper res
     | res + crawl >= height = 1
     | otherwise = 1 + helper ((res + crawl) - slides)