import Data.List
import Data.Char
main :: IO ()
main = do
    print $ groupEquals ["eat","tea","tan","ate","nat","bat"] -- == [["bat"],["nat","tan"],["ate","eat","tea"]]
    print $ groupEquals [""] -- == [[""]] 
    print $ groupEquals ["a"] -- == [["a"]]



groupEquals :: [String] -> [[String]]
groupEquals = 
    map (map fst) . groupBy (\(_,t) (_,u) -> t == u) . sortOn snd . map (\ word -> (word,sort word))