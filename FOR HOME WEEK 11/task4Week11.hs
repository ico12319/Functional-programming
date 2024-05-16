import Data.List

main :: IO ()
main = do

    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 21
    print $ closestAverage [(Temp 5 25.6), (Temp 7 18.9), (Temp 27 25.7), (Temp 11 18.9), (Temp 20 27.4)] == 5 --myTest






data Measuring = Temp Int Float

average :: [Measuring] -> Float
average xs = sum [temp | Temp _ temp <- xs] / fromIntegral (length xs)

closestAverage :: [Measuring] -> Int
closestAverage xs = fst $ foldr (\(Temp day temp) (cDay, cTemp) -> if abs (temp - (average xs)) < abs (cTemp - (average xs)) then (day, temp) else (cDay, cTemp)) (error "Empty list", 0) xs