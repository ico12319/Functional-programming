main :: IO()
main = do

    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"
    print $ canCarry 23 64.34 6.12 == "No"
    print $ canCarry 1 100 99 == "Yes" --myTest

canCarry :: Int -> Double -> Double -> String
canCarry c k w
 | k < 0 = error "The weight that John can carry should be a positive ineteger!"
 | c < 0 = error "The number of products should be a positive integer!"
 | w < 0 = error "The number of kilograms should be a positive integer!"
 | fromIntegral c * w <= k = "Yes"
 | otherwise = "No"