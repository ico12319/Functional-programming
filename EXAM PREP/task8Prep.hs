--John has a backpack. 
--With it he can carry k kilograms. 
--An item from the supermarket weighs w kilograms. 
--Define a function that accepts three numbers - c (number of products), k and w 
--and returns whether 
--John is capable of buying all the products in one trip to the supermarket.


main :: IO()
main = do
    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"


canCarry :: Double -> Double -> Double -> String
canCarry numOfProducts kilogramsHeCanCarry weights
 | numOfProducts * weights <= kilogramsHeCanCarry = "Yes"
 | otherwise = "No"

