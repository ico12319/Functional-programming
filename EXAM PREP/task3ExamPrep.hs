main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]] == [[3], [1, 2, 3, 4, 5]]




onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter (\xs -> sum xs == div (head xs + last xs) 2 * length xs)
