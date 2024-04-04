main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]] == [[3], [1, 2, 3, 4, 5]]
    print $ onlyArithmetic [[4,5,7,12], [1], [3,6,42]] == [[1]] --myTes



onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter (\xs -> sum xs == div (head xs + last xs) 2 * length xs)