--A number is a palindrome 
--if and only if it is the same number from right to left as well as from left to right. 
--Define a predicate that checks whether a non-negative number is a palindrome.


main :: IO()
main = do
    print $ isPalindrome 1 == True
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False 
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    print $ isPalindrome 121 == True
    print $ isPalindrome 12 == False
    print $ isPalindrome 120 == False
    print $ isPalindrome 12321 == True
    print $ isPalindrome 1221 == True


isPalindromeList :: Int -> Bool
isPalindromeList x = (reverse $ show x) == show x


rev :: Int -> Int
rev n = helper 0 n
 where
    helper :: Int -> Int -> Int 
    helper result number 
     | number == 0 = result
     | otherwise = helper ((result * 10) + mod number 10) (div number 10)

isPalindrome :: Int -> Bool 
isPalindrome x = rev x == x



