add x y = x + y


hypotenuse x y = sqrt (x^2 + y^2)


isLeapYear :: Int -> Bool
isLeapYear x =  (mod x 4 == 0 && mod x 100 /= 0) || ( mod x 400 == 0) 


listAverage :: [Int] -> Int
listAverage x = div (sum x) (length x)


grade :: Int -> Char
grade x 
    | x >= 91 = 'A' 
    | x >= 81 = 'B'
    | x >= 71 = 'C'
    | x >= 61 = 'D' 
    | x >= 51 = 'E'
    | otherwise = 'F'

ticketPrice :: Int->Double

ticketPrice x 
    | (x < 15 || x >= 65) = 4 
    | otherwise = 8



sumDigits :: Int -> Int

sumDigits x  
    | x>0 = ( mod x 10)+ (sumDigits(div x 10)) 
    | otherwise = x


help x = div x 10
help2 x = mod x 10



isPalindrome :: Int -> Bool
isPalindrome n = n == reverseNumber n

reverseNumber :: Int -> Int
reverseNumber n 
    | n>0 = (( mod n 10)* (10^ (lengthOfDigit n-1))) + (reverseNumber(div n 10))
    |otherwise = 0

lengthOfDigit x
    | x < 10 = 1
    | otherwise = 1 + lengthOfDigit(div x 10) 



newtonSqrt x y
    | checkAccuracy x (newtonForm x y) == True = y
    | otherwise = newtonSqrt x (newtonForm x y)

newtonForm x y = (y + (x/y))/2

checkAccuracy x y = abs (y^2 - x) <=0.01 



{-
jeho kod 


newtonSqrt x = sqrtIter (x/2)
    where
        sqrtIter guess
            |good Enaugh guess = guess 
-}