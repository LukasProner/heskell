import Data.Char (toLower)

hypotenuse::Double -> Double -> Double
hypotenuse a b = sqrt ((a ^ 2) + (b ^ 2))

isLeapYear:: Int -> Bool
isLeapYear x 
    | (mod x 4 == 0 && mod x 100 /= 0 ) || mod x 400 == 0 = True
    | otherwise = False


listAverage :: [Int] -> Int
listAverage x = div (sum x)(length x ) 

listAverageBez :: [Int] -> Int
listAverageBez x = div (mySum x) (myLength x) 

mySum [x] = x
mySum (x:xs)= x + mySum xs 

myLength [x] = 1
myLength(x:xs)= 1 + myLength xs


grade :: Int -> String
grade x
    | x >81 = "A"
    | x > 60 = "B"
    | otherwise = "XS"


ticketPrice :: Int -> Double 
ticketPrice x
    | x < 15 || x > 65 = 4
    |otherwise = 8

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = (mod x 10) + sumDigits (div x 10)


isPalindrome :: Int -> Bool
isPalindrome n = n == reverseNumber n
    where
        reverseNumber 0 = 0 
        reverseNumber x = (mod x 10)* 10^((lengthOfIntiger x)-1) + reverseNumber (x `div` 10)

        lengthOfIntiger::Int->Int
        lengthOfIntiger 0 = 0
        lengthOfIntiger x = 1 + lengthOfIntiger(div x 10)

-- newtonSqrt:: Double -> Double ->Double
newtonSqrt y x 
    | abs ((y^2) - x) <= 0.0001 = y
    | otherwise = newtonSqrt (improve x y) x

-- improve:: Int->Int->Int
improve x y = (y + (x/y))/ 2



-- Druhe cvicenie
-- !!!!!!!
rotateRight :: Int -> [a] -> [a]
rotateRight 0 a = a
rotateRight x a = rotateRight(x-1)(last a : init a)

-- Urob rotate left
-- UROB MY REPLICATE, Count

compress :: Eq a => [a] -> [a]
compress [] = []
compress a  
    | tail a == [] = a
    | head a == head(tail a) = compress (tail a) 
    | otherwise = (head a) : compress (tail a)


merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys)
    | x< y = x : merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

wordLengths :: [String] -> [Int]
wordLengths a = map(\x -> length x) a
-- wordLengths a = map length a

inRange :: Int -> Int -> [Int] -> [Int]
inRange x y a = [l| l<- a , l<=y && l>=x]

inRangeFilter x y a = filter (\e -> e>=x && e<= y)a

-- passingStudents :: [(String, Int)] -> [String]
-- passingStudents x = map(\(x,y)-> x) (filter(\(x,y) -> y>50) x)

passingStudents :: [(String, Int)] -> [String]
passingStudents x = [name | (name, score) <- x, score > 50]


normalizeNames :: [String] -> [String]
normalizeNames x = [map toLower a | a<- x , a/=""]

products :: [(String, Int)]
products = [("avia",1000), ("trabant",500), ("avia",400), ("tatra",600),
            ("trabant",200), ("avia",700)]
getValue :: [(String, Int)] -> String -> Int

-- getValue a x = sum[b | (c,b)<-a , c == x] THIS WAS NICE

getValue a x1 = sum (map(\(x,y)->y)(filter(\(x,y) -> x == x1)a))

---------------

replace :: [(String, String)] -> String -> String
replace = undefined
replacements = [("fox", "cat"), ("he", "she"), ("dog", "crocodile")]
sentence = "The quick brown fox jumps over the lazy dog"

-- 3.6 este raz nedal si to babrak replace 

--tretie cvicenie
--1.1
listAnd :: [Bool] -> Bool
-- listAnd [] = True
-- listAnd (x:xs)
--     | x == False = False
--     |otherwise = listAnd xs

-- listAnd xs = foldr (&&) True xs
listAnd xs = foldl (&&) True xs


sumAll :: [Int]->Int
-- sumAll xs = foldr (\x acc -> acc + x) 0 xs
sumAll xs = foldl (\acc x -> acc + x) 0 xs

myReverse :: [a] -> [a]
myReverse xs = foldr (\x acc -> x:acc) [] xs
-- myReverse xs = foldl (\acc x -> x:acc) [] xs



myFilter:: (a->Bool)->[a]->[a]
myFilter p xs = foldr(\x acc -> if p x then x:acc else acc)[] xs 

myMap :: (a -> b) -> [a] -> [b]
myMap p xs = foldr (\x acc -> (p x):acc  ) [] xs 


myCount :: (a->Bool) -> [a] -> Int
-- myCount p xs = sum (filter (\x -> p x )xs) 

myCount p xs = foldr(\x acc -> if p x then acc + 1 else acc + 0) 0 xs

divisors :: Integer -> [Integer]
divisors i = [ x | x <- [1..i] , mod i x == 0] 


---Tu sa potom opytat na dalsie ulohy idem opakovat tie z druheho cvicenia 

