import Data.Char (toLower)

rotateRight :: Int -> [a] -> [a]
rotateRight x xs 
    | x >1 = rotateRight (x-1) (last xs : init xs)
    |otherwise = last xs : init xs

rotateLeft :: Int -> [a] -> [a]
rotateLeft i (x:xs)
    |i> 1 = rotateLeft (i-1) (xs ++ [x])
    |otherwise= (xs ++ [x])

myReplicate :: Int -> a -> [a]
-- myReplicate 0 _= []
-- myReplicate i x =  x : myReplicate (i-1) x

-- myReplicate i x = [ x |_ <- [1..i]]
myReplicate i x = foldr(\_ acc -> x: acc)[][1..i]

count :: Eq a => a -> [a] -> Int
-- count x xs = length [i | i <- xs, i == x]
-- count c s = foldr(\x acc -> if x == c then acc + 1 else acc) 0 s
count y [] = 0
count y (x:xs) 
    | x == y = 1 + count y xs
    | otherwise = count y xs

compress :: Eq a => [a] -> [a]
-- compress [x] = [x]
-- compress (x:y:xs)
--     | x == y = compress (y:xs)
--     | otherwise = x: compress(y:xs)

compress x1 = foldr(\x acc -> if x /= head acc then x : acc else acc)[(head x1)] x1


wordLengths :: [String] -> [Int]
-- wordLengths x = foldr(\x1 acc -> length x1:acc)[] x
wordLengths x = map length x

inRange :: Int -> Int -> [Int] -> [Int]
-- inRange x y a = [x1 | x1 <- a , x1<=y && x1>=x]
-- inRange x1 y a = foldr(\x acc -> if x<=y && x>= x1 then x: acc else acc )[]a
inRange x y a = filter(\x1 -> x1<=y && x1>=x)a


passingStudents :: [(String, Int)] -> [String]
-- passingStudents a = [x | (x,y)<- a, y>50]
passingStudents a = map(\(x,y)->x)(filter(\(x,y) -> y>50)a)

normalizeNames :: [String] -> [String]
normalizeNames a = map(\x-> map toLower x)[x | x<- a, x/=""]





products :: [(String, Int)]
products = [("avia",1000), ("trabant",500), ("avia",400), ("tatra",600),("trabant",200), ("avia",700)]
getValue :: [(String, Int)] -> String -> Int
-- getValue a name = sum[y | (x,y)<-a , x==name] 
getValue a name = sum (map(\(x,y)->y)(filter(\(x,y)->x==name)a) )



replace :: [(String, String)] -> String -> String

replacements = [("fox", "cat"), ("he", "she"), ("dog", "crocodile")]
sentence = "The quick brown fox jumps over the lazy dog"

replace r s =  unwords (map(\x-> checkWord r x)(words s))

checkWord [] a = a
checkWord ((x,y):xs) word 
    | word == x = y
    | otherwise = checkWord xs word

bmiCategory :: Double -> Double -> String
bmiCategory w h 
    |bmi w h >= 25 = "Nadvaha"
    |bmi w h >= 18.5 = "normal"
    |otherwise = "Podvaha"
    where 
        bmi w h =  w / (h^2)

productDigits :: Int -> Int
productDigits 0 = 1
productDigits x = (mod x 10) * productDigits(div x 10)


adultsOnly :: [(String, Int)] -> [String]
adultsOnly xs = [x | (x,y)<- xs, y>18]

upperCaseNames :: [String] -> [String]
upperCaseNames xs = map(\x->map toLower x)[x | x<-xs ,x /= ""] 

myProduct :: [Int] -> Int
myProduct xs = foldr(\x acc -> acc * x)1 xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = foldr(\x acc -> if p x || acc == True then True else False ) False xs  

myMaximum :: Ord a => [a] -> a
myMaximum xs = foldr(\x acc -> if acc < x then x else acc) (head xs) xs 


-- intersect a b = [x | x<- a , x<- b]
intersect a b = [x | x1<- a ,x2<- b, x<- a, x == x1 && x == x2]

-- isSorted :: Ord a => [a] -> Bool
notisSorted xs = foldr(\x acc -> if x>acc && acc/=minimum xs then minimum xs else acc )(last xs) xs
isSorted xs | notisSorted xs == maximum xs = True
    |otherwise = False