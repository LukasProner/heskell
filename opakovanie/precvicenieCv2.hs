import Data.Char (toLower)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate x y = y : (myReplicate (x-1) y)


count :: Eq a => a -> [a] -> Int
count _ [] = 0
count c (x:xs) | x == c = 1 + (count c xs) 
    | otherwise = count c xs


compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs)
    | length xs == 0 = [x]
    | x == head xs = compress(xs)
    |otherwise = x:compress(xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x 
merge (x:xs) (y:ys) 
    | x<y = x:merge xs (y:ys)
    |otherwise = y:merge(x:xs) ys


wordLengths :: [String] -> [Int]
wordLengths l = map(\x-> length x) l

inRange :: Int -> Int -> [Int] -> [Int]
inRange x1 y l = filter(\x -> x>=x1 && x <= y )l

passingStudents :: [(String, Int)] -> [String]
-- passingStudents [("Eva",72), ("Adam",43), ("Mia",50), ("John",60)] == ["Eva","John"]
passingStudents l = map((\(x,y)-> x))(filter(\(x,y)-> y > 50)l)


normalizeNames :: [String] -> [String]
normalizeNames l = map(\x -> map(\x-> toLower x) x )(filter (\x-> x/="") l)


products :: [(String, Int)]
products = [("avia",1000), ("trabant",500), ("avia",400), ("tatra",600),
            ("trabant",200), ("avia",700)]
getValue :: [(String, Int)] -> String -> Int
getValue l s = sum (map(\(x,y) ->y)(filter(\(x,y) -> x==s) l) )