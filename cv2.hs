import Data.Char (toLower)



rotateLEFT :: Int -> [a] -> [a]

rotateLEFT i (x:xs) 
    | i /= 0 = rotateRight (i-1) (xs++ [x])  
    | otherwise = (x:xs)


rotateRight  :: Int -> [a] -> [a]

rotateRight  i xs 
    | i /= 0 = rotateRight (i-1) (last xs : init xs )  
    | otherwise = xs


myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate x y = y : myReplicate (x-1) y 


count :: Eq a => a -> [a] -> Int
count _ []  = 0
count c s 
    |c == head s = 1+ (count c (tail s))
    | otherwise =  count c (tail s)



compress :: Eq a => [a] -> [a]
compress l
    | null l || null (tail l) = l
    | head l == head(tail l ) =  compress (tail l)
    | otherwise = head l : compress(tail l)
    
    
    
merge :: Ord a => [a] -> [a] -> [a]
merge a b 
    | null a = b
    | null b = a  
    |head a < head b = head a : merge (tail a) b
    |head a > head b = head b : merge a (tail b)
    | otherwise =  head b : merge a (tail b)


wordLengths :: [String] -> [Int]
wordLengths a = map length a


inRange :: Int -> Int -> [Int] -> [Int]
inRange a b l = filter (\x -> x>= a && x <= b) l


passingStudents :: [(String, Int)] -> [String]
passingStudents l = map (\(x,y)-> x) (filter (\(x, y) -> y > 50) l)

normalizeNames :: [String] -> [String]
normalizeNames s = map(\x->map(\x -> toLower x)x) (filter (\x-> x /= "") s)

products :: [(String, Int)]
products = [("avia",1000), ("trabant",500), ("avia",400), ("tatra",600),
            ("trabant",200), ("avia",700)]
getValue :: [(String, Int)] -> String -> Int
-- getValue l s = filter(\x->x==s) (map(\(x,y)->x)l)
getValue l s = sum (map (\(x,y)-> y) (filter(\(x,y)-> x == s)l))


replace :: [(String, String)] -> String -> String
replacements = [("fox", "cat"), ("he", "she"), ("dog", "crocodile")]
sentence = "The quick brown fox jumps over the lazy dog"
-- l1 l= map (\(x,y) -> x) l
-- l2 l= words l


check word ((x1,x2):xs)
    | word == x1 = x2 
    | otherwise = check word xs
check w [] = w

replace r s = unwords(map(\x -> check x r) (words s))




{-OD 3.3 nahrad do moodle -}