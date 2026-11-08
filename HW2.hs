values :: [Int]
values = [6,2,12,10,7,3,0,12,4,8,7,8,10,13,10,8,2,11,17,9,16,12,14,9,9,2,7,3,7,10,1,11,11,13,10,12,15,9,15,13,9,8,16,1,18,9,10,13,15,5,13,15,11,10,8,1,9,13,10,9,7,14,6,6,9,14,12,17,7,7,4,12,11,4,7,8,10,6,14,6,13,14,4,1,10,1,8,11]



countNumSort:: [Int] -> Int -> Int
countNumSort (x:xs) i 
    | x == i && xs/=[] = 1 + countNumSort xs i 
    | x /= i && xs /= [] = countNumSort xs i
    | x == i && xs ==[]  = 1
    | otherwise = 0

-- goThroughNumbers values
goThroughNumbers:: [Int] -> [Int]
goThroughNumbers l = [countNumSort l x | x <- [0..(maximum l)]]

getLines::[Int]->[Int]
getLines l = map(\x -> if x>0 then 1 else 0) l 

removeLine::[Int]->[Int]
removeLine l1 = map(\x -> x - 1) l1

isThereOne :: [Int] -> Bool
isThereOne l 
    | sum l == 0 = False
    | otherwise = True

returnLines :: [Int] -> [[Int]]
returnLines l
    | isThereOne (getLines l) = (getLines l) : returnLines (removeLine l)
    |otherwise = [] 


legendColumn :: Int -> String
legendColumn n
    | n `mod` 5 == 0 = if n < 10 then " " ++ show n ++ " " else show n ++ " "
    | otherwise      = "   "

convert01 :: [[Int]] -> [String]
convert01 l = map(\x -> concatMap(\x -> if x == 1 then "| " else "  ")x)l

onlyLinesOfHistogram :: [[Int]] -> String
onlyLinesOfHistogram l = unlines (zipWith (++) labels lines)
  where
    lines = convert01 (reverse l)
    labels = map legendColumn [length l, length l - 1 .. 1]

grid =[[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0],[0,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0],[0,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0],[0,1,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0],[0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0],[0,0,0,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0],[0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]]


-- legendLine l = [x | x<- [0..length l], mod x 5 == 0 ]
legendLine :: [Int] -> String
legendLine l = concatMap (\x -> if mod x 5 == 0 then format x else "  ") [0 .. length l]
  where
    format x
      | x < 10    = show x ++ " "  
      | otherwise = show x

histogram :: [Int] -> String
histogram l = onlyLinesOfHistogram (returnLines (goThroughNumbers l)) ++ (legendLine(goThroughNumbers l))
