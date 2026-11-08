import System.Enviroment

main :: IO()
main do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: /histohram <filename>"
        (filename:_) -> do
            text <- readFile filename
            let number = parseNumbers text
            putStrLn (histogram numbers) 

counts :: [Int] -> [Int]
counts nums = [length (filter (== d) nums) | d <- [0 .. (maximum nums - 1)]]

countsExample :: [Int]
countsExample = [1,5,3,2,4,1,5,8,7,9,10,6,6,7,5,4,2,2,1]

histogram :: [Int] -> String
histogram values = unlines (legendC `beside` (body `above` legendL))
    where
        histCounts = counts values
        body = histogramBody histCounts
        legendL = [legendLine width]
        legendC = legendColumn height
        width = length histCounts
        height = maximum histCounts

histogramBody :: [Int] -> [String]
histogramBody values = map (histogramLine values) levels
    where levels = reverse [1..maximum values]

histogramLine :: [Int] -> Int -> String
histogramLine values level = concatMap cell values
    where cell x | x >= level = " |"
                 | otherwise  = "  "

legendLine :: Int -> String
legendLine width = concatMap filledLabel [0..width-1]

legendColumn :: Int -> [String]
legendColumn height = map filledLabel (reverse [1..height]) ++ ["  "]

filledLabel :: Int -> String
filledLabel x = rfill 2 (legendLabel x)

legendLabel :: Int -> String
legendLabel x | x `mod` 5 == 0 = show x
              | otherwise      = ""

rfill :: Int -> String -> String
rfill width str = replicate (width - length str) ' ' ++ str

above a b = a ++ b
beside a b = zipWith (++) a b