import System.Environment (getArgs)

values :: [Int]
values = [6,2,12,10,7,3,0,12,4,8,7,8,10,13,10,8,2,11,17,9,16,12,14,9,9,2,7,3,7,10,1,11,11,13,10,12,15,9,15,13,9,8,16,1,18,9,10,13,15,5,13,15,11,10,8,1,9,13,10,9,7,14,6,6,9,14,12,17,7,7,4,12,11,4,7,8,10,6,14,6,13,14,4,1,10,1,8,11]

main :: IO()
main = do
    args <- getArgs
    case args of 
        (filename:_) -> do
            -- 2. Prečítame obsah súboru ako String
            content <- readFile filename
            
            -- 3. Rozdelíme text na slová/čísla a skonvertujeme ich na [Int]
            -- Funkcia words rozdelí text podľa medzier a noviných riadkov
            let numbers = map read (words content) :: [Int]
            
            -- 4. Vygenerujeme a vypíšeme histogram
            putStrLn (histogram numbers)
            
        _ -> putStrLn "Chyba: Zadajte názov súboru ako argument! (napr. runhaskell program.hs cisla.txt)"

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