import Text.Read (readMaybe)

main :: IO ()
main = otherWay
-- main = do
--     putStrLn "meno:"
--     name <- getLine
--     putStrLn "mesto:"
--     city <- getLine
--     let response = "hello " ++ name ++ " from " ++ city
--     putStrLn response

otherWay :: IO()
otherWay =
    putStrLn "meno:">>
    getLine >>= \name ->
        putStrLn "mesto: ">>
        getLine >>= \city ->
            let response = "hello " ++ name ++ " from " ++ city in putStrLn response


-- Úloha 3.1
-- Implementujte funkciu getInt :: IO Int, ktorá sa opýta používateľa na celé číslo a vráti ho. Ak používateľ zadá neplatný vstup, program sa opýta znova. Použite funkciu readMaybe z modulu Text.Read na konverziu reťazca na číslo.

getInt :: IO Int
getInt = do
    putStrLn "cislo : "
    cislo <- getLine
    case readMaybe cislo of
        Just cislo -> pure cislo
        Nothing ->  getInt


-- Úloha 4.1
-- Nech máme definovaný zoznam otázok:

questions :: [String]
questions =
    [ "What is your name?"
    , "What is your favourite language?"
    , "What would you like to build?"
    ]
-- Definujte funkcie ask a askAll. Prvá sa bude pýtať zadanú otázku a vráti odpoveď. Druhá sa postupne opýta všetky otázky zo zoznamu a vráti zoznam odpovedí.

ask :: String -> IO String

ask question = do
    putStrLn question
    answer <- getLine
    pure answer
-- askAll :: [String] -> IO [String]

askAll :: [String] -> IO [String]
askAll [] = pure []
askAll (q:qs) = do
    a <- ask q
    as <- askAll qs
    pure (a:as)

askAll2 :: [String] -> IO [String]
askAll2 qs = do
    mapM ask qs