-- main :: IO ()
-- main = putStrLn "Hello world!"


-- main :: IO ()
-- main = do
--     putStrLn "Zadajte vaše meno:"
--     name <- getLine
--     putStrLn "Zadajte mesto, kde žijete:"
--     city <- getLine
--     let greeting = "Hello, " ++ name ++ " from " ++ city ++ "!"
--     putStrLn greeting


main :: IO ()
main = 
    putStrLn "Zadajte vaše meno:" >>
    getLine >>= \name ->
    putStrLn "Zadajte mesto, kde žijete:" >>
    getLine >>= \city ->
    let greeting = "Hello, " ++ name ++ " from " ++ city ++ "!"
    in putStrLn greeting



import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

getInt :: IO Int
getInt = do
    putStr "Zadajte celé číslo: "
    hFlush stdout  -- Zabezpečí okamžité zobrazenie výzvy pred čítaním vstupu
    input <- getLine
    case readMaybe input of
        Just number -> return number
        Nothing     -> do
            putStrLn "Neplatný vstup. Skúste to znova."
            getInt

getInt :: IO Int
getInt = do
    putStrLn "Zadajte celé číslo:"
    input <- getLine
    case reads input of
        [(n, "")] -> return n
        _         -> do
            putStrLn "Neplatný vstup, skúste znova."
            getInt