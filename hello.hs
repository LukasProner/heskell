import Text.Read (readMaybe)

-- main :: IO ()
-- main = putStrLn "Hello world!"

-- main :: IO ()
-- main = do
--     putStrLn "zadajte meno:"
--     name <- getLine
--     putStrLn "zkade si:"
--     from <- getLine
--     let greeting = "ahoj "++ name ++" ty si od "++from
--     putStrLn greeting

-- main = 
    -- putStrLn "zadajte meno:" >>
    -- getLine >>= \name -> 
    -- putStrLn "zadajte odkial" >>
    -- getLine >>= \city -> 
    -- let greeting = "ahoj "++ name ++ " z " ++ city
    -- in putStrLn greeting


getInt :: IO Int
getInt = do
    putStrLn "cislo"
    input <- getLine
    case readMaybe input of
        Just number -> return number
        Nothing -> do 
            putStrLn "Neplatný vstup. Skúste to znova."
            getInt


questions :: [String]
questions =
    [ "What is your name?"
    , "What is your favourite language?"
    , "What would you like to build?"
    ]
ask :: String -> IO String
ask questions = putStrLn question >> getLine
askAll :: [String] -> IO [String]
askAll [] = pure []
askAll(q:qs) = do
    a <- ask q
    as <- askAll qx 
    pure (a:as)

askAll' :: [String] -> IO [String]
askAll' = mapM ask

