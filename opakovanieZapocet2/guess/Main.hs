module Main where
import Text.Read (readMaybe)
import System.Random

main :: IO ()
main = do
    number <- randNumber 1 10
    putStrLn ("Hello, " ++ show number ++ "!")
    play number
    -- where
    --     play = do 
            -- putStrLn "tip:"
            -- guess <- getLine
            -- case readMaybe guess of
            --     Just n
            --         | n == number -> putStrLn " super trafil si"
            --         | n>number -> do
            --             putStrLn " ola ola cislo je mensie"
            --             play
            --         | n< number -> do
            --             putStrLn "moje cislo je vacsie"
            --             play
            --     Nothing -> do
            --         putStrLn " cece zle cislo more sokeres"
            --         play

play :: Int -> IO()
play number = do
    putStrLn "tip:"
    guess <- getLine
    case readMaybe guess of
        Just n
            | n == number -> putStrLn " super trafil si"
            | n>number -> do
                putStrLn " ola ola cislo je mensie"
                play number
            | n< number -> do
                putStrLn "moje cislo je vacsie"
                play number
        Nothing -> do
            putStrLn " cece zle cislo more sokeres"
            play number

randNumber :: Int -> Int -> IO Int
randNumber a b = getStdGen >>= \gen -> return (fst (randomR (a, b) gen))

