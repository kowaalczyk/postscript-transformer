module Main where
    import System.Environment
    import System.IO
    import Text.ParserCombinators.ReadP
    import Lib

    data Input = Value { n::Int } | 
            Add | Div | Sub | Mul | 
            MoveTo | LineTo | ClosePath | 
            Translate | Rotate

    -- isWhitespace :: Char -> Bool


    main = do
        args <- getArgs
        putStrLn $ head args
        -- n <- read $ head args
        -- putStrLn (show n ::String)


-- -- module Main where

-- -- import Lib

-- -- main :: IO ()
-- -- main = someFunc
