module Main where
    import System.Environment
    import System.IO
    import Data.List
    import Data.Maybe
    import Control.Monad.State
    import Lib

    data ParserState = ParserState {
        stack :: [R],
        currPoint :: Maybe Point,
        basePoint :: Maybe Point,
        currPicture :: Picture
    }
    initState = ParserState [] Nothing Nothing (Picture [])

    readR :: String -> R
    readR str = toRational n where
        n = read str :: Integer

    parseWord :: String -> ParserState -> ParserState
    parseWord word state
        | word == "add" = applyOp (+) state
        | word == "sub" = applyOp (-) state
        | word == "mul" = applyOp (*) state
        | word == "div" = applyOp (/) state
        | otherwise = push (readR word) state where
            applyOp :: (R->R->R) -> ParserState -> ParserState
            applyOp op (ParserState (a:b:rest) cp bp pic) = ParserState ((op b a):rest) cp bp pic
            push :: R -> ParserState -> ParserState
            push n (ParserState stack cp bp pic) = ParserState (n:stack) cp bp pic

    parseInput :: [String] -> State ParserState [R]
    parseInput [] = do
        (ParserState stack cp bp pic) <- get
        return stack
    parseInput (word:words) = do
        currentState <- get
        let nextState = parseWord word currentState
        put nextState
        parseInput words

    main :: IO()
    main = do
        args <- getArgs
        let n = read $ head args :: Integer
        input <- getContents
        putStrLn "300 400 translate"
        print $ evalState (parseInput . words $ input) initState
        putStrLn "stroke showpage"
