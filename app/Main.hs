module Main where
    import System.Environment
    import System.IO
    import Data.List
    import Data.Maybe
    import Control.Monad.State
    import Lib

    data ParserState = ParserState {
        stack :: [R],
        currPoint :: Maybe R2,
        basePoint :: Maybe R2,
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
        | word == "moveto" = moveTo state
        | word == "lineto" = lineTo state
        | word == "closepath" = closePath state
        | otherwise = push (readR word) state where
            applyOp :: (R->R->R) -> ParserState -> ParserState
            applyOp op (ParserState (a:b:rest) cp bp pic) = ParserState ((op b a):rest) cp bp pic
            push :: R -> ParserState -> ParserState
            push n (ParserState stack cp bp pic) = ParserState (n:stack) cp bp pic
            moveTo :: ParserState -> ParserState
            moveTo (ParserState (a:b:rest) cp bp pic) = ParserState rest (Just (b,a)) (Just (b,a)) pic
            moveTo state = state  -- TODO: Error handling
            lineTo :: ParserState -> ParserState
            lineTo (ParserState (a:b:rest) (Just cp) bp pic) = ParserState rest (Just (b,a)) bp (pic & line cp (b,a))
            lineTo state = state  -- TODO: Error handling
            closePath :: ParserState -> ParserState
            closePath (ParserState stack (Just cp) (Just bp) pic) = ParserState stack (Just bp) (Just bp) (pic & line cp bp)
            closePath state = state  -- by reqirement, closePath defaults to identity instead of error
            -- TODO: read parse error

    parseInput :: [String] -> State ParserState Picture
    parseInput [] = do
        (ParserState stack cp bp pic) <- get
        return pic
    parseInput (word:words) = do
        currentState <- get
        let nextState = parseWord word currentState
        put nextState
        parseInput words

    main :: IO()
    main = do
        args <- getArgs
        let n = read $ head args :: Int
        input <- getContents
        putStrLn "300 400 translate"
        print $ renderScaled n $ evalState (parseInput . words $ input) initState
        putStrLn "stroke showpage"
