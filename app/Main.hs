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
    } deriving (Show, Eq)
    initState = ParserState [] Nothing Nothing (Picture [])

    stackSize :: ParserState -> Int
    stackSize state = length . stack $ state

    readR :: String -> R
    readR str = toRational n where
        n = read str :: Integer

    applyOp :: (R->R->R) -> ParserState -> ParserState
    applyOp op state = case stack state of
        (a:b:rest) -> state { stack=((op b a):rest) }
        otherwise -> state -- TODO: Error handling

    push :: R -> ParserState -> ParserState
    push n state = state { stack=(n:prevStack) } where
        prevStack = stack state

    moveTo :: ParserState -> ParserState
    moveTo state = case stack state of
        (a:b:rest) -> state { stack=rest, currPoint=(Just (b,a)), basePoint=(Just (b,a)) }
        otherwise -> state -- TODO: Error handling

    lineTo :: ParserState -> ParserState
    lineTo state = case stack state of
        (a:b:rest) -> state { stack=rest, currPoint=(Just (b,a)), currPicture=(pic & line cp (b,a))} where
            (Just cp) = currPoint state -- TODO: Error handling
            pic = currPicture state
        otherwise -> state -- TODO: Error handling

    closePath :: ParserState -> ParserState
    closePath state = case currPoint state of
        (Just cp) -> state { currPoint=(Just bp), currPicture=(pic & line cp bp) } where
            (Just bp) = basePoint state  -- Just cp => Just bp
            pic = currPicture state
        Nothing -> state  -- unlike lineTo, this is not an error by design

    parseWord word state = case word of
        "add" -> applyOp (+) state
        "sub" -> applyOp (-) state
        "mul" -> applyOp (*) state
        "div" -> applyOp (/) state  -- TODO: Div error handling
        "moveto" -> moveTo state
        "lineto" -> lineTo state
        "closepath" -> closePath state
        otherwise -> push (readR word) state  -- TODO: read parse error

    parseInput :: [String] -> State ParserState Picture
    parseInput [] = do
        state <- get
        return (currPicture state)
    parseInput (word:words) = do
        state <- get
        let nextState = parseWord word state
        put nextState
        parseInput words

    showRendering :: IntRendering -> String
    showRendering rendering = unlines $ map showLine rendering where
        showLine :: IntLine -> String
        showLine ((x1,y1),(x2,y2)) = foldl (++) "" [show x1," ",show y1," moveto ",show x2," ",show y2," lineto"]

    main :: IO()
    main = do
        args <- getArgs
        let n = read $ head args :: Int
        input <- getContents
        putStrLn "300 400 translate"
        print $ showRendering $ renderScaled n $ evalState (parseInput . words $ input) initState
        putStrLn "stroke showpage"
