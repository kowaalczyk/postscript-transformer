module Main where
    import System.Environment
    import System.IO
    import Data.List
    import Data.Maybe
    import Control.Monad.State
    import Mon
    import Lib

    data ParserState = ParserState {
        stack :: [R],  -- Contains loaded variables
        currPoint :: Maybe R2,  -- Last point of current shape
        basePoint :: Maybe R2,  -- Starting point of current shape
        currPicture :: Picture,  -- Part of picture that is waiting for current tranfromation
        finPicture :: Picture,  -- Part of picture that is already transformed
        currTransform :: Transform,  -- Transformations that will be applied to current picture
        inTransform :: Bool  -- True only if last command was a transform
    } deriving (Show, Eq)
    initState = ParserState [] Nothing Nothing emptyPicture emptyPicture (Transform []) True

    stackSize :: ParserState -> Int
    stackSize state = length . stack $ state

    readR :: String -> R
    readR str = toRational n where
        n = read str :: Integer

    applyOp :: (R->R->R) -> ParserState -> ParserState
    applyOp op state = case stack state of
        (a:b:rest) -> state { stack=((op b a):rest) }
        otherwise -> state -- TODO: Error handling

    applyPush :: R -> ParserState -> ParserState
    applyPush n state = state { stack=(n:prevStack) } where
        prevStack = stack state

    applyMoveTo :: ParserState -> ParserState
    applyMoveTo state = case stack state of
        (a:b:rest) -> state { stack=rest, currPoint=(Just (b,a)), basePoint=(Just (b,a)) }
        otherwise -> state -- TODO: Error handling

    applyLineTo :: ParserState -> ParserState
    applyLineTo state = case stack state of
        (a:b:rest) -> state { stack=rest, currPoint=(Just (b,a)), currPicture=(pic & line cp (b,a))} where
            (Just cp) = currPoint state -- TODO: Error handling
            pic = currPicture state
        otherwise -> state -- TODO: Error handling

    applyClosePath :: ParserState -> ParserState
    applyClosePath state = case currPoint state of
        (Just cp) -> state { currPoint=(Just bp), currPicture=(pic & line cp bp) } where
            (Just bp) = basePoint state  -- Just cp => Just bp
            pic = currPicture state
        Nothing -> state  -- unlike lineTo, this is not an error by design

    applyRotate :: ParserState -> ParserState
    applyRotate state = case stack state of
        (a:rest) -> state { stack=rest, currTransform=(ct >< rotate a) } where
            ct = currTransform state
        otherwise -> state -- TODO: Error handling

    applyTranslate :: ParserState -> ParserState
    applyTranslate state = case stack state of
        (a:b:rest) -> state { stack=rest, currTransform=(ct >< (translate (vec (b,a)))) } where
            ct = currTransform state
        otherwise -> state -- TODO: Error handling

    toTransform :: ParserState -> ParserState
    toTransform state = case inTransform state of
        True -> state
        False -> state { finPicture=(fp & transform ct cp), currPicture=emptyPicture, inTransform=True } where
            ct = currTransform state
            cp = currPicture state
            fp = finPicture state

    fromTransform :: ParserState -> ParserState
    fromTransform state = state { inTransform=False }

    parseWord word state = case word of
        "add" -> applyOp (+) state
        "sub" -> applyOp (-) state
        "mul" -> applyOp (*) state
        "div" -> applyOp (/) state  -- TODO: Div error handling
        "moveto" -> applyMoveTo . fromTransform $ state
        "lineto" -> applyLineTo . fromTransform $ state
        "closepath" -> applyClosePath . fromTransform $ state
        "rotate" -> applyRotate . toTransform $ state
        "translate" -> applyTranslate . toTransform $ state
        otherwise -> applyPush (readR word) state  -- TODO: read parse error

    parseInput :: [String] -> State ParserState Picture
    parseInput [] = do
        state <- get
        return (finPicture . toTransform $ state)
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
