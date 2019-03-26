module Main where
    import System.Environment
    import System.IO
    import Data.List
    import Data.Maybe
    import Control.Monad.State
    import Mon
    import Lib

    data ParserState = ParserState {
        stack :: [R],  -- Stack for loaded variables
        bpt :: Maybe Point,  -- Base point of current path
        cpt :: Maybe Point,  -- Current point of current path
        clen :: Maybe Int,  -- Length of current path (in points)
        cpic :: Picture,  -- Current picture (including current path)
        ctr :: Transform  -- Current transform (applied to all following points)
    } deriving (Show, Eq)
    initState = ParserState [] Nothing Nothing Nothing picture (m1::Transform)

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
        (a:b:rest) -> state { stack=rest, cpt=newBase, clen=(Just 0), bpt=newBase } where -- TODO: Error handling
            newBase = Just $ point (b,a)
        otherwise -> state

    applyLineTo :: ParserState -> ParserState
    applyLineTo state = case stack state of
        (a:b:rest) -> state { stack=rest, cpt=(Just newPoint), clen=(Just newLen), cpic=newPic } where
            newLen = oldLen + 1
            newPoint = trpoint (ctr state) (point (b,a))
            newPic = (cpic state) & line (xy oldPoint) (xy newPoint)
            Just oldLen = clen state -- TODO: Error handling
            (Just oldPoint) = cpt state -- TODO: Error handling
        otherwise -> state -- TODO: Error handling

    applyClosePath :: ParserState -> ParserState
    applyClosePath state = case clen state <= Just 1 of
        True -> state  -- unlike lineTo, this is not an error by design
        False -> state { cpt=(Just newPoint), clen=(Just newLen), cpic=newPic } where
            newLen = oldLen + 1
            newPic = (cpic state) & line (xy oldPoint) (xy newPoint)
            (Just newPoint) = bpt state -- clen >= 2 ==> bpt is not Nothing
            (Just oldPoint) = cpt state -- clen >= 2 ==> cpt is not Nothing
            Just oldLen = clen state -- exists, and >= 2 by case definition

    applyRotate :: ParserState -> ParserState
    applyRotate state = case stack state of
        (a:rest) -> state { stack=rest, ctr=newTransform } where
            newTransform = rotate a >< ctr state
        otherwise -> state -- TODO: Error handling

    applyTranslate :: ParserState -> ParserState
    applyTranslate state = case stack state of
        (a:b:rest) -> state { stack=rest, ctr=newTransform } where
            newTransform = translate (vec (b,a)) >< ctr state
        otherwise -> state -- TODO: Error handling

    parseWord word state = case word of
        "add" -> applyOp (+) state
        "sub" -> applyOp (-) state
        "mul" -> applyOp (*) state
        "div" -> applyOp (/) state  -- TODO: Div error handling
        "moveto" -> applyMoveTo state
        "lineto" -> applyLineTo state
        "closepath" -> applyClosePath state
        "rotate" -> applyRotate state
        "translate" -> applyTranslate state
        otherwise -> applyPush (readR word) state  -- TODO: read parse error

    parseInput :: [String] -> State ParserState Picture
    parseInput [] = do
        state <- get
        return $ cpic state
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
        putStrLn $ showRendering $ renderScaled n $ evalState (parseInput . words $ input) initState
        putStrLn "stroke showpage"
