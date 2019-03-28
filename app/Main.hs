-- © Krzysztof Kowalczyk kk385830@students.mimuw.edu.pl
module Main where
    import System.Environment (getArgs)
    import System.IO (putStrLn, getContents)
    import Control.Monad.State (StateT, get, put, evalStateT)
    import Control.Monad.Except (Except, throwError, runExcept)
    import Text.Read (readMaybe)
    import Data.List
    import Data.Maybe
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

    applyOp :: (R->R->R) -> ParserState -> Except String ParserState
    applyOp op state = case stack state of
        (a:b:rest) -> return state { stack=((op b a):rest) }
        otherwise -> throwError "StackTooSmall"

    applyDiv :: ParserState -> Except String ParserState
    applyDiv state = case stack state of
        (0:b:rest) -> throwError "DivideByZero"
        (a:b:rest) -> return state { stack=((b / a):rest) }
        otherwise -> throwError "StackTooSmall"

    applyPush :: String -> ParserState -> Except String ParserState
    applyPush word state = case (readMaybe word :: Maybe Integer) of
        (Just n) -> return state { stack=((toRational n):prevStack) } where
            prevStack = stack state
        Nothing -> throwError "ParsingError"

    applyMoveTo :: ParserState -> Except String ParserState
    applyMoveTo state = case stack state of
        (a:b:rest) -> return state { stack=rest, cpt=newBase, clen=(Just 0), bpt=newBase } where
            newBase = Just $ trpoint (ctr state) (point (b,a))
        otherwise -> throwError "StackTooSmall"

    applyLineTo :: ParserState -> Except String ParserState
    applyLineTo state = case (clen state, stack state) of
        (Nothing,_) -> throwError "NoStartingPoint"
        (Just _,(a:b:rest)) -> return state { stack=rest, cpt=(Just newPoint), clen=(Just newLen), cpic=newPic } where
            newLen = oldLen + 1
            newPoint = trpoint (ctr state) (point (b,a))
            newPic = (cpic state) & line (xy oldPoint) (xy newPoint)
            Just oldLen = clen state
            (Just oldPoint) = cpt state  -- clen is not Nothing ==> cpt is not Nothing
        otherwise -> throwError "StackTooSmall"

    applyClosePath :: ParserState -> Except String ParserState
    applyClosePath state = case clen state <= Just 1 of
        True -> return state  -- unlike lineTo, this is not an error by design
        False -> return state { cpt=(Just newPoint), clen=(Just newLen), cpic=newPic } where
            newLen = oldLen + 1
            newPic = (cpic state) & line (xy oldPoint) (xy newPoint)
            (Just newPoint) = bpt state -- clen >= 2 ==> bpt is not Nothing
            (Just oldPoint) = cpt state -- clen >= 2 ==> cpt is not Nothing
            Just oldLen = clen state -- exists, and >= 2 by case definition

    applyRotate :: ParserState -> Except String ParserState
    applyRotate state = case stack state of
        (a:rest) -> return state { stack=rest, ctr=newTransform } where
            newTransform = rotate a >< ctr state
        otherwise -> throwError "StackTooSmall"

    applyTranslate :: ParserState -> Except String ParserState
    applyTranslate state = case stack state of
        (a:b:rest) -> return state { stack=rest, ctr=newTransform } where
            newTransform = translate (vec (b,a)) >< ctr state
        otherwise -> throwError "StackTooSmall"

    parseWord :: String -> ParserState -> Except String ParserState
    parseWord word state = case word of
        "add" -> applyOp (+) state
        "sub" -> applyOp (-) state
        "mul" -> applyOp (*) state
        "div" -> applyDiv state
        "moveto" -> applyMoveTo state
        "lineto" -> applyLineTo state
        "closepath" -> applyClosePath state
        "rotate" -> applyRotate state
        "translate" -> applyTranslate state
        otherwise -> applyPush word state

    parseInput :: [String] -> StateT ParserState (Except String) Picture
    parseInput [] = do
        state <- get
        return $ cpic state
    parseInput (word:words) = do
        state <- get
        let result = runExcept $ parseWord word state
        case result of
            (Right nextState) -> do
                put nextState
                parseInput words
            (Left e) -> throwError e

    postprocess :: Int -> Either String Picture -> String
    -- enable printing exceptions for debugging purposes here:
    -- postprocess _ (Left e) = e
    postprocess _ (Left _) = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"
    postprocess picScale (Right pic) = showRendering $ renderScaled picScale pic where
        showRendering :: IntRendering -> String
        showRendering rendering = unlines $ map showLine rendering
        showLine :: IntLine -> String
        showLine ((x1,y1),(x2,y2)) = foldl (++) "" [show x1," ",show y1," moveto ",show x2," ",show y2," lineto"]

    main :: IO()
    main = do
        args <- getArgs
        case (args, readMaybe $ head args :: Maybe Int) of
            (a:as, Just n) -> do
                input <- getContents
                putStrLn "300 400 translate"
                putStrLn $ postprocess n (runExcept (evalStateT (parseInput . words $ input) initState))
                putStrLn "stroke showpage"
            otherwise -> do
                putStrLn "Missing positional argument N::Int - scaling factor for the rendered picture"
