module Lib where

    import Data.List
    import Mon
    
    type R = Rational
    type R2 = (R,R)
    
    data Vec = Vec { vecX::R, vecY::R } deriving (Show, Eq)
    data Point = Point { x::R, y::R } deriving (Show, Eq)
    
    point :: R2 -> Point
    point (x,y) = Point x y
    
    vec :: R2 -> Vec
    vec (x,y) = Vec x y
    
    instance Mon Vec where
        m1 = Vec 0 0
        (><) (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)
    
    data Line = Line { start::Point, stroke::Vec } deriving (Show, Eq)
    
    data Picture = Picture { lines::[Line] } deriving (Show, Eq)
    -- odcinek pomiędzy punktami o podanych współrzędnych
    line :: (R,R) -> (R,R) -> Picture
    line (x1,y1) (x2,y2) = Picture [Line (Point x1 y1) (Vec (x2-x1) (y2-y1))]
    
    -- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
    rectangle :: R -> R -> Picture
    rectangle a b = foldr (&) (Picture [])
        [ line (-a/2,-b/2) (-a/2,b/2)
        , line (-a/2,-b/2) (a/2,-b/2)
        , line (a/2,-b/2) (-a/2,b/2)
        , line (a/2,-b/2) (a/2,-b/2)]
    
    -- suma (nałożenie) dwóch rysunków
    (&) :: Picture -> Picture -> Picture
    (&) (Picture list1) (Picture list2) = Picture $ nub list1 ++ list2
    
    type IntLine = ((Int,Int), (Int,Int))
    type IntRendering = [IntLine]
    
    -- Obrazowanie przy danym współczynniku powiększenia
    -- z zaokrągleniem do najbliższych wartości całkowitych
    renderScaled :: Int -> Picture -> IntRendering
    renderScaled n (Picture lines) = map scaleLines lines where
        scaleLines :: Line -> IntLine
        scaleLines (Line (Point x y) (Vec xv yv)) = (scaleCoords x y,scaleCoords (x+xv) (y+yv))
        scaleCoords :: Rational -> Rational -> (Int,Int)
        scaleCoords x y = (round $ (toRational n)*x, round $ (toRational n)*y)
    
    fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
    fullCircle = 360
    
    data Angle = Angle { deg::R } deriving (Eq,Show,Ord)
    angle :: R -> Angle
    angle deg
        | deg >= fullCircle = angle (deg-fullCircle)
        | deg <= -fullCircle = angle (deg+fullCircle)
        | otherwise = Angle deg
    
    instance Mon Angle where
        m1 = Angle 0
        (><) (Angle deg1) (Angle deg2) = angle (deg1+deg2)
    
    data TransformStep = Translation { v::Vec } | Rotation { a::Angle } deriving (Eq,Show)
    data Transform = Transform { steps::[TransformStep] } deriving (Eq,Show)
    
    -- przesunięcie o wektor
    translate :: Vec -> Transform
    translate v = Transform [Translation v]
    
    -- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
    -- jednostki mozna sobie wybrać
    rotate :: R -> Transform
    rotate r = Transform [Rotation (angle r)]
    
    instance Mon Transform where
        m1 = Transform []
        (><) (Transform t1) (Transform t2) = Transform (compress [] (t1++t2)) where
            compress :: [TransformStep] -> [TransformStep] -> [TransformStep]
            compress csteps ((Rotation a1):(Rotation a2):rest) = compress (csteps ++ [Rotation (a1 >< a2)]) rest
            compress csteps ((Translation v1):(Translation v2):rest) = compress (csteps ++ [Translation (v1 >< v2)]) rest
            compress csteps (next:rest) = compress (csteps++[next]) rest
            compress csteps [] = csteps
    
    applyAngle :: Angle -> Point -> Point
    applyAngle (Angle deg) (Point x y) = Point newX newY where
        newX = x*(toRational $ cos radians) - y*(toRational $ sin radians)
        newY = y*(toRational $ cos radians) + x*(toRational $ sin radians)
        radians = 2*pi*(fromRational deg)/(fromRational fullCircle)
    
    applyAngleVec :: Angle -> Vec -> Vec  -- TODO: DRY
    applyAngleVec (Angle deg) (Vec x y) = Vec newX newY where
        newX = x*(toRational $ cos radians) - y*(toRational $ sin radians)
        newY = y*(toRational $ cos radians) + x*(toRational $ sin radians)
        radians = 2*pi*(fromRational deg)/(fromRational fullCircle)
    
    applyVector :: Vec -> Point -> Point
    applyVector (Vec xv yv) (Point x y) = Point (x+xv) (y+yv)
    
    trpoint :: Transform -> Point -> Point
    trpoint (Transform steps) point = fst $ foldl applyStep (point, Angle 0) steps where
        -- applyStep caches current, precise angle to limit floating point error
        applyStep :: (Point,Angle) -> TransformStep -> (Point,Angle)
        applyStep (point,r) (Translation vec) = (applyVector vec point,r)
        applyStep (point,angle1) (Rotation angle2) = (applyAngle (angle1 >< angle2) point, angle1 >< angle2)
    
    trvec :: Transform -> Vec -> Vec
    trvec (Transform steps) vec = fst $ foldl applyStep (vec, Angle 0) steps where
        -- applyStep caches current, precise angle to limit floating point error
        applyStep :: (Vec,Angle) -> TransformStep -> (Vec,Angle)
        applyStep (vec,angle1) (Rotation angle2) = (applyAngleVec (angle1 >< angle2) vec, angle1 >< angle2)
        applyStep (vec,r) (Translation _) = (vec,r)
    
    transform :: Transform -> Picture -> Picture
    transform tr (Picture lines) = Picture (map trline lines) where
        trline :: Line -> Line
        trline (Line start stroke) = Line (trpoint tr start) (trvec tr stroke)
