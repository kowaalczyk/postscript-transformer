-- © Krzysztof Kowalczyk kk385830@students.mimuw.edu.pl
module Lib where
    import Data.List
    import Mon

    type R = Rational
    type R2 = (R,R)

    data Vec = Vec { vxy::R2 } deriving (Show, Eq)
    data Point = Point { xy::R2 } deriving (Show, Eq)

    point :: R2 -> Point
    point xy = Point xy

    vec :: R2 -> Vec
    vec vxy =Vec vxy

    instance Mon Vec where
        m1 = vec (0,0)
        (><) (Vec (x1,y1)) (Vec (x2,y2)) = Vec (x1+x2,y1+y2)

    data Line = Line { start::Point, end::Point } deriving (Show, Eq)
    data Picture = Picture { lines::[Line] } deriving (Show, Eq)

    picture :: Picture
    picture = Picture []

    -- odcinek pomiędzy punktami o podanych współrzędnych
    line :: (R,R) -> (R,R) -> Picture
    line (x1,y1) (x2,y2) = Picture [Line (Point (x1,y1)) (Point (x2,y2))]

    -- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
    rectangle :: R -> R -> Picture
    rectangle a b = foldr (&) picture
        [ line (-a/2,-b/2) (-a/2,b/2)
        , line (-a/2,-b/2) (a/2,-b/2)
        , line (a/2,-b/2) (-a/2,b/2)
        , line (a/2,-b/2) (a/2,-b/2)]

    -- suma (nałożenie) dwóch rysunków
    (&) :: Picture -> Picture -> Picture
    (&) (Picture list1) (Picture list2) = Picture $ nub (list1 ++ list2)

    type IntLine = ((Int,Int), (Int,Int))
    type IntRendering = [IntLine]

    -- Obrazowanie przy danym współczynniku powiększenia
    -- z zaokrągleniem do najbliższych wartości całkowitych
    renderScaled :: Int -> Picture -> IntRendering
    renderScaled n (Picture lines) = map scaleLines lines where
        scaleLines :: Line -> IntLine
        scaleLines (Line p1 p2) = ((scale $ xy p1),(scale $ xy p2))
        scale :: (Rational,Rational) -> (Int,Int)
        scale (x,y) = (round $ (toRational n)*x, round $ (toRational n)*y)

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
        (><) a1 a2 = angle (deg a1 + deg a2)

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
        (><) (Transform t1) (Transform t2) = Transform $ compress [] (t1++t2) where
            compress :: [TransformStep] -> [TransformStep] -> [TransformStep]
            compress csteps ((Rotation r1):(Rotation r2):rest) = compress (csteps++[Rotation $ r1 >< r2]) rest
            compress csteps ((Translation v1):(Translation v2):rest) = compress (csteps++[Translation $ v1 >< v2]) rest
            compress csteps (next:rest) = compress (csteps++[next]) rest
            compress csteps [] = csteps

    rsin :: R -> R
    rsin r  -- Bhaskara I's sine approximation formula for rational numbers
        | 0 <= r && r <= 180 = 4*r*(180-r) / (40500 - r*(180-r))
        | r < 0 = - rsin (r+180)
        | otherwise = - rsin (r-180)

    rcos :: R -> R
    rcos r = rsin (r+90)

    rotateR2 :: Angle -> R2 -> R2
    rotateR2 (Angle deg) (x,y) = (rotateX x y deg, rotateY x y deg) where
        rotateX :: R -> R -> R -> R
        rotateX x y deg = x*(rcos deg) - y*(rsin deg)
        rotateY :: R -> R -> R -> R
        rotateY x y deg = y*(rcos deg) + x*(rsin deg)

    trpoint :: Transform -> Point -> Point
    trpoint (Transform steps) pt = foldl applyStep pt steps where
        applyStep :: Point -> TransformStep -> Point
        applyStep pt (Translation v) = applyVector v pt
        applyStep pt (Rotation ang) = point $ rotateR2 ang (xy pt)
        applyVector :: Vec -> Point -> Point
        applyVector (Vec (xv,yv)) (Point (x,y)) = point (x+xv,y+yv)

    trvec :: Transform -> Vec -> Vec
    trvec (Transform steps) v = foldl applyStep v steps where
        applyStep :: Vec -> TransformStep -> Vec
        applyStep v (Rotation ang) = vec $ rotateR2 ang (vxy v)
        applyStep v (Translation _) = v

    transform :: Transform -> Picture -> Picture
    transform tr (Picture lines) = Picture (map trline lines) where
        trline :: Line -> Line
        trline (Line start end) = Line (trpoint tr start) (trpoint tr end)
