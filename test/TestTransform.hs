-- Based on original tests supplied with the assignment

module Main(main) where
    import Lib
    import Test.QuickCheck hiding((><))
    import Control.Monad(liftM, liftM2)
    import Data.Ratio
    import Data.Functor
    import Mon
    
    testCompTrans :: R2 -> R2 -> R2 -> Property
    testCompTrans v1 v2 p = trpoint (translate (vec v1) >< translate (vec v2)) (point p)
                            === trpoint (translate (vec v1 >< vec v2)) (point p)
    
    testCompRot :: R -> R -> R2 -> Property
    testCompRot r1 r2 p = trpoint (rotate r1 >< rotate r2) (point p)
                            === trpoint (rotate (r1 + r2)) (point p)
    
    testAssRot :: R -> R -> R -> R2 -> Property
    testAssRot a b c p = trpoint (rotate a >< (rotate b >< rotate c)) (point p)
                            === trpoint ((rotate a >< rotate b) >< rotate c) (point p)
    
    testFullCircleRot :: R2 -> Property
    testFullCircleRot p = trpoint (rotate fullCircle) (point p) === (point p)
    
    testFullCircleRot2 :: R -> R2 -> Property
    testFullCircleRot2 r p = trpoint (rotate (r+fullCircle)) (point p) === trpoint (rotate r) (point p)
    
    testLeftUnit :: Transform -> Property
    testLeftUnit t = m1 >< t === t
    
    testRightUnit :: Transform -> Property
    testRightUnit t = t >< m1 === t
    
    testCompAssoc :: Transform -> Transform -> Transform -> Property
    testCompAssoc a b c = (a >< b) >< c === a >< (b >< c)
    
    testVecAssoc :: Vec -> Vec -> Vec -> Property
    testVecAssoc a b c = (a >< b) >< c === a >< (b >< c)
    
    writeln = putStrLn
    
    main = do
      writeln ""
      writeln "composition leftUnit"
      quickCheck testLeftUnit
      writeln "composition rightUnit"
      quickCheck testRightUnit
      writeln "composition associativity"
      quickCheckWith stdArgs {maxSuccess = 1000} testCompAssoc
      writeln "vector associativity"
      quickCheckWith stdArgs {maxSuccess = 1000} testVecAssoc
      writeln "------------------------"
      writeln "Compose translations"
      quickCheck testCompTrans
      writeln "Compose rotations"
      quickCheck testCompRot
      writeln "Rotation associativity"
      quickCheck testAssRot
      writeln "Full circle rot"
      quickCheck testFullCircleRot
      writeln "------------------------"
    
    -- Do not read past this point
    
    -- No, seriously...
    
    ------------------------------------------------------------
    -- Lasciate ogni speranza, voi ch’entrate
    ------------------------------------------------------------
    
    instance Arbitrary Transform where
      arbitrary = sized arb where
        arb 0 = return m1
        arb 1 = oneof [ transGen
                      , rotate <$> arbitrary
                      ]
        arb n = liftM2 (><) arb2 arb2 where
          arb2 = arb (n `div` 2)
        transGen :: Gen Transform
        transGen = translate . vec <$> arbitrary
    
    {-
      shrink = shrinkTrans
    
    shrinkTrans :: Transform -> [Transform]
    shrinkTrans (TrRotate r) = [TrRotate r' | r' <- shrinkRational r]
    shrinkTrans (Translate v) = [Translate v' | v' <- shrink v]
    shrinkTrans (TrCompose t u)= [t, u] ++ [TrCompose t' u' | (t', u') <- shrink (t, u)]
    shrinkTrans t = []
    -}
    
    instance Arbitrary Vec where
      arbitrary = fmap vec arbitrary
    {-
      shrink (V x y) = [V x' y' | x' <- shrinkRational x, y' <- shrinkRational y]
    -}
    
    shrinkRational :: Rational -> [Rational]
    shrinkRational r
      | (abs n < 10)  &&  d < 10 = []
      | otherwise = [n' % d'] where
      n' = div n 2
      d' = max (div d 2) 1
      n = numerator r
      d = denominator r