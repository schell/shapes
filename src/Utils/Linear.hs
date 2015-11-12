module Utils.Linear where

import Control.Applicative

data P2 = P2 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double deriving Show

data V2 = V2 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double deriving Show

data V3 = V3 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double deriving Show

-- row-major
data M22 = M22 {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double deriving Show

data M33 = M33 {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double deriving Show

instance Num V2 where
  (V2 a b) + (V2 c d) = V2 (a + c) (b + d)
  (V2 a b) - (V2 c d) = V2 (a - c) (b - d)
  (V2 a b) * (V2 c d) = V2 (a * c) (b * d)
  abs (V2 a b) = V2 (abs a) (abs b)
  negate (V2 a b) = V2 (negate a) (negate b)
  signum (V2 a b) = V2 (signum a) (signum b)
  fromInteger x = V2 (fromInteger x) 0

dot3 :: V3 -> V3 -> Double
dot3 (V3 a b c) (V3 d e f) = (a * d) + (b * e) + (c * f)

rows3 :: M33 -> [V3]
rows3 (M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) = [ V3 a0 b0 c0
                                         , V3 a1 b1 c1
                                         , V3 a2 b2 c2 ]

cols3 :: M33 -> [V3]
cols3 (M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) = [ V3 a0 a1 a2
                                         , V3 b0 b1 b2
                                         , V3 c0 c1 c2 ]

testM33 :: M33
testM33 = M33 1 2 3 4 5 6 7 8 9

(!*!) :: M33 -> M33 -> M33
--a !*! b = fromList33 $ dot3 <$> rows3 a <*> cols3 b
(M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) !*! (M33 d0 e0 f0 d1 e1 f1 d2 e2 f2) =
  M33 (a0 * d0 + b0 * d1 + c0 * d2)
      (a0 * e0 + b0 * e1 + c0 * e2)
      (a0 * f0 + b0 * f1 + c0 * f2)
      (a1 * d0 + b1 * d1 + c1 * d2)
      (a1 * e0 + b1 * e1 + c1 * e2)
      (a1 * f0 + b1 * f1 + c1 * f2)
      (a2 * d0 + b2 * d1 + c2 * d2)
      (a2 * e0 + b2 * e1 + c2 * e2)
      (a2 * f0 + b2 * f1 + c2 * f2)

(!**!) :: M22 -> M22 -> M22
(M22 a0 b0 a1 b1) !**! (M22 c0 d0 c1 d1) =
  M22 (a0 * c0 + b0 * c1)
      (a0 * d0 + b0 * d1)
      (a1 * c0 + b1 * c1)
      (a1 * d0 + b1 * d1)

afdot :: P2 -> V2 -> Double
afdot (P2 x y) (V2 x' y') = (x * x') + (y * y')

afscale33 :: V2 -> M33
afscale33 (V2 x y) = M33 x 0 0
                         0 y 0
                         0 0 1

rotate22_ :: Double -> Double -> M22
rotate22_ cosv sinv = M22 cosv (-sinv)
                          sinv cosv

rotate22 :: Double -> M22
rotate22 ori = rotate22_ c s
  where c = cos ori
        s = sin ori

aftranslate33 :: V2 -> M33
aftranslate33 (V2 x y) = M33 1 0 x
                             0 1 y
                             0 0 1

afrotate33_ :: Double -> Double -> M33
afrotate33_ cosv sinv = M33 cosv (-sinv) 0
                            sinv cosv    0
                            0    0       1

afrotate33 :: Double -> M33
afrotate33 ori = M33 c (-s) 0
                     s c    0
                     0 0    1
  where c = cos ori
        s = sin ori

identity33 :: M33
identity33 = M33 1 0 0
                 0 1 0
                 0 0 1

(!*) :: M33 -> V3 -> V3
(M33 a0 b0 c0 a1 b1 c1 a2 b2 c2) !* (V3 d0 d1 d2) =
  V3 (a0 * d0 + b0 * d1 + c0 * d2)
     (a1 * d0 + b1 * d1 + c1 * d2)
     (a2 * d0 + b2 * d1 + c2 * d2)

afmul' :: M33 -> V2 -> V2
afmul' (M33 a0 b0 _ a1 b1 _ _ _ _) (V2 d0 d1) =
  V2 (a0 * d0 + b0 * d1)
     (a1 * d0 + b1 * d1)

afmul :: M33 -> P2 -> P2
afmul (M33 a0 b0 c0 a1 b1 c1 _ _ _) (P2 d0 d1) =
  P2 (a0 * d0 + b0 * d1 + c0)
     (a1 * d0 + b1 * d1 + c1)
