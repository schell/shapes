module BenchLinear where

import Criterion.Main
import Physics.Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import Linear.Matrix
import qualified Utils.Linear as UL

thePoint :: P2 Double
thePoint = P (V2 1 2)

theDiff :: Diff V2 Double
theDiff = V2 3 4

thePoint' :: UL.P2
thePoint' = UL.P2 1 2

theDiff' :: UL.V2
theDiff' = UL.V2 3 4

theMat33 :: M33 Double
theMat33 = V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)

theMat33' :: UL.M33
theMat33' = UL.testM33

benchy :: [Benchmark]
benchy = [ bench "afdot" $ whnf (uncurry afdot) (thePoint, theDiff)
         , bench "!*!" $ whnf (uncurry (!*!)) (theMat33, theMat33) ]

benchy' :: [Benchmark]
benchy' = [ bench "UL.afdot" $ whnf (uncurry UL.afdot) (thePoint', theDiff')
          , bench "UL.!*!" $ whnf (uncurry (UL.!*!)) (theMat33', theMat33')]

main :: IO ()
main = defaultMain (benchy ++ benchy')
