module Physics.BenchGeometry where

import Criterion.Main
import Linear.V2
import Physics.ConvexHull
import Physics.Geometry
import Physics.Transform
import Debug.Trace

makeBox :: Double -> Double -> Double -> Double -> ConvexHull Double
makeBox x y w h = trace "makeBox" $ shape
  where vertices = rectangleVertices w h
        shape = listToHull $ transform (translateTransform (V2 x y)) vertices

benchy :: Benchmark
benchy = bench "contact" $ whnf (uncurry contact) (makeBox 0 0 4 4, makeBox 1 3 2 2)

main :: IO ()
main = defaultMain [benchy]
