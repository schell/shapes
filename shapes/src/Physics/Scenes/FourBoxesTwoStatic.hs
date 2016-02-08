module Physics.Scenes.FourBoxesTwoStatic where

import Linear.Epsilon
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.External
import Physics.Geometry
import Physics.Object
import Physics.World
import Physics.Scenes.Scene

boxA :: (Fractional a, Eq a) => PhysicalObj a
boxA = PhysicalObj { _physObjVel = V2 1 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) 0
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (2, 1) }

boxB :: (Fractional a, Eq a) => PhysicalObj a
boxB = PhysicalObj { _physObjVel = V2 (-4) 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 5 2
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (1, 0.5) }

boxC :: (Fractional a, Eq a) => PhysicalObj a
boxC = PhysicalObj { _physObjVel = V2 0 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 0 (-6)
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (0, 0) }

boxD :: (Fractional a, Eq a) => PhysicalObj a
boxD = PhysicalObj { _physObjVel = V2 0 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) (-4)
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (1, 0) }

boxA' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxA' = WorldObj boxA 0.2 $ rectangleHull 4 4

boxB' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxB' = WorldObj boxB 0.2 $ rectangleHull 2 2

boxC' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxC' = WorldObj boxC 0.2 $ rectangleHull 18 1

boxD' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxD' = WorldObj boxD 0.2 $ rectangleHull 0.4 3

world :: (Epsilon a, Floating a, Ord a) => World (WorldObj a)
world = fromList [boxA', boxB', boxC', boxD']

externals :: (Physical n a, Epsilon n, Floating n, Ord n) => [External n a]
externals = [constantAccel (V2 0 (-2))]

contactBehavior :: (Floating a) => ContactBehavior a
contactBehavior = ContactBehavior 0.01 0.02

scene :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene = Scene world externals contactBehavior
