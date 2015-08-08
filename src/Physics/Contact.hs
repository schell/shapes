{-# LANGUAGE DataKinds #-}

module Physics.Contact where

import Control.Lens
import Data.Either
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import qualified Physics.Geometry as G
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.Linear
import Physics.Transform
import Utils.Utils

data Contact a = Contact { contactA :: PhysicalObj a
                         , contactB :: PhysicalObj a
                         , contactPoint :: P2 a
                         , contactNormal :: V2 a
                         , contactDepth :: a
                         , contactIndex :: (Int, Int) } deriving Show
data ContactBehavior a = ContactBehavior { contactBaumgarte :: a
                                         , contactPenetrationSlop :: a } deriving Show

defaultContactBehavior :: (Num a) => ContactBehavior a
defaultContactBehavior = ContactBehavior { contactBaumgarte = 0
                                         , contactPenetrationSlop = 0 }

generateContacts :: (Epsilon a, Floating a, Ord a) => ConstrainedPair a -> [Flipping (Contact a)]
generateContacts cp = case mc of Nothing -> []
                                 Just c -> flipInjectF $ flipMap f c cp
  where shapes = pairMap physicsShape cp
        mc = uncurry G.contact shapes
        f (cc, feat) (a', b') = fmap g ps
          where ps = G.flattenContactPoints cc
                n = G.contactNormal cc
                g p = Contact { contactA = a'
                              , contactB = b'
                              , contactPoint = p ^. G.clens'
                              , contactNormal = n
                              , contactDepth = G.contactDepth feat (p ^. G.clens)
                              , contactIndex = (G.featIndex feat, G.featIndex p)}

generator :: (Epsilon a, Floating a, Ord a) => ConstraintGen a
generator = getGenerator defaultContactBehavior

getGenerator :: (Epsilon a, Floating a, Ord a) => ContactBehavior a -> ConstraintGen a
getGenerator beh dt cp = fmap f (generateContacts cp)
               where f c = (flipExtractPair contactIndex c, const $ toConstraint beh dt c)

toConstraint :: (Fractional a, Ord a) => ContactBehavior a -> a -> Flipping (Contact a) -> Constraint a
toConstraint beh dt c = flipExtractWith (id, f) (fmap (toConstraint_ beh dt) c)
  where f (Constraint j b) = Constraint (flip33 j) b

toConstraint_ :: (Fractional a, Ord a) => ContactBehavior a -> a -> Contact a -> Constraint a
toConstraint_ beh dt c = Constraint (jacobian c) (baumgarte beh dt c)

jacobian :: (Num a) => Contact a -> V6 a
jacobian (Contact a b p n _ _) = ja `join33` jb
  where ja = (-n) `append2` ((xa - p') `cross22` n)
        jb = n `append2` ((p' - xb) `cross22` n)
        xa = _physObjPos a
        xb = _physObjPos b
        p' = view _Point p

-- add extra energy if the penetration exceeds the allowed slop
-- (i.e. subtract from C' = Jv + b in constraint C' <= 0)
baumgarte :: (Fractional a, Ord a) => ContactBehavior a -> a -> Contact a -> a
baumgarte beh dt c = if (d > slop) then (b / dt) * (slop - d) else 0
  where b = contactBaumgarte beh
        slop = contactPenetrationSlop beh
        d = contactDepth c
