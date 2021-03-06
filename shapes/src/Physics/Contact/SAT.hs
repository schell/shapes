{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

{- |
Separating Axis Test (SAT).
A separating axis is a direction along which the projections of two shapes do not overlap.
Alternately, a separating axis is a line between two shapes that do not intersect.

If no separating axis is found, use the axis of smallest overlap to determine
which features of the objects are involved in the collision (e.g. calculate contact points and normals).
-}
module Physics.Contact.SAT where

import GHC.Types (Double(D#))

import Control.Lens ((^.), view, makeLenses, _1, makePrisms)
import Data.Either.Combinators
import Data.Function (on)
import Physics.Contact.ConvexHull
import Physics.Linear
import Utils.Descending
import Utils.Utils

data Overlap = Overlap { _overlapEdge :: !Neighborhood
                       , _overlapDepth :: !Double
                       , _overlapPenetrator :: !Neighborhood
                       } deriving Show
makeLenses ''Overlap

data SATResult = Separated Neighborhood | MinOverlap Overlap
                 deriving Show
makePrisms ''SATResult

type ContactPoints = Either Neighborhood (SP Neighborhood Neighborhood)

data Contact =
  Contact { _contactEdge :: !Neighborhood
          , _contactPenetrator :: !ContactPoints
          , _contactPenetratingEdge :: !(SP Neighborhood Neighborhood)
          } deriving Show
makeLenses ''Contact

satToEither :: SATResult -> Either Neighborhood Overlap
satToEither (Separated x) = Left x
satToEither (MinOverlap x) = Right x
{-# INLINE satToEither #-}

-- assumes pairs are (min, max)
overlapTest :: (Ord a) => SP a a -> SP a a -> Bool
overlapTest (SP a b) (SP c d) = not (c > b || d < a)
{-# INLINE overlapTest #-}

-- intervals are of distance along edge normal of shape X
overlapAmount :: (Ord a, Num a) => SP a a -> SP a a -> Maybe a
overlapAmount x@(SP _ edge) y@(SP penetrator _) = toMaybe (overlapTest x y) (edge - penetrator)
{-# INLINE overlapAmount #-}

overlapNormal :: Overlap -> V2
overlapNormal = _neighborhoodUnitNormal . _overlapEdge
{-# INLINE overlapNormal #-}

overlap :: ConvexHull -> Neighborhood -> ConvexHull -> Maybe Overlap
overlap sEdge edge sPen =
  fmap (\oval' -> Overlap edge oval' penetrator ) oval
  where dir = _neighborhoodUnitNormal edge
        extentS = extentAlongSelf sEdge (edge ^. neighborhoodIndex, dir)
        extentP = extentAlong sPen dir
        penetrator = extentP ^. extentMin
        oval = overlapAmount (extentS ^. extentProjection) (extentP ^. extentProjection)
{-# INLINE overlap #-}

minOverlap :: ConvexHull
           -> [Neighborhood]
           -> ConvexHull
           -> SATResult
minOverlap sEdge edges sPen =
  foldl1 f os -- lazy fold for early exit?
  where os = fmap (\edge -> maybe (Separated edge) MinOverlap $ overlap sEdge edge sPen) edges
        f :: SATResult -> SATResult -> SATResult
        f sep@(Separated _) _ = sep
        f _ sep@(Separated _) = sep
        f mino@(MinOverlap mino') o@(MinOverlap o') =
          if _overlapDepth o' < _overlapDepth mino' then o else mino
        {-# INLINE f #-}
{-# INLINE minOverlap #-}

minOverlap' :: ConvexHull -> ConvexHull -> SATResult
minOverlap' a = minOverlap a (neighborhoods a)
{-# INLINE minOverlap' #-}

penetratingEdge :: Overlap -> SP Neighborhood Neighborhood
penetratingEdge (Overlap edge _ b) =
  if bcn < abn then SP b c
  else SP a b
  where c = _neighborhoodNext b
        a = _neighborhoodPrev b
        cc = _neighborhoodCenter c
        bb = _neighborhoodCenter b
        aa = _neighborhoodCenter a
        abn = abs (D# ((bb `diffP2` aa) `dotV2` n))
        bcn = abs (D# ((cc `diffP2` bb) `dotV2` n))
        n = _neighborhoodUnitNormal edge
{-# INLINE penetratingEdge #-}

penetratedEdge :: Overlap -> SP Neighborhood Neighborhood
penetratedEdge (Overlap edgeStart _ _) = SP edgeStart (_neighborhoodNext edgeStart)
{-# INLINE penetratedEdge #-}

contactPoints' :: ContactPoints -> Either P2 (SP P2 P2)
contactPoints' = mapBoth f g
  where f = _neighborhoodCenter
        g = spMap f
{-# INLINE contactPoints' #-}

flattenContactPoints :: ContactPoints -> Descending Neighborhood
flattenContactPoints (Left p) = Descending [p]
flattenContactPoints (Right (SP p1 p2)) =
  if _neighborhoodIndex p1 > _neighborhoodIndex p2
  then Descending [p1, p2]
  else Descending [p2, p1]
{-# INLINE flattenContactPoints #-}

clipEdge :: SP Neighborhood Neighborhood -> V2 -> SP Neighborhood Neighborhood -> Maybe ContactPoints
clipEdge (SP aa bb) n inc_ = do
  inc' <- lApplyClip' l (clipSegment aBound (SP cd' inc)) inc_
  inc'' <- lApplyClip' l (clipSegment bBound (SP cd' (f inc'))) inc'
  applyClip'' (clipSegment abBound (SP cd' (f inc''))) inc''
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (negateV2 n)
        cd' = toLine2 c d
        inc@(SP c d) = f inc_
        (SP a b) = f (SP aa bb)
        f = spMap (view neighborhoodCenter)
        l = neighborhoodCenter
{-# INLINE clipEdge #-}

convertContactResult :: Flipping (Either Neighborhood (Maybe Contact))
                     -> Maybe (Flipping (Either Neighborhood Contact))
convertContactResult = flipInjectF . fmap liftRightMaybe
{-# INLINE convertContactResult #-}

-- 'Flipping' indicates the direction of the collision. 'Same' means 'a' is penetrated by 'b'.
-- TODO: return all useful values (e.g. overlap depth)
contactDebug :: ConvexHull
             -> ConvexHull
             -> (Maybe (Flipping (Either Neighborhood Contact)), SATResult, SATResult)
contactDebug a b = (convertContactResult $ fmap (mapRight contact_) ovl, ovlab, ovlba)
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovlab' = satToEither ovlab
        ovlba' = satToEither ovlba
        ovl :: Flipping (Either Neighborhood Overlap)
        ovl = eitherBranchBoth ((<) `on` _overlapDepth) ovlab' ovlba'
{-# INLINE contactDebug #-}

contact :: ConvexHull
        -> ConvexHull
        -> Maybe (Flipping (Either Neighborhood Contact))
contact a b = contactDebug a b ^. _1
{-# INLINE contact #-}

contact_ :: Overlap -> Maybe Contact
contact_ ovl@Overlap{..} = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = Contact _overlapEdge c pen
{-# INLINE contact_ #-}
