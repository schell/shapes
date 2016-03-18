{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.ContactSolver where

import Control.Applicative
import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe
import Physics.Constraint
import Physics.Contact
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import qualified Physics.ConstraintSolver as CS
import Utils.Utils

data ContactResult x = ContactResult { _contactNonPen :: !x
                                     , _contactFriction :: !x
                                     } deriving (Show, Eq)
makeLenses ''ContactResult

-- use to generate constraints (jacobians)
type ConstraintGen n a = ContactResult (Constraint' n a)

-- cache constraint impulses
--type SolutionCache n = ContactResult n

-- ConstraintGen' = dt -> (obj1, obj2) -> ([((featIndex1, featIndex2), ConstraintGen)], new ConstraintGen')
newtype ConstraintGen' n a =
  ConstraintGen' { _cgen' :: (n -> Key -> (a, a) -> ([(Key, ConstraintGen n a)], ConstraintGen' n a)) }
makeLenses ''ConstraintGen'

-- per-contact (feature pair) cache:
type FeaturePairCaches n a = PairMap (ContactResult n, ConstraintGen n a)

data WorldCache n a = WorldCache { _wcDt :: n
                                 , _wcCgen' :: ConstraintGen' n a
                                 }
makeLenses ''WorldCache

instance (Show n) => Show (WorldCache n a) where
  show WorldCache{..} =
    "WorldCache " ++ show _wcDt

wcApplyCgen' :: WorldCache n a
             -> Key
             -> (a, a)
             -> ([(Key, ConstraintGen n a)], WorldCache n a)
wcApplyCgen' worldCache@WorldCache{..} pairKey ab =
  (featurePairCgens, worldCache & wcCgen' .~ newCgen')
  where (featurePairCgens, newCgen') = (_cgen' _wcCgen') _wcDt pairKey ab

-- apply rules to total constraint impulse
-- (previously accumulated impulse) -> (current iteration's sln) -> (objects) -> (new accumulated impulse, incremental sln to apply)
type SolutionProcessor n a = ContactResult n -> ContactResult (ConstraintResult n) -> (a, a) -> (ContactResult n, ContactResult (ConstraintResult n))

instance Functor ContactResult where
  fmap f (ContactResult a b) = ContactResult (f a) (f b)

instance Applicative ContactResult where
  pure f = ContactResult f f
  (ContactResult f g) <*> (ContactResult x y) = ContactResult (f x) (g y)

emptySln :: (Num n) => ContactResult n
emptySln = ContactResult 0 0

-- copy the last frame's cached solution if it exists
-- cache the ConstraintGens for this frame
updatePairCache :: [(Key, ConstraintGen n a)]
                   -> ContactResult n
                   -> Maybe (FeaturePairCaches n a)
                   -> Maybe (FeaturePairCaches n a)
updatePairCache [] _ _ = Nothing -- no contacts this frame = clear everything
updatePairCache featurePairCgens emptySolution (Just featurePairCaches0) =
  Just $ foldl' f IM.empty featurePairCgens
  where f !featurePairCaches (!pairKey, !cgen) =
          insertPair pairKey (sln', cgen) featurePairCaches
          where sln' = fromMaybe emptySolution (fmap fst $ lookupPair pairKey featurePairCaches0)
updatePairCache featurePairCgens emptySolution Nothing =
  Just $ foldl' f IM.empty featurePairCgens
  where f !featurePairCaches (!pairKey, !cgen) =
          insertPair pairKey (emptySolution, cgen) featurePairCaches

pairCacheInitializer :: (Num n)
                     => CS.PairCacheInitializer a (FeaturePairCaches n a) (WorldCache n a)
pairCacheInitializer pairKey ab mFeaturePairCaches worldCache =
  (mFeaturePairCaches', worldCache')
  where mFeaturePairCaches' = updatePairCache featurePairCgens emptySln mFeaturePairCaches
        (featurePairCgens, worldCache') = wcApplyCgen' worldCache pairKey ab

worldCacheInitializer' :: n -> WorldCache n a -> WorldCache n a
worldCacheInitializer' dt worldCache = worldCache & wcDt .~ dt

worldCacheInitializer :: CS.WorldCacheInitializer a n (WorldCache n a)
worldCacheInitializer _ _ _ = worldCacheInitializer'

-- TODO: traverse/fold these IntMaps directly instead of folding over keys
--       to get rid of some of these fromJusts (also in ConstraintSolver)
pairUpdater :: (Contactable n a, Fractional n)
            => SolutionProcessor n a
            -> CS.PairUpdater a (FeaturePairCaches n a) (WorldCache n a)
pairUpdater slnProc pairKey ab0 featurePairCaches0 worldCache0 =
  (ab', featurePairCaches', worldCache0)
  where (ab', featurePairCaches') =
          solveMany slnProc (keys featurePairCaches0) featurePairCaches0 ab0

-- apply the cached solutions
cacheApplicator :: (Contactable n a, Fractional n)
                => CS.PairUpdater a (FeaturePairCaches n a) (WorldCache n a)
cacheApplicator pairKey ab0 featurePairCaches0 worldCache0 =
  (foldl' f ab0 (keys featurePairCaches0), featurePairCaches0, worldCache0)
  where f !ab !featurePairKey = applyContactConstraintResult constraintResult ab
          where (sln, cgen) = fromJust $ lookupPair featurePairKey featurePairCaches0
                constraint = fmap ($ ab0) cgen
                constraintResult = (,) <$> sln <*> constraint

solveMany :: (Contactable n a, Fractional n)
          => SolutionProcessor n a
          -> [Key]
          -> FeaturePairCaches n a
          -> (a, a)
          -> ((a, a), FeaturePairCaches n a)
solveMany slnProc featurePairKeys featurePairCaches0 ab0 =
  foldl f (ab0, featurePairCaches0) featurePairKeys
  where f (ab, featurePairCaches) featurePairKey =
          (ab', insertPair featurePairKey (sln', cgen) featurePairCaches)
          where (ab', sln') = solveOne slnProc featurePairKey cgen ab sln
                (sln, cgen) = fromJust $ lookupPair featurePairKey featurePairCaches

solveOne :: (Contactable n a, Fractional n)
         => SolutionProcessor n a
         -> Key
         -> ConstraintGen n a
         -> (a, a)
         -> ContactResult n
         -> ((a, a), ContactResult n)
solveOne sp k cg ab sln = (ab', sln')
  where ab' = applyContactConstraintResult cr' ab
        cr = fmap (`constraintResult` ab) cg
        (sln', cr') = sp sln cr ab

applyContactConstraintResult :: (Contactable n a, Fractional n)
                             => ContactResult (ConstraintResult n)
                             -> (a, a)
                             -> (a, a)
applyContactConstraintResult (ContactResult cr cr') = applyConstraintResult cr' . applyConstraintResult cr
