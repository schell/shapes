module Physics.SolversST where

import Control.Lens ((&), (%~))
import Linear.Epsilon
import qualified Physics.Contact as C
import qualified Physics.ContactConstraints as CC
import qualified Physics.ContactSolver as ContS
import qualified Physics.SolutionProcessors as SP
import Physics.PairMap (Key)
import Physics.World
import Physics.WorldSolver
import Physics.WorldSolverST
import Utils.Utils

type PairCache n wObj = ContS.FeaturePairCaches n wObj
type WorldM' s n wObj = WorldM s wObj (PairCache n wObj) (ContS.WorldCache n wObj)

contactWSGen :: (C.Contactable n wObj, Epsilon n, Floating n, Ord n)
             => [Key]
             -> WorldLens Key (World wObj) wObj
             -> n
             -> WorldM' s n wObj ()
contactWSGen pairKeys l dt =
  prepareNextFrame ContS.worldCacheInitializer' ContS.pairCacheInitializer'
