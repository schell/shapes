{-# LANGUAGE MagicHash #-}

module Physics.Constraint.ConvertLinear where

import GHC.Types (Double(D#))

import qualified Linear.Affine as L
import qualified Linear.V2 as L
import qualified Physics.Linear as L

import Physics.Constraint.OptLinear

toLV2 :: V2 -> L.V2 Double
toLV2 = (\[x, y] -> L.V2 x y) . toListV2

toLP2 :: P2 -> L.P2 Double
toLP2 (P2 v) = L.P . toLV2 $ v

fromLV2 :: L.V2 Double -> V2
fromLV2 (L.V2 (D# x) (D# y)) = V2 x y
