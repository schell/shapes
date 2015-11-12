{-# LANGUAGE DataKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, TypeSynonymInstances #-}

module Physics.Transform where

import Utils.Utils
import Utils.Linear

data WorldTransform = WorldTransform { worldTrans :: {-# UNPACK #-} !M33
                                     , worldUntrans :: {-# UNPACK #-} !M33
                                     } deriving Show

toTransform :: V2 -> Double -> WorldTransform
toTransform pos ori = joinTransforms (translateTransform pos) (rotateTransform ori)

scaleTransform :: V2 -> WorldTransform
scaleTransform s@(V2 x y) = WorldTransform (afscale33 s) (afscale33 s')
  where s' = V2 (1/x) (1/y)

rotateTransform :: Double -> WorldTransform
rotateTransform ori = WorldTransform rot rot'
  where rot = afrotate33_ c s
        rot' = afrotate33_ c (-s)
        c = cos ori
        s = sin ori

translateTransform :: V2 -> WorldTransform
translateTransform pos = WorldTransform transl transl'
  where transl = aftranslate33 pos
        transl' = aftranslate33 (-pos)

idTransform :: WorldTransform
idTransform = WorldTransform identity33 identity33

joinTransforms :: WorldTransform -> WorldTransform -> WorldTransform
joinTransforms (WorldTransform outer outer') (WorldTransform inner inner') =
  WorldTransform (outer !*! inner) (inner' !*! outer')

joinTransforms' :: [WorldTransform] -> WorldTransform
joinTransforms' = foldl1 joinTransforms

invertTransform :: WorldTransform -> WorldTransform
invertTransform (WorldTransform f g) = WorldTransform g f

data LocalT a = LocalT !WorldTransform !a deriving Show
type LV2 = LocalT V2
type LP2 = LocalT P2

data WorldT a = WorldT !a deriving Show
type WV2 = WorldT V2
type WP2 = WorldT P2

iExtract :: WorldT a -> a
iExtract (WorldT x) = x

iInject :: a -> WorldT a
iInject = WorldT

iInject_ :: a -> LocalT a
iInject_ = LocalT idTransform

instance Functor (LocalT) where
  fmap f (LocalT t v) = LocalT t (f v)

instance Functor WorldT where
  fmap f (WorldT v) = WorldT (f v)

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t where
  transform :: WorldTransform -> t -> t
  untransform :: WorldTransform -> t -> t

  wExtract :: LocalT t -> WorldT t
  wExtract (LocalT t v) = WorldT (transform t v)

  wExtract_ :: LocalT t -> t
  wExtract_ = iExtract . wExtract

  wInject :: WorldTransform -> WorldT t -> LocalT t
  wInject t v = LocalT t (untransform t (iExtract v))

  wInject_ :: WorldTransform -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance WorldTransformable P2 where
  transform (WorldTransform trans _) = afmul trans
  untransform (WorldTransform _ untrans) = afmul untrans

instance WorldTransformable V2 where
  transform (WorldTransform trans _) = afmul' trans
  untransform (WorldTransform _ untrans) = afmul' untrans

instance WorldTransformable (LocalT b) where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract

instance (WorldTransformable b) => WorldTransformable (b, b) where
  transform t = pairMap (transform t)
  untransform t = pairMap (untransform t)

instance (WorldTransformable b) => WorldTransformable [b] where
  transform t = map (transform t)
  untransform t = map (untransform t)

instance (WorldTransformable b) => WorldTransformable (Maybe b) where
  transform t = fmap (transform t)
  untransform t = fmap (untransform t)

wfmap :: (Functor t) => (a -> t b) -> WorldT a -> t (WorldT b)
wfmap f (WorldT v) = fmap WorldT (f v)

wflip :: (Functor t) => WorldT (t a) -> t (WorldT a)
wflip (WorldT v) = fmap WorldT v

wmap :: (a -> b) -> WorldT a -> WorldT b
wmap = fmap

wlift2 :: (a -> b -> c) -> WorldT a -> WorldT b -> WorldT c
wlift2 f x = wap (wmap f x)

wlift2_ :: (a -> b -> c) -> WorldT a -> WorldT b -> c
wlift2_ f x y = iExtract (wlift2 f x y)

wap :: WorldT (a -> b) -> WorldT a -> WorldT b
wap (WorldT f) = wmap f

wlap :: (WorldTransformable a) => WorldT (a -> b) -> LocalT a -> WorldT b
wlap f = wap f . wExtract

lwap :: (WorldTransformable a) => LocalT (a -> b) -> WorldT a -> LocalT b
lwap (LocalT t f) x = lmap f (wInject t x)

lap :: (WorldTransformable a) => LocalT (a -> b) -> LocalT a -> LocalT b
lap f x = lwap f (wExtract x)

lmap :: (a -> b) -> LocalT a -> LocalT b
lmap = fmap

lfmap :: (Functor t) => (a -> t b) -> LocalT a -> t (LocalT b)
lfmap f (LocalT t v) = fmap (LocalT t) (f v)

lunsafe_ :: (a -> b) -> LocalT a -> b
lunsafe_ f (LocalT _ v) = f v

wlens :: (Functor f) => (a -> f a) -> WorldT a -> f (WorldT a)
wlens f = fmap WorldT . f . iExtract
