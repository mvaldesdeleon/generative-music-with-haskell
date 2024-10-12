module Euterpea.Extras.Utils (linear, constA, foldA) where

import Control.Arrow

-- Interpolator
linear :: (Fractional a) => (a, a) -> (a, a) -> a -> a
linear (imin, imax) (omin, omax) value =
  let t = (value - imin) / (imax - imin)
   in omin + t * (omax - omin)

-- Constant Arrow
constA :: (Arrow arr) => b -> arr a b
constA = arr . const

-- foldA
foldA :: (Foldable t, Arrow arr) => (b -> c -> c) -> c -> t (arr a b) -> arr a c
foldA f c = foldr (\sfb sfc -> sfb &&& sfc >>> arr (uncurry f)) (constA c)