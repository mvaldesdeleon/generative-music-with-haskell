{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.Extras.Biquad
  ( filterLowPassBQ,
    filterHighPassBQ,
    filterBandPassBQ,
    filterNotchBQ,
  )
where

import Control.Arrow.Operations (ArrowCircuit (delay))
import Euterpea

data BiquadCoeffs = BiquadCoeffs !Double !Double !Double !Double !Double !Double

mkLPF :: Double -> Double -> Double -> BiquadCoeffs
mkLPF sr freq q =
  let w0 = 2 * pi * freq / sr
      sinw0 = sin w0
      cosw0 = cos w0
      a = sinw0 / 2 / q
   in BiquadCoeffs ((1 - cosw0) / 2) (1 - cosw0) ((1 - cosw0) / 2) (1 + a) (-2 * cosw0) (1 - a)

mkHPF :: Double -> Double -> Double -> BiquadCoeffs
mkHPF sr freq q =
  let w0 = 2 * pi * freq / sr
      sinw0 = sin w0
      cosw0 = cos w0
      a = sinw0 / 2 / q
   in BiquadCoeffs ((1 + cosw0) / 2) (-(1 + cosw0)) ((1 + cosw0) / 2) (1 + a) (-2 * cosw0) (1 - a)

mkBPF :: Double -> Double -> Double -> BiquadCoeffs
mkBPF sr freq q =
  let w0 = 2 * pi * freq / sr
      sinw0 = sin w0
      cosw0 = cos w0
      a = sinw0 / 2 / q
   in BiquadCoeffs (q * a) 0 (-q * a) (1 + a) (-2 * cosw0) (1 - a)

mkNotch :: Double -> Double -> Double -> BiquadCoeffs
mkNotch sr freq q =
  let w0 = 2 * pi * freq / sr
      sinw0 = sin w0
      cosw0 = cos w0
      a = sinw0 / 2 / q
   in BiquadCoeffs 1 (-2 * cosw0) 1 (1 + a) (-2 * cosw0) (1 - a)

filterLowPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterLowPassBQ =
  let sr = rate (undefined :: p)
   in proc (sig, freq, q) -> do
        biquad -< (sig, mkLPF sr freq q)

filterHighPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterHighPassBQ =
  let sr = rate (undefined :: p)
   in proc (sig, freq, q) -> do
        biquad -< (sig, mkHPF sr freq q)

filterBandPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterBandPassBQ =
  let sr = rate (undefined :: p)
   in proc (sig, freq, q) -> do
        biquad -< (sig, mkBPF sr freq q)

filterNotchBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterNotchBQ =
  let sr = rate (undefined :: p)
   in proc (sig, freq, q) -> do
        biquad -< (sig, mkNotch sr freq q)

biquad :: Signal p (Double, BiquadCoeffs) Double
biquad = proc (s, BiquadCoeffs a0 a1 a2 b0 b1 b2) -> do
  rec s' <- delay 0 -< s
      s'' <- delay 0 -< s'
      let y = (a0 / b0) * s + (a1 / b0) * s' + (a2 / b0) * s'' - (b1 / b0) * y' - (b2 / b0) * y''
      y' <- delay 0 -< y
      y'' <- delay 0 -< y'
  outA -< y
