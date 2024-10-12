{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.Extras.Effect
  ( vibrato,
    tremolo,
    feedbackDelay,
    ringMod,
    sampleAndHold_,
    sampleAndHold,
    portamento_,
    portamento,
  )
where

import Control.Arrow.Operations (ArrowCircuit (delay))
import Euterpea
import Euterpea.Extras.Osc

vibrato :: Double -> Double -> AudSF Double Double
vibrato vfreq vdepth = proc sig -> do
  delta <- sine -< vfreq
  outA -< sig + delta * vdepth

tremolo :: Double -> Double -> AudSF Double Double
tremolo vfreq vdepth = proc sig -> do
  delta <- sine -< vfreq
  outA -< sig * (1.0 + delta * vdepth)

feedbackDelay :: Double -> Double -> Double -> AudSF Double Double
feedbackDelay time feedback dryWet = proc sig -> do
  rec r <- delayLine time -< sig + r * feedback
  outA -< sig * (1.0 - dryWet) + r * dryWet

ringMod :: (Arrow a, Num c) => a b c -> a b c -> a b c
ringMod a1 a2 = a1 &&& a2 >>> arr (uncurry (*))

sampleAndHold_ :: forall p. (Clock p) => Double -> Signal p Double Double
sampleAndHold_ dur =
  let sr = rate (undefined :: p)
      sz = truncate (dur * sr)
   in proc sig -> do
        rec let s = i == sz - 1
            i <- delay 0 -< if s then 0 else i + 1
            y <- delay 0 -< if s then sig else y
        outA -< y

sampleAndHold :: AudSF (Double, Double) Double
sampleAndHold =
  proc (sig, ctrl) -> do
    rec let s = ctrl < c -- this will be true on a falling edge
        c <- delay 0 -< ctrl
        y <- delay 0 -< if s then sig else y
    outA -< y

portamento_ :: forall p. (Clock p) => Double -> Signal p Double Double
portamento_ r =
  let sr = rate (undefined :: p)
      delta = r / sr
   in proc sig -> do
        rec let next
                  | sig > s = min sig (s + delta)
                  | sig < s = max sig (s - delta)
                  | otherwise = sig
            s <- delay 0 -< next
        outA -< s

portamento :: forall p. (Clock p) => Signal p (Double, Double) Double
portamento =
  let sr = rate (undefined :: p)
   in proc (sig, r) -> do
        rec let delta = r / sr
                next
                  | sig > s = min sig (s + delta)
                  | sig < s = max sig (s - delta)
                  | otherwise = sig
            s <- delay 0 -< next
        outA -< s