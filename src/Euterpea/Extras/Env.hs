{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.Extras.Env
  ( envAR,
    denvAR,
    envARexp,
    denvARexp,
    envARexp2,
    envASR,
    denvASR,
    envADSR,
    denvADSR,
    pulse,
    pulse',
  )
where

import Control.Arrow.Operations (ArrowCircuit (delay))
import Euterpea hiding (envASR)
import Euterpea.Extras.Utils

envAR :: (Clock p) => Double -> Double -> Signal p () Double
envAR a r = envLineSeg [0, 1, 0, 0] [a, r, 1.0]

-- the d- variations take an additional input which is the expected duration
-- for the gate signal. that way we can play them with midi notes
denvAR :: (Clock p) => Double -> Double -> Double -> Signal p () Double
denvAR a r dur
  | dur < a = envLineSeg [0, dur / a, 0, 0] [dur, r, 1.0]
  | otherwise = envLineSeg [0, 1, 0, 0] [a, r, 1.0]

envARexp :: (Clock p) => Double -> Double -> Signal p () Double
envARexp a r = envExponSeg [0.001, 1, 0.001, 0.001] [a, r, 1.0]

denvARexp :: (Clock p) => Double -> Double -> Double -> Signal p () Double
denvARexp a r dur
  | dur < a = envExponSeg [0.001, 0.001 * pow (1 / 0.001) (dur / a), 0.001, 0.001] [dur, r, 1.0]
  | otherwise = envExponSeg [0.001, 1, 0.001, 0.001] [a, r, 1.0]
  where
    pow a b = exp (log a * b)

envASR :: (Clock p) => Double -> Double -> Double -> Signal p () Double
envASR a s r = envLineSeg [0, 1, 1, 0, 0] [a, s, r, 1.0]

denvASR :: (Clock p) => Double -> Double -> Double -> Double -> Signal p () Double
denvASR a s r dur
  | dur < a = envLineSeg [0, dur / a, 0, 0] [dur, r, 1.0]
  | dur < a + s = envLineSeg [0, 1, 1, 0, 0] [a, dur - a, r, 1.0]
  | otherwise = envLineSeg [0, 1, 1, 0, 0] [a, s, r, 1.0]

envADSR :: (Clock p) => Double -> Double -> Double -> Double -> Signal p () Double
envADSR a d s r = envLineSeg [0, 1, s, 0, 0] [a, d, r, 1.0]

denvADSR :: (Clock p) => Double -> Double -> Double -> Double -> Double -> Signal p () Double
denvADSR a d s r dur
  | dur < a = envLineSeg [0, dur / a, 0, 0] [dur, r, 1.0]
  | dur < a + d = envLineSeg [0, 1, 1 - (1 - s) * ((dur - a) / d), 0, 0] [a, dur - a, r, 1.0]
  | otherwise = envLineSeg [0, 1, s, s, 0, 0] [a, d, dur - a - d, r, 1.0]

envARexp2 :: (Clock p) => Double -> Double -> Double -> Double -> Signal p () Double
envARexp2 a r d v =
  let e1 = envARexp a r
      e2 = e1 >>> arr (* v) >>> delayLine d
   in foldA max 0 [e1, e2]

pulse :: forall p. (Clock p) => Double -> Double -> Signal p () Double
pulse w d =
  let sr = rate (undefined :: p)
      sw = truncate (w * sr)
      sd = truncate ((w + d) * sr)
   in proc _ -> do
        rec i <- delay 0 -< i + 1
        y <- delay 0 -< if (i < sw) || (i >= sd) then 0 else 1
        outA -< y

pulse' :: forall p. (Clock p) => Double -> Signal p () Double
pulse' w =
  let sr = rate (undefined :: p)
      sw = truncate (w * sr)
      sd = truncate (w * sr + 1)
   in proc _ -> do
        rec i <- delay 0 -< i + 1
        y <- delay 0 -< if (i < sw) || (i >= sd) then 0 else 1
        outA -< y
