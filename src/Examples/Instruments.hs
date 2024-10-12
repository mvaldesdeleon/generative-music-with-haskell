module Examples.Instruments (bell1, bell2, bell3, wind) where

import Euterpea
import Euterpea.Extras.Osc
import Euterpea.Extras.Utils

bell1 :: Instr (Mono AudRate)
bell1 dur ap vol params =
  let f = apToHz ap
      v = fromIntegral vol / 100
      d = fromRational dur
      sfs = map (\p -> constA (f * p) >>> sine) [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
   in proc _ -> do
        amp <- envExponSeg [0, 1, 0.001] [0.003, d - 0.003] -< ()
        s <- foldA (+) 0 sfs -< ()
        outA -< s * amp * v / 9

bell2 :: Instr (Mono AudRate)
bell2 dur ap vol params =
  let f = apToHz ap
      v = fromIntegral vol / 100
      d = fromRational dur
      sfs = map partialSF [4.07, 3.76, 3, 2.74, 2, 1.71, 1.19, 0.92, 0.56]
      partialSF p = proc _ -> do
        s <- sine -< f * p
        amp <- envExponSeg [0, 1, 0.001] [0.003, d - 0.003] -< ()
        outA -< s * amp
   in proc _ -> do
        s <- foldA (+) 0 sfs -< ()
        outA -< s * v / 9

bell3 :: Instr (AudSF () Double)
bell3 dur ap vol params =
  let f = apToHz ap
      v = fromIntegral vol / 100
      d = fromRational dur
   in proc () -> do
        x1 <- sine -< f
        x2 <- sine -< f * 4.1
        x3 <- sine -< f * 6.05
        x4 <- sine -< f * 8.2
        env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 1.0] -< ()
        env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 1.0] -< ()
        env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 1.0] -< ()
        env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 1.0] -< ()
        -- envx1 <- envLineSeg [0, 1, 1, 0] [0.0001 * d, 0.9999 * d, 0.0001 * d] -< ()
        envx2 <- envLineSeg [1, 0.5, 0.2, 0, 0] [0.05, 0.2, 3, 1.0] -< ()
        let envs = envx2
            partials = ((x1 * env1) + (x2 * env2) + (x3 * env3) + (x4 * env4)) / 4
        outA -< 0.95 * envs * partials * v

wind :: Instr (Mono AudRate)
wind dur ap vol params =
  let f = apToHz ap
      v = fromIntegral vol / 100
   in proc () -> do
        s <- noiseWhite 42 -< ()
        lfo1 <- sine -< 0.9
        lfo2 <- sine -< 1.3
        a2 <- filterBandPass 2 -< (s, f + 100 * (lfo1 + lfo2), 200)
        outA -< a2 * v / 5