module Examples.IO
  ( testSandbox,
    testSong,
    testInstruments,
    testOscillators,
    testEnvelopes,
  )
where

import Euterpea hiding (envASR)
import Euterpea.Extras.Biquad
import Euterpea.Extras.Effect
import Euterpea.Extras.Env
import Euterpea.Extras.Osc
import Euterpea.Extras.Utils
import Examples.ChickCorea
import Examples.Instruments

data SourceOsc = SSine | SSaw | SLPNoise | SBPNoise | SHPNoise
  deriving (Eq, Show)

sourceOsc :: SourceOsc -> AudSF Double Double
sourceOsc src = case src of
  SSine -> sine
  SSaw -> saw
  SLPNoise ->
    proc freq -> do
      s <- noiseWhite 42 -< ()
      filterLowPassBW -< (s, freq)
  SBPNoise -> proc freq -> do
    s <- noiseWhite 42 -< ()
    filterBandPassBW -< (s, freq, 1000)
  SHPNoise -> proc freq -> do
    s <- noiseWhite 42 -< ()
    filterHighPassBW -< (s, freq)

data ModOsc = MSine | MSaw | MNoise
  deriving (Eq, Show)

modOsc :: ModOsc -> AudSF Double Double
modOsc src = case src of
  MSine -> sine
  MSaw -> saw
  MNoise -> constA () >>> noiseWhite 42

-- Envs: AR, ARexp, ARexp2

-- if we make dur dynamic, then t' can be over dur by many times. we need to do proper math, not just subtract

--

noiseSweep :: Double -> AudSF () Double
noiseSweep dur =
  proc _ -> do
    e <- pulse (dur / 8) (dur / 4) -< ()
    s <- noiseWhite 42 -< ()
    w <- filterNotchBQ -< (s, 10000 + 5000 * e, 2.0)
    outA -< w

filterCircle :: Double -> Double -> Double -> AudSF Double Double
filterCircle x y r =
  let w = max (x - r) 0
   in proc s -> do
        e <- pulse w (2 * r) -< ()
        freq <- delayLine w <<< phasor -< 1 / 2 / r
        let t = freq * 2 - 1
            f = sqrt (1 - t * t)
        w <- filterBandPassBQ -< (s, y + r * 35000 * f * e, 4.0)
        w' <- filterBandPassBQ -< (w, y - r * 35000 * f * e, 4.0)
        outA -< w' * e + s * (1 - e)

circleWave :: Double -> Double -> Double -> AudSF () Double
circleWave x y r =
  let w = max (x - r) 0
   in proc _ -> do
        e <- pulse w (2 * r) -< ()
        freq <- delayLine w <<< phasor -< 1 / 2 / r
        let t = freq * 2 - 1
            f = sqrt (1 - t * t)
        w <- saw -< y + r * 40000 * f * e
        w' <- saw -< y - r * 40000 * f * e
        outA -< (w + w') * e

circles :: AudSF () Double
circles = proc _ -> do
  c1 <- circleWave 0.5 10000 0.25 -< ()
  c2 <- circleWave 0.5 10000 0.10 -< ()
  c3 <- circleWave 0.65 18000 0.03 -< ()
  c4 <- circleWave 0.15 4000 0.02 -< ()
  outA -< c1 + c2 + c3 + c4

sound :: AudSF Double Double
sound = proc freq -> do
  amp <- envAR 0.1 0.4 -< ()
  y <- sine -< freq
  outA -< y * amp

testSandbox :: IO ()
testSandbox = do
  let s1 = constA 440 >>> vibrato 5 20 >>> sound >>> feedbackDelay 0.1 0.8 0.5
  let s2 = constA 440 >>> sound >>> tremolo 5 0.25 >>> feedbackDelay 0.1 0.8 0.5
  let s3 = (proc _ -> do v1 <- s1 -< (); v2 <- s2 -< (); outA -< v1 * v2) :: AudSF () Double
  -- outFile "s1.wav" 5.0 s1
  -- outFile "s2.wav" 5.0 s2
  -- outFile "s3.wav" 5.0 s3
  -- outFile "s3b.wav" 5.0 (ringMod s1 s2)
  -- outFile "s3b-sh2.wav" 5.0 (noiseWhite 42 &&& (envLine 20 5 1 >>> phasor) >>> sampleAndHold)
  -- outFile "sweep.wav" 1.0 (noiseSweep 1.0)
  outFile
    "biquad-notch.wav"
    1.0
    ( noiseWhite 42
        >>> filterCircle 0.5 10000 0.25
        >>> filterCircle 0.5 10000 0.10
        >>> filterCircle 0.65 18000 0.03
        >>> filterCircle 0.15 4000 0.02
        >>> filterCircle 1.0 5000 0.30
    )

-- outFile "circles.wav" 1.0 circles

testSong :: IO ()
testSong = recordSong

testInstruments :: IO ()
testInstruments = do
  outFile "bell1.wav" 6 (bell1 6 (absPitch (C, 5)) 100 [])
  outFile "bell2.wav" 6 (bell2 6 (absPitch (C, 5)) 100 [])
  outFile "bell3.wav" 6 (bell3 6 (absPitch (C, 5)) 100 [])
  outFile "wind.wav" 6 (wind 6 (absPitch (C, 7)) 100 [])

testOscillators :: IO ()
testOscillators =
  let duration = 1.0
      frequency = 440
      mkFilePath str = str ++ "-" ++ show frequency ++ ".wav"
   in do
        outFile (mkFilePath "sine") duration (sine_ frequency)
        outFile (mkFilePath "saw") duration (saw_ frequency)
        outFile (mkFilePath "square") duration (square_ frequency)
        outFile (mkFilePath "triangle") duration (triangle_ frequency)
        outFile (mkFilePath "squareWidth") duration (squareWidth_ frequency 0.25)
        outFile (mkFilePath "triangleWidth") duration (triangleWidth_ frequency 0.25)
        outFile (mkFilePath "phasor") duration (phasor_ frequency)

testEnvelopes :: IO ()
testEnvelopes =
  let duration = 2.0
      frequency = 440
      mkFilePath str = "sine" ++ "-" ++ str ++ ".wav"
   in do
        outFile (mkFilePath "AR") duration (ringMod (sine_ frequency) (envAR 0.05 0.45))
        outFile (mkFilePath "ARexp") duration (ringMod (sine_ frequency) (envARexp 0.05 0.45))
        outFile (mkFilePath "ARexp2") duration (ringMod (sine_ frequency) (envARexp2 0.05 0.45 0.05 0.9))
        outFile (mkFilePath "ASR") duration (ringMod (sine_ frequency) (envASR 0.05 0.05 0.4))
        outFile (mkFilePath "ADSR") duration (ringMod (sine_ frequency) (envADSR 0.05 0.15 0.5 0.3))