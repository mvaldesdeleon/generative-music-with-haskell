{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MuniHac.Challenges () where

import Euterpea

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

-- Oscillators

sine :: AudSF Double Double
sine = undefined

square :: AudSF Double Double
square = undefined

saw :: AudSF Double Double
saw = undefined

-- signal, pulse width
squarePWM :: AudSF (Double, Double) Double
squarePWM = undefined

triangle :: AudSF Double Double
triangle = undefined

-- signal, pulse width
trianglePWM :: AudSF (Double, Double) Double
trianglePWM = undefined

-- like saw, except it goes from 0.0 to 1.0
-- useful for control signals
phasor :: AudSF Double Double
phasor = undefined



-- Effects

-- signal, frequency, depth
vibrato :: AudSF (Double, Double, Double) Double
vibrato = undefined

-- signal, frequency, depth
tremolo :: AudSF (Double, Double, Double) Double
tremolo = undefined

-- signal, rate
portamento :: forall p. (Clock p) => Signal p (Double, Double) Double
portamento =
  let sr = rate (undefined :: p)
  in proc (sig, rate) -> do
    outA -< undefined

-- signal, delay, feedback
feedbackDelay :: AudSF (Double, Double, Double) Double
feedbackDelay = undefined

ringMod :: (Arrow a, Num c) => a b c -> a b c -> a b c
ringMod = undefined

distortion :: AudSF (Double, Double) Double
distortion = undefined

-- signal, control
sampleAndHold :: AudSF (Double, Double) Double
sampleAndHold = undefined


-- Envelope generators
envAR :: (Clock p) => Double -> Double -> Signal p () Double
envAR = undefined

envASR :: (Clock p) => Double -> Double -> Double -> Signal p () Double
envASR = undefined

envADSR :: (Clock p) => Double -> Double -> Double -> Double -> Signal p () Double
envADSR = undefined

envARexp2 :: (Clock p) => Double -> Double -> Double -> Double -> Signal p () Double
envARexp2 = undefined



-- Biquadratic filters
-- https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html

filterLowPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterLowPassBQ = undefined

filterHighPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterHighPassBQ = undefined

filterBandPassBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterBandPassBQ = undefined

filterNotchBQ :: forall p. (Clock p) => Signal p (Double, Double, Double) Double
filterNotchBQ = undefined



-- Percussive sounds
-- Kick drum
--    Use a sine or saw oscillator
--    Start with a high frequency and quickly modulate it down
--    Use an amplitude envelope with very short attack

-- Snare drum
--    Layer one
--        Similar to kick drum
--        Use a sine oscillator
--        Use a short decay/release
--    Layer two
--        Use white noise
--        Use a high-pass filter
--        Use a longer envelope

-- Clap
--    Use white noise
--    Use a high-pass or band-pass filter
--    Get creative with the envelope
--    Consider using multiple layers with different delays

-- You can also look for resurces into physical modeling
-- Then you can recreate the sound additively
-- https://www.soundonsound.com/techniques/synthesizing-percussion

-- Melodic sounds
--
-- Basic 
--    Pick an oscillator (start with square or saw)
--    Add a suitable envelope
--
-- Ideas
--    Add a second or third oscillator detuned from the first
--    Add a resonant low-pass or high-pass filter
--    Start modulating things
--        Use another oscillator to modulate the frequencies of either oscillator or filter
--        Add an envelope to the filter frequency
--        AM Synthesis
--        FM Synthesis
--            https://en.wikipedia.org/wiki/Frequency_modulation_synthesis