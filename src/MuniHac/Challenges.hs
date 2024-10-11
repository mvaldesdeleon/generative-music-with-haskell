{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MuniHac.Challenges () where

import Euterpea

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

-- signal, time
portamento :: AudSF (Double, Double) Double
portamento = undefined

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