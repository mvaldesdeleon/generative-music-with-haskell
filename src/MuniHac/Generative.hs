module MuniHac.Generative () where

import Data.List (elemIndices, unfoldr)
import Euterpea
import System.Random

monoInstrument :: Instr (Mono AudRate)
monoInstrument duration pch vel params =
  let freq = apToHz pch
      amplitude = fromIntegral vel / 127
   in proc _ -> do
        -- Plug your AudSF here
        outA -< 0

stereoInstrument :: Instr (Stereo AudRate)
stereoInstrument duration pch vel params =
  let freq = apToHz pch
      amplitude = fromIntegral vel / 127
   in proc _ -> do
        -- Plug your AudSF here
        outA -< (0, 0)

mono2stereo :: Instr (Mono AudRate) -> Instr (Stereo AudRate)
mono2stereo mono duration pch vel params =
  let sf = mono duration pch vel params
   in proc _ -> do
        s <- sf -< ()
        outA -< (s, s)

monoInstr, stereoInstr :: InstrumentName
monoInstr = CustomInstrument "mono-instr"
stereoInstr = CustomInstrument "stereo-instr"

instrMap :: InstrMap (Stereo AudRate)
instrMap = [(monoInstr, mono2stereo monoInstrument), (stereoInstr, stereoInstrument)]

drumBeat :: Music AbsPitch
drumBeat =
  let g0 = mkStdGen 5
      nums = take 128 $ randomRs (50 :: AbsPitch, 85) g0
   in line $ map (note sn) nums

bassLine :: Music AbsPitch
bassLine =
  let g0 = mkStdGen 5
      nums = take 128 $ randomRs (50 :: AbsPitch, 85) g0
   in line $ map (note sn) nums

drums :: Music (AbsPitch, Volume)
drums = (\p -> (p, 40)) <$> instrument monoInstr drumBeat

bass :: Music (AbsPitch, Volume)
bass = (\p -> (p, 100)) <$> instrument stereoInstr bassLine

song :: Dur -> Music (AbsPitch, Volume)
song bpm = tempo (bpm / 60) (bass :=: drums)

recordSong :: IO ()
recordSong =
  let (duration, sf) = renderSF (song 90) instrMap
   in outFile "song.wav" duration sf

-- Can we do something better?

choose :: [a] -> StdGen -> (a, StdGen)
choose [] gen = error "Nothing to choose from!"
choose xs gen =
  let (i, gen') = first fromIntegral . genWord64 $ gen
   in (xs !! (i `mod` length xs), gen')

randomNote :: [AbsPitch] -> [Dur] -> Volume -> StdGen -> Maybe (Music (AbsPitch, Volume), StdGen)
randomNote pitches durations restTreshold gen =
  let (pch, gen') = choose pitches gen
      (dur, gen'') = choose durations gen'
      (vel, gen''') = randomR (0, 127) gen''
      x = if vel < restTreshold then rest dur else note dur (pch, vel)
   in Just (x, gen''')

randomMelody :: [AbsPitch] -> [Dur] -> Volume -> StdGen -> Music (AbsPitch, Volume)
randomMelody pitches durations restTreshold gen =
  let noteGen = randomNote pitches durations restTreshold
   in line $ unfoldr noteGen gen

pitches1, pitches2 :: [AbsPitch]
pitches1 = [60, 62, 63, 65, 67, 68, 70, 72, 74, 75, 77, 79] -- C-minor
pitches2 = [36, 36, 43, 43, 46, 48] -- also C-minor (root, 5th, 7th, root)

efMaj, fMin, cMin :: [AbsPitch]
efMaj = [3, 5, 7, 8, 10, 12, 14]
fMin = [5, 7, 8, 10, 12, 13, 15]
cMin = [0, 2, 3, 5, 7, 8, 10]

renderScale :: (AbsPitch, AbsPitch) -> [AbsPitch] -> [AbsPitch]
renderScale (from, to) scale =
  filter (\p -> from <= p && p <= to) $ concatMap (\i -> map (\p -> p + 12 * i) scale) [0 .. 10]

fitToScale :: [AbsPitch] -> AbsPitch -> AbsPitch
fitToScale [] pch = error "Empty pitch space"
fitToScale scale pch =
  let dists = map (\a -> abs (pch - a)) scale
   in scale !! head (elemIndices (minimum dists) dists)