module MuniHac.Generative () where

import Euterpea

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
drumBeat = undefined

bassLine :: Music AbsPitch
bassLine = undefined

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