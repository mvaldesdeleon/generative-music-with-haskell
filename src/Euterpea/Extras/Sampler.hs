module Euterpea.Extras.Sampler (importTable) where

import qualified Codec.Wav as Wav
import Data.Array.IArray (amap, elems)
import Data.Audio
  ( Audio (Audio, channelNumber, sampleData),
    SampleData,
    convert,
    sampleNumber,
  )
import Data.Int (Int32)
import Euterpea

importTable :: FilePath -> IO Table
importTable path = do
  res <- Wav.importFile path :: IO (Either String (Audio Int32))
  case res of
    Left err -> error $ "Could not load WAV: " ++ err
    Right Audio {channelNumber = cn, sampleData = sd} ->
      if cn /= 1
        then error $ "Please use a mono WAV file"
        else do
          let sdd = convert sd :: SampleData Double
              size = sampleNumber sdd
              maxabs = maximum . map abs . elems $ sdd
              normalized = amap (/ maxabs) sdd
          return $ undefined -- Table size normalized True