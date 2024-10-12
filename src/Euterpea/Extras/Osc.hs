module Euterpea.Extras.Osc
  ( sine,
    sine_,
    saw,
    saw_,
    square,
    square_,
    squareWidth,
    squareWidth_,
    triangle,
    triangle_,
    triangleWidth,
    triangleWidth_,
    phasor,
    phasor_,
  )
where

import Euterpea
import Euterpea.Extras.Utils (linear)

-- Wavetable Settings

tableSize :: Int
tableSize = 16384

partialsSize :: Int
partialsSize = 40

-- Helper functions

-- Partials (Fourier series)

squarePartials :: Int -> [Double]
squarePartials pn = take pn $ [if odd p then 1 / dp else 0 | p <- [1 ..], let dp = fromIntegral p]

sawPartials :: Int -> [Double]
sawPartials pn = take pn $ [(if odd p then 1 else -1) / dp | p <- [1 ..], let dp = fromIntegral p]

trianglePartials :: Int -> [Double]
trianglePartials pn = take pn $ [if odd p then (if even hp then 1 else -1) / (dp * dp) else 0 | p <- [1 ..], let dp = fromIntegral p, let hp = p `div` 2]

-- Wavetables

sineTable :: Table
sineTable = tableSinesN tableSize [1]

sawTable :: Table
sawTable = tableLinearN tableSize (-1.0) [(1.0, 1.0)]

triangleTable :: Table
triangleTable = tableLinearN tableSize (-1.0) [(0.5, 1.0), (0.5, -1.0)]

phaseTable :: Table
phaseTable = tableLinearN tableSize 0.0 [(1.0, 1.0)]

squareTableP :: Table
squareTableP = tableSinesN tableSize $ squarePartials partialsSize

sawTableP :: Table
sawTableP = tableSinesN tableSize $ sawPartials partialsSize

triangleTableP :: Table
triangleTableP = tableSinesN tableSize $ trianglePartials partialsSize

-- Oscillators

-- Sine

sineF_ :: Double -> AudSF () Double
sineF_ = oscFixed

sineT_ :: Double -> AudSF () Double
sineT_ freq = proc _ -> do
  osc sineTable 0 -< freq

sineT :: AudSF Double Double
sineT = proc freq -> do
  osc sineTable 0 -< freq

sine :: AudSF Double Double
sine = sineT

sine_ :: Double -> AudSF () Double
sine_ = sineF_

-- Saw

sawT_ :: Double -> AudSF () Double
sawT_ freq = proc _ -> do
  osc sawTable 0 -< freq

sawTP_ :: Double -> AudSF () Double
sawTP_ freq = proc _ -> do
  osc sawTableP 0 -< freq

sawT :: AudSF Double Double
sawT = proc freq -> do
  osc sawTable 0 -< freq

sawTP :: AudSF Double Double
sawTP = proc freq -> do
  osc sawTableP 0 -< freq

saw :: AudSF Double Double
saw = sawT

saw_ :: Double -> AudSF () Double
saw_ = sawT_

-- Square

squareF_ :: Double -> AudSF () Double
squareF_ freq = proc _ -> do
  y <- sineF_ freq -< ()
  outA -< if y > 0 then 1.0 else -1.0

squareTP_ :: Double -> AudSF () Double
squareTP_ freq = proc _ -> do
  osc squareTableP 0 -< freq

squareTP :: AudSF Double Double
squareTP = proc freq -> do
  osc squareTableP 0 -< freq

square :: AudSF Double Double
square = squareTP

square_ :: Double -> AudSF () Double
square_ = squareF_

-- Square w/ variable width

squareWidthT_ :: Double -> Double -> AudSF () Double
squareWidthT_ freq width = proc _ -> do
  y <- osc phaseTable 0 -< freq
  outA -< if y > width then 1.0 else -1.0

squareWidthT :: AudSF (Double, Double) Double
squareWidthT = proc (freq, width) -> do
  y <- osc phaseTable 0 -< freq
  outA -< if y > width then 1.0 else -1.0

squareWidth :: AudSF (Double, Double) Double
squareWidth = squareWidthT

squareWidth_ :: Double -> Double -> AudSF () Double
squareWidth_ = squareWidthT_

-- Triangle

triangleT_ :: Double -> AudSF () Double
triangleT_ freq = proc _ -> do
  osc triangleTable 0 -< freq

triangleTP_ :: Double -> AudSF () Double
triangleTP_ freq = proc _ -> do
  osc triangleTableP 0 -< freq

triangleT :: AudSF Double Double
triangleT = proc freq -> do
  osc triangleTable 0 -< freq

triangleTP :: AudSF Double Double
triangleTP = proc freq -> do
  osc triangleTableP 0 -< freq

triangle :: AudSF Double Double
triangle = triangleT

triangle_ :: Double -> AudSF () Double
triangle_ = triangleT_

-- Triangle w/ variable Width

triangleWidthT_ :: Double -> Double -> AudSF () Double
triangleWidthT_ freq width = proc _ -> do
  y <- osc phaseTable 0 -< freq
  outA -< if y < width then linear (0.0, width) (-1.0, 1.0) y else linear (width, 1.0) (-1.0, 1.0) y

triangleWidthT :: AudSF (Double, Double) Double
triangleWidthT = proc (freq, width) -> do
  y <- osc phaseTable 0 -< freq
  outA -< if y < width then linear (0.0, width) (-1.0, 1.0) y else linear (width, 1.0) (-1.0, 1.0) y

triangleWidth :: AudSF (Double, Double) Double
triangleWidth = triangleWidthT

triangleWidth_ :: Double -> Double -> AudSF () Double
triangleWidth_ = triangleWidthT_

--- Phasor (Like Saw, except it goes from 0.0 to 1.0)

phasorT_ :: Double -> AudSF () Double
phasorT_ freq = proc _ -> do
  osc phaseTable 0 -< freq

phasorT :: AudSF Double Double
phasorT = proc freq -> do
  osc phaseTable 0 -< freq

phasor_ :: Double -> AudSF () Double
phasor_ = phasorT_

phasor :: AudSF Double Double
phasor = phasorT