{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Euterpea
import Helpers
import HSoM
import FRP.UISF
import System.Random
import System.Random.Distributions


asNote2 :: Pitch -> [MidiMessage]
asNote2 x = [ANote 0 n 64 0.05]
            where n = absPitch x


sGen :: StdGen
sGen = mkStdGen 42


asMidiMessage :: Int -> SEvent [MidiMessage]
asMidiMessage p = Just [ANote 0 p 64 0.05]


randFreq :: Double
randFreq = head $ randomRs (1,10) sGen



threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  -- rf <- liftAIO randomRIO -< (0.1, 1.0)

  f <- title "Frequency" $ withDisplay (hSlider (0.1, 10) 0.1) -< ()
  tick <- timer -< 1/f

  m2 <- hold [ANote 0 1 64 0.05] -< m
  _ <- title "Midi in" display -< m2

  r1 <- liftAIO randomRIO -< (30, 70)
  r2 <- liftAIO randomRIO -< (30, 70)

  _ <- title "R1" display -< asMidiMessage r1
  _ <- title "R2" display -< asMidiMessage r2

  midiOut -< (mo, fmap const asMidiMessage r1 tick)

runThree = runMUI (styling "Blah!" (300, 600)) threeUI
