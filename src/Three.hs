{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Data.Maybe (isJust)
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


asMidiMessage :: Int -> [MidiMessage]
asMidiMessage p = [ANote 0 p 64 0.05]


randFreq :: Double
randFreq = head $ randomRs (1,10) sGen


threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  -- rf <- liftAIO randomRIO -< (0.1, 1.0)
  min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
  max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()

  rate <- title "Frequency" $ withDisplay (hSlider (0.1, 10) 0.1) -< ()
  tick <- timer -< 1/rate

  m2 <- hold [ANote 0 1 64 0.05] -< m
  _ <- title "Midi in" display -< m2

  r1 <- liftAIO randomRIO -< (min, max)
  r2 <- liftAIO randomRIO -< (min, max)

  _ <- title "R1" display -< asMidiMessage r1
  _ <- title "R2" display -< asMidiMessage r2

  let outMsgs = if isJust tick then Just (asMidiMessage r1) else Nothing

  midiOut -< (mo, outMsgs)

runThree = runMUI (styling "Blah!" (500, 800)) threeUI
