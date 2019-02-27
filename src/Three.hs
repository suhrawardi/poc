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

  min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
  max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()

  minRand <- title "Min rand frq" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
  maxRand <- title "Max rand frq" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
  rf <- liftAIO randomRIO -< (minRand, maxRand)
  rate <- title "Frequency" $ withDisplay (hSlider (0.01, 10) 0.1) -< ()
  tick <- timer -< 1/(rate * rf)

  m2 <- hold [ANote 0 1 64 0.05] -< m
  _ <- title "Midi in" display -< m2

  r1 <- liftAIO randomRIO -< (min, max)
  r2 <- liftAIO randomRIO -< (min, max)

  _ <- title "RF" display -< rf

  let outMsgs = if isJust tick then Just (asMidiMessage r1) else Nothing

  midiOut -< (mo, outMsgs)

runThree = runMUI (styling "Blah!" (500, 800)) threeUI
