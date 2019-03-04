{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Buttons
import Data.Maybe (isJust)
import Euterpea
import Helpers
import HSoM
import FRP.UISF
import System.Random
import System.Random.Distributions


throttle = proc (channel, freq, tick, isPlaying) -> do
    rec messages <- delay Nothing -< messages'
        let messages' = if isJust tick && isPlaying
                            then Just (asMidiMessage channel freq)
                            else Nothing
    returnA -< messages


randomPitchBound = proc _ -> do
    min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    returnA -< (min, max)


randomFreq = proc _ -> do
    max <- title "Max Rate" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
    frq <- title "Frequency" $ withDisplay (hSlider (0.01, 10.0) 0.1) -< ()
    rate <- liftAIO randomRIO -< (0.1, max)
    returnA -< 1 / frq / rate


sGen :: StdGen
sGen = mkStdGen 42


threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  -- Display incoming Midi
  m2 <- hold [ANote 0 1 64 0.05] -< m
  _ <- title "Midi in" display -< m2

  -- Set whether it's playing
  (start, stop) <- buttonsPanel -< ()
  isPlaying <- handleButtons -< (start, stop)

  (minP, maxP) <- randomPitchBound -< ()
  r1 <- liftAIO randomRIO -< (minP, maxP)

  frq <- randomFreq -< ()
  tick <- timer -< frq


  outMsg <- throttle -< (0, r1, tick, isPlaying)


  midiOut -< (mo, outMsg)

runThree = runMUI (styling "Blah!" (800, 1200)) threeUI
