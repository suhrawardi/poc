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


sGen :: StdGen
sGen = mkStdGen 42


throttle = proc (channel, freq, tick, isPlaying) -> do
    rec messages <- delay Nothing -< messages'
        let messages' = if isJust tick && isPlaying
                            then Just (asMidiMessage channel freq)
                            else Nothing
    returnA -< messages


randomPitchBound = proc (a, b) -> do
    min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    returnA -< (min, max)


randomFreq = proc _ -> do
    max <- title "Max Rate" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
    fr <- title "Frequency" $ withDisplay (hSlider (0.01, 10.0) 0.1) -< ()
    rnd <- liftAIO randomRIO -< (0.1, max)
    rec frq <- delay 1.0 -< frq'
        let frq' = rnd / fr * 10
    returnA -< frq


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

  (minP, maxP) <- randomPitchBound -< (30, 70)
  r1 <- liftAIO randomRIO -< (minP, maxP)


  frq <- randomFreq -< ()
  _ <- title "Freak" display -< frq
  tick <- timer -< 1 / frq
  _ <- title "Tick" display -< tick


  outMsg <- throttle -< (0, r1, tick, isPlaying)


  midiOut -< (mo, outMsg)

runThree = runMUI (styling "Blah!" (800, 1200)) threeUI
