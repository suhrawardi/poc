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


randomPitchBound = title "Pitch" $ topDown $ proc (a, b) -> do
    min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    returnA -< (min, max)


channelPanel = title "Channel" $ leftRight $ proc (a, b) -> do
    (minP, maxP) <- randomPitchBound -< (a, b)
    pitch <- liftAIO randomRIO -< (minP, maxP)
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    returnA -< (pitch, isPlaying)


threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  -- Display incoming Midi
  m2 <- hold [ANote 0 1 64 0.05] -< m
  _ <- title "Midi in" display -< m2

  (r1, isPlaying) <- channelPanel -< (30, 70)

  f <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
  tick <- timer -< 1 / f
  _ <- title "Tick" display -< tick


  outMsg <- throttle -< (0, r1, tick, isPlaying)


  midiOut -< (mo, outMsg)

runThree = runMUI (styling "Blah!" (800, 1200)) threeUI
