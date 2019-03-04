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


buttonsPanel = topDown $ proc _ -> do
    start <- edge <<< button "Start" -< ()
    stop <- edge <<< button "Stop" -< ()
    returnA -< (start, stop)


handleButtons :: UISF (SEvent a, SEvent b) Bool
handleButtons = proc (start, stop) -> do
    rec isPlaying <- delay False -< isPlaying'
        let isPlaying' = case (start, stop) of
                             (Just _, _) -> True
                             (_, Just _) -> False
                             _ -> isPlaying
    returnA -< isPlaying


throttle = proc (channel, freq, tick, isPlaying) -> do
    rec messages <- delay Nothing -< messages'
        let messages' = if isJust tick && isPlaying
                            then Just (asMidiMessage channel freq)
                            else Nothing
    returnA -< messages



asNote2 :: Int -> Pitch -> [MidiMessage]
asNote2 channel freq = [ANote 0 n 64 0.05]
            where n = absPitch freq


sGen :: StdGen
sGen = mkStdGen 42


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


randFreq :: Double
randFreq = head $ randomRs (1,10) sGen





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


  min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
  max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()

  minRand <- title "Min rand frq" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
  maxRand <- title "Max rand frq" $ withDisplay (hSlider (0.01, 10.0) 1.0) -< ()
  rf <- liftAIO randomRIO -< (minRand, maxRand)
  rate <- title "Frequency" $ withDisplay (hSlider (0.01, 10) 0.1) -< ()
  tick <- timer -< (1 / rate) / rf

  r1 <- liftAIO randomRIO -< (min, max)
  r2 <- liftAIO randomRIO -< (min, max)

  _ <- title "RF" display -< rf


  outMsg <- throttle -< (0, r1, tick, isPlaying)


  midiOut -< (mo, outMsg)

runThree = runMUI (styling "Blah!" (500, 800)) threeUI
