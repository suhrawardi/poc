{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
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


randomPitchBound = title "Pitch" $ topDown $ proc _ -> do
    min <- title "Min" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    max <- title "Max" $ withDisplay (hiSlider 1 (30, 70) 60) -< ()
    returnA -< (min, max)


channelPanel = title "Channel" $ topDown $ proc _ -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()

    notes <- leftRight $ title "Note Selection" $ checkGroup notes -< ()
    note <- hold 0 <<< randNote -< notes

    returnA -< (note, isPlaying, notes)


displayMidiMessage = topDown $ proc s -> do
    m2 <- hold [ANote 0 1 64 0.05] -< s
    _ <- title "Midi in" display -< m2
    returnA -< s


delayPanel = title "Delay" $ leftRight $ proc m -> do
    r <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
    d <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.1) -< ()
    f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()
    rec s <- vdelay -< (1/f, fmap (mapMaybe (decay 0.1 r)) m')
        let m' = mappend m s
    returnA -< s


midiPanel = topDown $ setSize (400, 600) $ proc _ -> do
    (mi, mo) <- getDeviceIDs -< ()
    m <- midiIn -< mi

    m2 <- delayPanel -< m
    _ <- displayMidiMessage -< m2

    f <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
    tick <- timer -< 1 / f

    returnA -< (mo, tick)


randNote = proc notes -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    [] -> Nothing
                    _ -> Just $ pcToInt $ notes !! i
    returnA -< note


statusPanel = leftRight $ proc (channel, isPlaying, note, notes) -> do
    _ <- title "On?" display -< isPlaying
    _ <- title "Channel" display -< channel
    _ <- title "Playing" display -< note
    _ <- title "Notes" display -< notes
    returnA -< ()


instrumentPanel = topDown $ setSize (400, 800) $ proc (channel, tick) -> do
    (note, isPlaying, notes) <- channelPanel -< ()

    _ <- statusPanel -< (channel, isPlaying, note, notes)

    f <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
    tick2 <- timer -< 1 / f
    _ <- title "Tick" display -< tick2

    outMsg <- throttle -< (channel, note, tick, isPlaying)
    returnA -< outMsg


threeUI :: UISF () ()
threeUI = leftRight $ proc _ -> do

  (mo, masterTick) <- midiPanel -< ()

  out1 <- instrumentPanel -< (0, masterTick)
  midiOut -< (mo, out1)

  out2 <- instrumentPanel -< (1, masterTick)
  midiOut -< (mo, out2)

  out3 <- instrumentPanel -< (2, masterTick)
  midiOut -< (mo, out3)

  out4 <- instrumentPanel -< (3, masterTick)
  midiOut -< (mo, out4)

runThree = runMUI (styling "Composer!" (2000, 800)) threeUI
