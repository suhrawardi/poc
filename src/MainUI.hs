{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Euterpea
import Helpers
import HSoM
import FRP.UISF
import MidiPanel
import System.Random
import System.Random.Distributions


randNote :: UISF [PitchClass] (Maybe Int)
randNote = proc notes -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    [] -> Nothing
                    _ -> Just $ pcToInt $ notes !! i
    returnA -< note


filterTick :: UISF (Maybe (), Int) (Maybe ())
filterTick = proc (dt, max) -> do
    rec (dt, cnt) <- delay (Nothing, 0) -< (dt, cnt')
        let dt' = if isJust dt && cnt == max then Just () else Nothing
            cnt' = if isJust dt
                   then if cnt == max then 0 else cnt + 1
                   else cnt
    returnA -< dt


throttle :: UISF (Int, Int, Maybe (), Bool) (Maybe [MidiMessage])
throttle = proc (channel, freq, tick, isPlaying) -> do
    rec messages <- delay Nothing -< messages'
        let messages' = if isJust tick && isPlaying
                            then Just (asMidiMessage channel freq)
                            else Nothing
    returnA -< messages


channelPanel :: UISF (Maybe ()) (Int, Bool, [PitchClass], Maybe ())
channelPanel = title "Channel" $ topDown $ proc ticker -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()

    i <- title "Frequency" $ withDisplay (hiSlider 1 (1, 16) 4) -< ()
    tick <- filterTick -< (ticker, i)

    notes <- leftRight $ title "Note Selection" $ checkGroup notes -< ()
    note <- hold 0 <<< randNote -< notes

    returnA -< (note, isPlaying, notes, tick)


statusPanel :: UISF (Int, Bool, Int, [PitchClass]) ()
statusPanel = leftRight $ proc (channel, isPlaying, note, notes) -> do
    _ <- title "On?" display -< isPlaying
    _ <- title "Channel" display -< channel
    _ <- title "Playing" display -< note
    _ <- title "Notes" display -< notes
    returnA -< ()


instrumentPanel :: UISF (Int, Maybe ()) (Maybe [MidiMessage])
instrumentPanel = topDown $ setSize (400, 800) $ proc (channel, ticker) -> do
    (note, isPlaying, notes, tick) <- channelPanel -< ticker

    _ <- statusPanel -< (channel, isPlaying, note, notes)

    outMsg <- throttle -< (channel, note, tick, isPlaying)
    returnA -< outMsg


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, masterTick) <- midiPanel -< ()

  out1 <- instrumentPanel -< (1, masterTick)
  midiOut -< (mo, out1)

  out2 <- instrumentPanel -< (2, masterTick)
  midiOut -< (mo, out2)

  out3 <- instrumentPanel -< (3, masterTick)
  midiOut -< (mo, out3)

  out4 <- instrumentPanel -< (4, masterTick)
  midiOut -< (mo, out4)

runMainUI = runMUI (styling "Composer!" (2000, 800)) mainUI
