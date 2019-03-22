{-# LANGUAGE Arrows #-}

module InstrumentPanel (
    instrumentPanel
  ) where

import Buttons
import Data.Maybe (isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


instrumentPanel :: UISF (Int, Int) (Maybe [MidiMessage])
instrumentPanel = topDown $ setSize (400, 800) $ proc (channel, f) -> do
    (note, isPlaying, notes, tick) <- channelPanel -< f

    _ <- statusPanel -< (channel, isPlaying, note, notes)

    outMsg <- throttle -< (channel, note, tick, isPlaying)
    returnA -< outMsg


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


channelPanel :: UISF Int (Int, Bool, [PitchClass], Maybe ())
channelPanel = title "Channel" $ topDown $ proc f -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()

    max <- title "Frequency" $ withDisplay (hiSlider 1 (1, 8) 4) -< ()
    tick <- timer -< 8 / fromIntegral (f * max)

    notes <- leftRight $ title "Note Selection" $ checkGroup notes -< ()
    note <- hold 0 <<< maybeNote -< (tick, notes)

    returnA -< (note, isPlaying, notes, tick)


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]


randNote :: UISF [PitchClass] (Maybe Int)
randNote = proc notes -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    [] -> Nothing
                    _ -> Just $ pcToInt $ notes !! i
    returnA -< note


maybeRandNote :: UISF (Maybe (), [PitchClass]) (Maybe Int)
maybeRandNote = proc (_, notes) -> randNote -< notes


maybeNote :: UISF (Maybe (), [PitchClass]) (Maybe Int)
maybeNote = proc (tick, notes) -> do
    mNote <- if isJust tick
      then do
        note <- maybeRandNote -< (tick, notes)
        returnA -< note
      else
        returnA -< Nothing
    returnA -< mNote


sGen :: StdGen
sGen = mkStdGen 42


statusPanel :: UISF (Int, Bool, Int, [PitchClass]) ()
statusPanel = leftRight $ proc (channel, isPlaying, note, notes) -> do
    _ <- title "On?" display -< isPlaying
    _ <- title "Channel" display -< channel
    _ <- title "Playing" display -< note
    _ <- title "Notes" display -< notes
    returnA -< ()


throttle :: UISF (Int, Int, Maybe (), Bool) (Maybe [MidiMessage])
throttle = proc (channel, freq, tick, isPlaying) -> do
    rec messages <- delay Nothing -< messages'
        let messages' = if isJust tick && isPlaying
                            then Just (asMidiMessage channel freq)
                            else Nothing
    returnA -< messages
