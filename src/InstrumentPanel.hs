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


instrumentPanel :: UISF (Int, Maybe ()) (Maybe [MidiMessage])
instrumentPanel = topDown $ setSize (400, 800) $ proc (channel, ticker) -> do
    (note, isPlaying, notes, tick) <- channelPanel -< ticker

    _ <- statusPanel -< (channel, isPlaying, note, notes)

    outMsg <- throttle -< (channel, note, tick, isPlaying)
    returnA -< outMsg


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


channelPanel :: UISF (Maybe ()) (Int, Bool, [PitchClass], Maybe ())
channelPanel = title "Channel" $ topDown $ proc ticker -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()

--    i <- title "Frequency" $ withDisplay (hiSlider 1 (1, 16) 4) -< ()
--    tick <- filterTick -< (ticker, i)

    tick <- arr id -< ticker
    _ <- display -< tick

    notes <- leftRight $ title "Note Selection" $ checkGroup notes -< ()
    note <- hold 0 <<< maybeNote -< (tick, notes)

    returnA -< (note, isPlaying, notes, tick)


filterTick :: UISF (Maybe (), Int) (Maybe ())
filterTick = proc (dt, max) -> do
    rec (dt, max, cnt) <- delay (Nothing, 1, 0) -< (dt', max', cnt')
        let dt' = dt
            max' = max
            cnt' = cnt
    mDt <- if isJust dt && cnt `mod` max == 0
      then returnA -< Just ()
      else returnA -< Nothing
    returnA -< mDt


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
