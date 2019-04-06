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

    rSeed <- title "Rand" $ hSlider (2.4, 4.0) 2.4 -< ()
    t <- timer -< fromIntegral f
    r <- accum 0.1 -< fmap (const (grow rSeed)) t
    _ <- display -< normalizeGrowth r

    max <- title "Frequency" $ withDisplay (hiSlider 1 (1, 8) 4) -< ()
    tick <- timer -< 8 / fromIntegral (f * max) * normalizeGrowth r

    notes <- leftRight $ title "Note Selection" $ checkGroup notes -< ()
    note <- hold 0 <<< maybeNote -< (tick, notes, 30)

    returnA -< (note, isPlaying, notes, tick)


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


normalizeGrowth :: Double -> Double
normalizeGrowth x = (/100) $ fromIntegral $ round $ (*100) $ (+0.42) x


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]


maybeRandNote :: UISF (Maybe (), [PitchClass]) (Maybe Int)
maybeRandNote = proc (_, notes) -> randNote -< notes


maybeTick :: UISF (Maybe (), [PitchClass], Int) (Maybe ())
maybeTick = proc (tick, notes, rule) -> do
    rec (a, b, c) <- delay (Nothing, Nothing, Nothing) -< (b, c, tick')
        let tick' = cellAutom rule (a, b, c)
    returnA -< tick'


cellAutom 30 state = cellAutom30 state
cellAutom rule state = cellAutom30 state


cellAutom30 (Just (), Just (), Just ()) = Nothing
cellAutom30 (Just (), Just (), Nothing) = Just ()
cellAutom30 (Just (), Nothing, Just ()) = Just ()
cellAutom30 (Just (), Nothing, Nothing) = Nothing
cellAutom30 (Nothing, Just (), Just ()) = Just ()
cellAutom30 (Nothing, Just (), Nothing) = Just ()
cellAutom30 (Nothing, Nothing, Just ()) = Just ()
cellAutom30 (Nothing, Nothing, Nothing) = Nothing


maybeNote :: UISF (Maybe (), [PitchClass], Int) (Maybe Int)
maybeNote = proc (tick, notes, rule) -> do
    mNote <- if isJust tick
      then do
        tick' <- maybeTick -< (tick, notes, rule)
        note <- maybeRandNote -< (tick', notes)
        returnA -< note
      else
        returnA -< Nothing
    returnA -< mNote


randNote :: UISF [PitchClass] (Maybe Int)
randNote = proc notes -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    []  -> Nothing
                    [n] -> Just $ pcToInt n
                    _   -> Just $ pcToInt $ notes !! i
    returnA -< note


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
