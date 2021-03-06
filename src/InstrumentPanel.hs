{-# LANGUAGE Arrows #-}

module InstrumentPanel (
    instrumentPanel
  ) where

import Buttons
import CellularAutomata
import Data.Maybe (isJust)
import Euterpea
import HSoM
import FRP.UISF
import System.Random


instrumentPanel :: UISF (Int, Int) (Maybe [MidiMessage])
instrumentPanel = topDown $ setSize (500, 800) $ proc (channel, f) -> do
    (note, isPlaying, notes, tick) <- channelPanel -< f

    _ <- statusPanel -< (channel, isPlaying, note, notes)

    outMsg <- throttle -< (channel, note, tick, isPlaying)
    returnA -< outMsg


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


freqPanel :: UISF Int (Bool, Maybe (), Double)
freqPanel = setSize (350, 460) $ topDown $ proc f -> do
    isPlaying <- buttonsPanel >>> handleButtons -< ()

    rSeed <- title "Rand" $ hSlider (2.4, 4.0) 2.4 -< ()
    t <- timer -< 1 / fromIntegral f
    r <- accum 0.1 -< fmap (const (grow rSeed)) t
    _ <- display -< normalizeGrowth r

    max <- title "Frequency" $ hiSlider 1 (1, 8) 4 -< ()
    _ <- display -< max
    tick <- timer -< 8 / fromIntegral (f * max) * normalizeGrowth r

    returnA -< (isPlaying, tick, r)


channelPanel :: UISF Int (Int, Bool, [PitchClass], Maybe ())
channelPanel = title "Channel" $ leftRight $ proc f -> do
    (isPlaying, tick, r) <- freqPanel -< f

    notes <- topDown $ setSize (75, 460) $ title "Notes" $ checkGroup notes -< ()
    i <- topDown $ setSize (75, 460) $ title "Rule" $ radio (fmap fst rules) 0 -< ()
    note <- hold 0 <<< maybeNote -< notes
    tick' <- maybeTick -< (tick, toRule i)

    returnA -< (note, isPlaying, notes, tick')


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


maybeNote :: UISF [PitchClass] (Maybe Int)
maybeNote = proc notes -> randNote -< notes


maybeTick :: UISF (Maybe (), Int) (Maybe ())
maybeTick = proc (tick, rule) -> do
    rec (a, b, c) <- delay (Nothing, Nothing, Nothing) -< (b, c, tick')
        let tick' = cellularAutomata rule (a, b, c)
    returnA -< tick'


randNote :: UISF [PitchClass] (Maybe Int)
randNote = proc notes -> do
    i <- liftAIO randomRIO -< (0, length notes - 1)
    let note = case notes of
                    []  -> Nothing
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
