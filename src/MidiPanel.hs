{-# LANGUAGE Arrows #-}

module MidiPanel (
    midiPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Euterpea
import HSoM
import FRP.UISF


midiPanel :: UISF () (Maybe OutputDeviceID, Maybe ())
midiPanel = topDown $ setSize (400, 600) $ proc _ -> do
    (mi, mo) <- getDeviceIDs -< ()
    m <- midiIn -< mi

    f <- title "Frequency" $ withDisplay (hiSlider 1 (1, 24) 1) -< ()
    tick <- timer -< 6 / fromIntegral f

    r <- title "Rand" $ hSlider (2.4, 4.0) 2.4 -< ()
    r2 <- accum 0.1 -< fmap (const (grow r)) tick
    tick2 <- timer -< (12 / fromIntegral f) * r2

    tick3 <- maybeTick -< m

    m2 <- delayPanel -< m
    _ <- displayMidiMessage -< m2

    returnA -< (mo, tick)


asTick :: MidiMessage -> Maybe ()
asTick ANote{}        = Just ()
asTick (Std NoteOn{}) = Just ()
asTick _              = Nothing


decay :: Time -> MidiMessage -> Maybe MidiMessage
decay dur m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * 2.4)
                       in Just (ANote c k v' d)
                  else Nothing
  in case m of
    ANote c k v d      -> f c k v d
    Std (NoteOn c k v) -> f c k v dur
    _                  -> Nothing


displayMidiMessage :: UISF (Maybe [MidiMessage]) (Maybe [MidiMessage])
displayMidiMessage = topDown $ proc m -> do
    m2 <- hold [ANote 0 1 64 0.05] -< m
    _ <- title "Midi in" display -< m2
    returnA -< m


delayPanel :: UISF (Maybe [MidiMessage]) (Maybe [MidiMessage])
delayPanel = title "Delay" $ topDown $ proc m -> do
    d <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.1) -< ()
    f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()
    rec s <- vdelay -< (1/f, fmap (mapMaybe (decay d)) m')
        let m' = mappend m s
    returnA -< s


getDeviceIDs :: UISF () (Maybe InputDeviceID, Maybe OutputDeviceID)
getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mi, mo)


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


maybeTick :: UISF (Maybe [MidiMessage]) (Maybe [()])
maybeTick = proc m ->
    returnA -< fmap (mapMaybe asTick) m
