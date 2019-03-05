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


sGen :: StdGen
sGen = mkStdGen 42


decay :: Time -> Double -> MidiMessage -> Maybe MidiMessage
decay dur r m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * r)
                       in Just (ANote c k v' d)
                  else Nothing
  in case m of
    ANote c k v d      -> f c k v d
    Std (NoteOn c k v) -> f c k v dur
    _                  -> Nothing


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


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


channelPanel = title "Channel" $ leftRight $ proc _ -> do
    (minP, maxP) <- randomPitchBound -< ()
    pitch <- liftAIO randomRIO -< (minP, maxP)
    isPlaying <- buttonsPanel >>> handleButtons -< ()
    returnA -< (pitch, isPlaying)


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


threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  m2 <- delayPanel -< m
  _ <- displayMidiMessage -< m2

  (r1, isPlaying) <- channelPanel -< ()

  f <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
  tick <- timer -< 1 / f
  _ <- title "Tick" display -< tick

  outMsg <- throttle -< (0, r1, tick, isPlaying)

  midiOut -< (mo, outMsg)

runThree = runMUI (styling "Blah!" (800, 1200)) threeUI
