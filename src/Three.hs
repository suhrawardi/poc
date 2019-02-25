{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Data.Maybe --(catMaybes, mapMaybe)
import Euterpea
import Helpers
import HSoM
import FRP.UISF


applyJust :: [Maybe (a -> a)] -> a -> a
applyJust = foldr (.) id . catMaybes

toM :: MidiMessage -> Maybe MidiMessage
toM (ANote c k v d)      = Just (ANote c k v d)
toM (Std (NoteOn c k v)) = Just (ANote c k v 0.1)
toM _                    = Nothing

grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)

popToNote :: Double -> [MidiMessage]
popToNote x = [ANote 0 n 64 0.05]
              where n = truncate (x * 127)


threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  f <- title "Frequency" $ withDisplay (hSlider (0.1, 10) 0.1) -< ()
  tick <- timer -< 1/f

  r <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
  pop <- accum 0.1 -< fmap (const (grow r)) tick

  rec m2 <- arr id -< m'
      let m' = fromMaybe m2 m
  _ <- title "Midi in" display -< m2

  midiOut -< (mo, fmap(const (popToNote pop)) tick)

runThree = runMUI (styling "Blah!" (300, 600)) threeUI
