{-# LANGUAGE Arrows #-}

module BifurcateUI (
    runBifurcateUI
  ) where

import Data.Maybe
import Euterpea
import Helpers
import HSoM
import FRP.UISF


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

popToNote :: Double -> [MidiMessage]
popToNote x = [ANote 0 n 64 0.05]
              where n = truncate (x * 127)

bifurcateUI :: UISF () ()
bifurcateUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi
  f <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
  tick <- timer -< 1/f
  r <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
  pop <- accum 0.1 -< fmap (const (grow r)) tick
  _ <- title "Population" $ display -< pop
  d <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.1) -< ()
  f <- title "Echo frequency" $ withDisplay (hSlider (0, 10) 0) -< ()
  _ <- title "Midi in" $ display -< m
  rec s <- vdelay -< (1/f, fmap (mapMaybe (decay 0.1 r)) m')
      let m' = mappend m s
  midiOut -< (mo, m')
  midiOut -< (mo, fmap(const (popToNote pop)) tick)

runBifurcateUI = runMUI (styling "Bifurcate!" (300, 600)) bifurcateUI
