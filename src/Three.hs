{-# LANGUAGE Arrows #-}

module Three (
    runThree
  ) where

import Data.Maybe --(catMaybes, mapMaybe)
import Text.Read (readMaybe)
import Euterpea
import Helpers
import HSoM
import FRP.UISF


asNote :: Pitch -> [MidiMessage]
asNote x = [ANote 0 n 64 0.05]
            where n = absPitch x

ps :: [Pitch]
ps = [(C,4), (D,4), (E,4)]



threeUI :: UISF () ()
threeUI = proc _ -> do
  (mi, mo) <- getDeviceIDs -< ()
  m <- midiIn -< mi

  f <- title "Frequency" $ withDisplay (hSlider (0.1, 10) 0.1) -< ()
  tick <- timer -< 1/f

  _ <- title "Midi in" display -< m

  midiOut -< (mo, fmap(const (asNote (C,4))) tick)

runThree = runMUI (styling "Blah!" (300, 600)) threeUI
