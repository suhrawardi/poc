{-# LANGUAGE Arrows #-}

module Two (
    runUI5
  ) where

import Euterpea
import Helpers
import HSoM
import FRP.UISF


ui5 :: UISF () ()
ui5 = proc _ -> do
  (_, devid) <- getDeviceIDs -< ()
  ap <- title "Absolute Pitch" (hiSlider 1 (0, 100) 0) -< ()
  f <- title "Tempo" (hSlider (1,10) 1) -< ()
  title "Pitch" display -< pitch ap
  tick <- timer -< 1/f
  midiOut -< (devid, fmap(const [ANote 0 ap 100 0.1]) tick)


runUI5 = runMUI (styling "Midi Out / In" (400, 400)) ui5
