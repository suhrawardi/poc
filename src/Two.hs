{-# LANGUAGE Arrows #-}

module Two (
    rrun
  ) where

import Euterpea
import Helpers
import HSoM
import FRP.UISF


secondCounter :: UISF () ()
secondCounter = proc () -> do
  ap <- viSlider 1 (0, 100) 0 -< ()
  display -< pitch ap
  rec count <- fcdelay 0 1 -< count + 1
  display -< count

ui5 :: UISF () ()
ui5 = proc _ -> do
  (_, devid) <- getDeviceIDs -< ()
  ap <- title "Absolute Pitch" (hiSlider 1 (0, 100) 0) -< ()
  f <- title "Tempo" (hSlider (1,10) 1) -< ()
  title "Pitch" display -< pitch ap
  tick <- timer -< 1/f
  midiOut -< (devid, fmap(const [ANote 0 ap 100 0.1]) tick)


ui3 :: UISF () ()
ui3 = proc _ -> do
  (_, mo) <- getDeviceIDs -< ()
  ap <- title "Absolute Pitch" (hiSlider 1 (0, 100) 0) -< ()
  title "Pitch" display -< pitch ap
  uap <- unique -< ap
  midiOut -< (mo, fmap (\k -> [ANote 0 k 100 0.1]) uap)

rrun = runMUI (styling "Midi Out / In" (400, 400)) ui5
