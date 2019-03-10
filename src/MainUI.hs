{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import Euterpea
import HSoM
import InstrumentPanel
import FRP.UISF
import MidiPanel


runMainUI = runMUI (styling "Composer" (2000, 800)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, masterTick) <- midiPanel -< ()

  out1 <- instrumentPanel -< (1, masterTick)
  midiOut -< (mo, out1)

  out2 <- instrumentPanel -< (2, masterTick)
  midiOut -< (mo, out2)

  out3 <- instrumentPanel -< (3, masterTick)
  midiOut -< (mo, out3)

  out4 <- instrumentPanel -< (4, masterTick)
  midiOut -< (mo, out4)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
