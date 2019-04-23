{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import Euterpea
import HSoM
import InstrumentPanel
import FRP.UISF
import MidiPanel


runMainUI = runMUI (styling "Composer" (1900, 800)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do

  (mo, f) <- midiPanel -< ()

  out1 <- instrumentPanel -< (1, f)
  midiOut -< (mo, out1)

  out2 <- instrumentPanel -< (2, f)
  midiOut -< (mo, out2)

  out3 <- instrumentPanel -< (3, f)
  midiOut -< (mo, out3)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
