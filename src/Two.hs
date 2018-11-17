{-# LANGUAGE Arrows #-}

module Two (
    rrun
  ) where

import Euterpea
import HSoM
import FRP.UISF

secondCounter :: UISF () ()
secondCounter = proc () -> do
  ap <- viSlider 1 (0, 100) 0 -< ()
  display -< pitch ap
  rec count <- fcdelay 0 1 -< count + 1
  display -< count

rrun = runMUI' secondCounter
