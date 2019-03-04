{-# LANGUAGE Arrows #-}

module Helpers (
    asMidiMessage,
    getDeviceIDs,
    styling
  ) where

import Euterpea
import HSoM
import FRP.UISF


getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mi, mo)


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
