{-# LANGUAGE Arrows #-}

module Helpers (
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


styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
