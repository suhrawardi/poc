module Lib (
    playMusic
  ) where

import Euterpea

playMusic :: IO ()
playMusic = do
  let channel = 7
  playDev channel $ c 4 wn
