module Lib (
    playMusic
  ) where

import Euterpea

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

twinkle1 =
  line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn :+:
  line (map pcToQN [F,F,E,E,D,D]) :+: g 4 hn :+:
  line (map pcToQN [G,G,F,F,E,E]) :+: g 4 hn :+:
  line (map pcToQN [G,G,F,F,E,E]) :+: g 4 hn

playMusic :: IO ()
playMusic = do
  let channel = 2
  playDev channel twinkle1
