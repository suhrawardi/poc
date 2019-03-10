{-# LANGUAGE Arrows #-}

module Helpers (
    asMidiMessage,
    asTick,
    notes,
    sGen,
    styling
  ) where

import Euterpea
import HSoM
import FRP.UISF
import System.Random


sGen :: StdGen
sGen = mkStdGen 42


asTick :: MidiMessage -> Maybe ()
asTick ANote{}        = Just ()
asTick (Std NoteOn{}) = Just ()
asTick _              = Nothing


notes :: [(String, PitchClass)]
notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
