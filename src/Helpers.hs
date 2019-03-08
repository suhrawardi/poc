{-# LANGUAGE Arrows #-}

module Helpers (
    asMidiMessage,
    decay,
    getDeviceIDs,
    grow,
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


decay :: Time -> Double -> MidiMessage -> Maybe MidiMessage
decay dur r m =
  let f c k v d = if v > 0
                  then let v' = truncate(fromIntegral v * r)
                       in Just (ANote c k v' d)
                  else Nothing
  in case m of
    ANote c k v d      -> f c k v d
    Std (NoteOn c k v) -> f c k v dur
    _                  -> Nothing


grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)


notes = [("C", C), ("Cs", Cs),
         ("D", D), ("Ds", Ds),
         ("E", E),
         ("F", F), ("Fs", Fs),
         ("G", G), ("Gs", Gs),
         ("A", A), ("As", As),
         ("B", B), ("Bs", Bs)]


getDeviceIDs = topDown $
  proc () -> do
    mi <- selectInput -< ()
    mo <- selectOutput -< ()
    outA -< (mi, mo)


asMidiMessage :: Int -> Int -> [MidiMessage]
asMidiMessage channel freq = [ANote channel freq 64 0.05]


styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
