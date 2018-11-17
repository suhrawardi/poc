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


chordIntervals :: [(String, [Int])]
chordIntervals = [("Maj",  [4, 3, 5]),    ("Maj7", [4, 3, 4, 1]),
                  ("Maj9", [4, 3, 4, 3]), ("Maj6", [4, 3, 4, 1]),
                  ("min", [3, 4, 5]),     ("min7", [3, 4, 3, 2]),
                  ("min9", [3, 4, 3, 4]), ("min7b5", [3, 3, 4, 2]),
                  ("mMaj7", [3, 4, 4, 1]),("dim", [3, 3, 3]),
                  ("dim7", [3, 3, 3, 3]), ("Dom7", [4, 3, 3, 2]),
                  ("Dom9", [4, 3, 3, 4]), ("Dom7b9", [4, 3, 3, 3])]

toChord :: Int -> MidiMessage -> [MidiMessage]
toChord i m =
  case m of
    Std (NoteOn c k v) -> f NoteOn c k v
    Std (NoteOff c k v) -> f NoteOff c k v
    _ -> []
  where f g c k v = map (\k' -> Std (g c k' v))
                        (scanl (+) k (snd (chordIntervals!!i)))

buildChord :: UISF () ()
buildChord = leftRight $
  proc _ -> do
    (mi, mo) <-getDeviceIDs -< ()
    m <- midiIn -< mi
    i <- topDown $ title "Chord Type" $
         radio (map fst chordIntervals) 0 -< ()
    midiOut -< (mo, fmap (concatMap $ toChord i) m)

-- rrun = runMUI (styling "Midi Out / In" (400, 400)) ui5
rrun = runMUI (styling "Chord Builder" (600, 400)) buildChord
