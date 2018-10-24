module One (
    playMusic
  ) where

import Euterpea

data Cluster = Cluster SNote [Cluster]
type SNote = (Dur, AbsPitch)

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat)
  where mkCluster note = Cluster note (map (mkCluster . addMult note) pat)

addMult :: SNote -> SNote -> SNote
addMult (d0,p0) (d1,p1) = (d0 * d1,p0 + p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe (n - 1)) cls

simToMusic :: [SNote] -> Music Pitch
simToMusic = line . map mkNote

mkNote :: (Dur,AbsPitch) -> Music Pitch
mkNote (d,ap) = note d (pitch ap)

ss pat n tr te = transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

m0 :: [SNote]
m0 = [(1,2), (1,0), (1,5), (1,7)]
tm0 = instrument AcousticGrandPiano (ss m0 4 50 20)

ttm0 = tm0 :=: transpose 12 (retro tm0)

playMusic :: IO ()
playMusic = do
  let channel = 2
  playDev channel tm0
