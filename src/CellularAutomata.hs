{-# LANGUAGE Arrows #-}

module CellularAutomata (
    cellularAutomata,
    rules,
    toRule
  ) where


rules :: [(String, Int)]
rules = [("30", 30), ("31", 31)]


toRule :: Int -> Int
toRule i = snd $ rules !! i


cellularAutomata :: Int -> (Maybe (), Maybe (), Maybe ()) -> Maybe ()
cellularAutomata 30 state = cellAutom30 state
cellularAutomata _ state = cellAutom30 state


cellAutom30 (Just (), Just (), Just ()) = Nothing
cellAutom30 (Just (), Just (), Nothing) = Just ()
cellAutom30 (Just (), Nothing, Just ()) = Just ()
cellAutom30 (Just (), Nothing, Nothing) = Nothing
cellAutom30 (Nothing, Just (), Just ()) = Just ()
cellAutom30 (Nothing, Just (), Nothing) = Just ()
cellAutom30 (Nothing, Nothing, Just ()) = Just ()
cellAutom30 (Nothing, Nothing, Nothing) = Nothing
