{-# LANGUAGE Arrows #-}

module CellularAutomata (
    cellularAutomata,
    rules,
    toRule
  ) where


rules :: [(String, Int)]
rules = [("255", 255), ("30", 30), ("54", 54), ("64", 64), ("73", 73),
         ("105", 105), ("109", 109), ("110", 110), ("126", 126), ("150", 150)]


toRule :: Int -> Int
toRule i = snd $ rules !! i


cellularAutomata :: Int -> (Maybe (), Maybe (), Maybe ()) -> Maybe ()
cellularAutomata 30 state = cellAutom30 state
cellularAutomata 54 state = cellAutom54 state
cellularAutomata 64 state = cellAutom64 state
cellularAutomata 73 state = cellAutom73 state
cellularAutomata 105 state = cellAutom105 state
cellularAutomata 109 state = cellAutom109 state
cellularAutomata 110 state = cellAutom110 state
cellularAutomata 126 state = cellAutom126 state
cellularAutomata 150 state = cellAutom150 state
cellularAutomata _ state = Just ()


cellAutom30 (Just (), Just (), Just ()) = Nothing
cellAutom30 (Just (), Just (), Nothing) = Just ()
cellAutom30 (Just (), Nothing, Just ()) = Just ()
cellAutom30 (Just (), Nothing, Nothing) = Nothing
cellAutom30 (Nothing, Just (), Just ()) = Just ()
cellAutom30 (Nothing, Just (), Nothing) = Just ()
cellAutom30 (Nothing, Nothing, Just ()) = Just ()
cellAutom30 (Nothing, Nothing, Nothing) = Nothing


cellAutom54 (Just (), Just (), Just ()) = Nothing
cellAutom54 (Just (), Just (), Nothing) = Nothing
cellAutom54 (Just (), Nothing, Just ()) = Just ()
cellAutom54 (Just (), Nothing, Nothing) = Just ()
cellAutom54 (Nothing, Just (), Just ()) = Just ()
cellAutom54 (Nothing, Just (), Nothing) = Nothing
cellAutom54 (Nothing, Nothing, Just ()) = Nothing
cellAutom54 (Nothing, Nothing, Nothing) = Just ()


cellAutom64 (Just (), Just (), Just ()) = Nothing
cellAutom64 (Just (), Just (), Nothing) = Just ()
cellAutom64 (Just (), Nothing, Just ()) = Just ()
cellAutom64 (Just (), Nothing, Nothing) = Nothing
cellAutom64 (Nothing, Just (), Just ()) = Just ()
cellAutom64 (Nothing, Just (), Nothing) = Just ()
cellAutom64 (Nothing, Nothing, Just ()) = Just ()
cellAutom64 (Nothing, Nothing, Nothing) = Nothing


cellAutom73 (Just (), Just (), Just ()) = Nothing
cellAutom73 (Just (), Just (), Nothing) = Just ()
cellAutom73 (Just (), Nothing, Just ()) = Just ()
cellAutom73 (Just (), Nothing, Nothing) = Nothing
cellAutom73 (Nothing, Just (), Just ()) = Just ()
cellAutom73 (Nothing, Just (), Nothing) = Just ()
cellAutom73 (Nothing, Nothing, Just ()) = Just ()
cellAutom73 (Nothing, Nothing, Nothing) = Nothing


cellAutom105 (Just (), Just (), Just ()) = Nothing
cellAutom105 (Just (), Just (), Nothing) = Just ()
cellAutom105 (Just (), Nothing, Just ()) = Just ()
cellAutom105 (Just (), Nothing, Nothing) = Nothing
cellAutom105 (Nothing, Just (), Just ()) = Just ()
cellAutom105 (Nothing, Just (), Nothing) = Just ()
cellAutom105 (Nothing, Nothing, Just ()) = Just ()
cellAutom105 (Nothing, Nothing, Nothing) = Nothing


cellAutom109 (Just (), Just (), Just ()) = Nothing
cellAutom109 (Just (), Just (), Nothing) = Just ()
cellAutom109 (Just (), Nothing, Just ()) = Just ()
cellAutom109 (Just (), Nothing, Nothing) = Nothing
cellAutom109 (Nothing, Just (), Just ()) = Just ()
cellAutom109 (Nothing, Just (), Nothing) = Just ()
cellAutom109 (Nothing, Nothing, Just ()) = Just ()
cellAutom109 (Nothing, Nothing, Nothing) = Nothing


cellAutom110 (Just (), Just (), Just ()) = Nothing
cellAutom110 (Just (), Just (), Nothing) = Just ()
cellAutom110 (Just (), Nothing, Just ()) = Just ()
cellAutom110 (Just (), Nothing, Nothing) = Nothing
cellAutom110 (Nothing, Just (), Just ()) = Just ()
cellAutom110 (Nothing, Just (), Nothing) = Just ()
cellAutom110 (Nothing, Nothing, Just ()) = Just ()
cellAutom110 (Nothing, Nothing, Nothing) = Nothing


cellAutom126 (Just (), Just (), Just ()) = Nothing
cellAutom126 (Just (), Just (), Nothing) = Just ()
cellAutom126 (Just (), Nothing, Just ()) = Just ()
cellAutom126 (Just (), Nothing, Nothing) = Nothing
cellAutom126 (Nothing, Just (), Just ()) = Just ()
cellAutom126 (Nothing, Just (), Nothing) = Just ()
cellAutom126 (Nothing, Nothing, Just ()) = Just ()
cellAutom126 (Nothing, Nothing, Nothing) = Nothing


cellAutom150 (Just (), Just (), Just ()) = Nothing
cellAutom150 (Just (), Just (), Nothing) = Just ()
cellAutom150 (Just (), Nothing, Just ()) = Just ()
cellAutom150 (Just (), Nothing, Nothing) = Nothing
cellAutom150 (Nothing, Just (), Just ()) = Just ()
cellAutom150 (Nothing, Just (), Nothing) = Just ()
cellAutom150 (Nothing, Nothing, Just ()) = Just ()
cellAutom150 (Nothing, Nothing, Nothing) = Nothing
