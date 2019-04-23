{-# LANGUAGE Arrows #-}

module CellularAutomata (
    cellularAutomata,
    rules,
    toRule
  ) where


rules :: [(String, Int)]
rules = [("255", 255), ("30", 30), ("45", 45), ("73", 73), ("75", 75),
         ("86", 86), ("89", 89), ("97", 97), ("101", 101), ("105", 105),
         ("109", 109), ("110", 110), ("124", 124), ("126", 126), ("135", 135),
         ("137", 137), ("149", 149), ("150", 150), ("153", 153), ("169", 169)]


rulesMatrix = [[0, 0, 0, 1, 1, 1, 1, 0], -- 30
               [0, 0, 1, 0, 1, 1, 0, 1], -- 45
               [0, 1, 0, 0, 1, 0, 0, 1], -- 73
               [0, 1, 0, 0, 1, 0, 1, 1], -- 75
               [0, 1, 0, 1, 0, 1, 1, 0], -- 86
               [0, 1, 0, 1, 1, 0, 0, 1], -- 89
               [0, 1, 1, 0, 0, 0, 0, 1], -- 97
               [0, 1, 1, 0, 0, 1, 0, 1], -- 101
               [0, 1, 1, 0, 1, 0, 0, 1], -- 105
               [0, 1, 1, 0, 1, 1, 0, 1], -- 109
               [0, 1, 1, 0, 1, 1, 1, 0], -- 110
               [1, 1, 1, 1, 1, 1, 0, 0], -- 124
               [0, 1, 1, 1, 1, 1, 1, 0], -- 126
               [1, 0, 0, 0, 0, 1, 1, 1], -- 135
               [1, 0, 0, 0, 1, 0, 0, 1], -- 137
               [1, 0, 0, 1, 0, 1, 0, 1], -- 149
               [1, 0, 0, 1, 0, 1, 1, 0], -- 150
               [1, 0, 0, 1, 1, 0, 0, 1], -- 153
               [1, 0, 1, 0, 1, 0, 0, 1], -- 169
               [1, 1, 1, 1, 1, 1, 1, 1]] -- 255


asIndex :: Int -> (Maybe (), Maybe (), Maybe ()) -> Maybe ()
asIndex rule (Just (), Just (), Just ()) = fromInt $ head (rulesMatrix !! rule)
asIndex rule (Just (), Just (), Nothing) = fromInt $ rulesMatrix !! rule !! 1
asIndex rule (Just (), Nothing, Just ()) = fromInt $ rulesMatrix !! rule !! 2
asIndex rule (Just (), Nothing, Nothing) = fromInt $ rulesMatrix !! rule !! 3
asIndex rule (Nothing, Just (), Just ()) = fromInt $ rulesMatrix !! rule !! 4
asIndex rule (Nothing, Just (), Nothing) = fromInt $ rulesMatrix !! rule !! 5
asIndex rule (Nothing, Nothing, Just ()) = fromInt $ rulesMatrix !! rule !! 6
asIndex rule (Nothing, Nothing, Nothing) = fromInt $ rulesMatrix !! rule !! 7


cellularAutomata :: Int -> (Maybe (), Maybe (), Maybe ()) -> Maybe ()
cellularAutomata rule = asIndex (asKey rule)


fromInt :: Int -> Maybe ()
fromInt 1 = Just ()
fromInt 0 = Nothing


asKey :: Int -> Int
asKey 30  = 0
asKey 45  = 1
asKey 73  = 2
asKey 75  = 3
asKey 86  = 4
asKey 89  = 5
asKey 97  = 6
asKey 101 = 7
asKey 105 = 8
asKey 109 = 9
asKey 110 = 10
asKey 124 = 11
asKey 126 = 12
asKey 135 = 13
asKey 137 = 14
asKey 149 = 15
asKey 150 = 16
asKey 153 = 17
asKey 169 = 18
asKey 255 = 19


toRule :: Int -> Int
toRule i = snd $ rules !! i
