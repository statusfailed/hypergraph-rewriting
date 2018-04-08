module Rewrite where

testPattern = mkGraph v e where
  ixs = [0..4]
  name = ('V':) . show
  v = map name ixs
  e =
    [ mkEdge () [0] [1,2]
    , mkEdge () [0] [3,4]
    ]

testGraph = mkGraph v e where
  ixs = [0..5]
  name = ('V':) . show
  v = map name ixs
  e =
    [ mkEdge () [0] [1,2]
    , mkEdge () [0] [5,0]
    , mkEdge () [0] [3,4]
    ]

