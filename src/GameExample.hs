module GameExample
  (
  )where

import GameTypes
import GameFunctions
import qualified Data.Set as DS
import qualified Data.Map as DM

data Player = P1 | P2 | Chance deriving (Eq,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show)

g = GameNode Chance
  [
    (Heads, Just (GameNode P1
                  [(ActionLeft, Nothing),
                   (ActionRight, Just (GameNode P2
                                       [(Heads, Nothing),
                                        (Forfeit, Nothing),
                                        (Tails, Nothing)
                                       ]
                                      )
                   )
                  ]
                 )
    ),
    (Tails, Just (GameNode P1
                  [(ActionLeft, Nothing),
                   (ActionRight, Just (GameNode P2
                                       [(Heads, Nothing),
                                        (Forfeit, Nothing),
                                        (Tails, Nothing)
                                       ]
                                      )
                   )
                  ]
                 )
    )
  ] 

infoMapOfG = getInformationMap g
infoSetsOfG = getSetOfInfoSets infoMapOfG
historyOfG = _H infoMapOfG
terminalHistoryOfG = _Z g
anInfoSetOfG = _I infoSetsOfG [Heads,ActionRight]

actionsFromHeads = (_A g [Heads])
playerFromHeads = (_P g [Heads])
actionFromInfoSet = anInfoSetOfG >>= (_AofI g)

sigma :: (Show player, Show action, Ord action, Eq action) =>
  (GameTree player action) -> Sigma action
sigma g = \h -> \a -> helper (gameTraverse g h) a
  where
  helper Nothing _ = 0 :: Float
  helper (Just subTree) a = helper2 (getActions (subForest subTree)) a
  helper2 actionSet a = if DS.member a actionSet then (fromIntegral 1) / (fromIntegral (DS.size actionSet))
                        else 0 :: Float
