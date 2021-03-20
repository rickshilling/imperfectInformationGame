module GameExample
  (
  )where

import GameTypes
import GameFunctions
import Data.Set

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

infoOnG = getInformationSets g
historyOnG = _H infoOnG

actionsFromHeads = (_A g [Heads])
playerFromHeads = (_P g [Heads])

sigma :: (Show player, Show action, Ord action, Eq action) =>
  (GameTree player action) -> Sigma action
sigma g = \h -> \a -> helper (gameTraverse g h) a
  where
  helper Nothing _ = 0 :: Float
  helper (Just subTree) a = helper2 (getActions (subForest subTree)) a
  helper2 actionSet a = if Data.Set.member a actionSet then (fromIntegral 1) / (fromIntegral (Data.Set.size actionSet))
                        else 0 :: Float 
