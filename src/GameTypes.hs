module GameTypes
  (
  Player(..),
  Action(..),
  GameTree(..)
  ) where

import Control.Monad

data Player = P1 | P2 | Chance deriving (Eq,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit | A | B | C | D deriving (Eq,Ord,Show)

data GameTree = GameNode {
        rootLabel :: Player,
        subForest :: [(Action, Maybe GameTree)]
}
