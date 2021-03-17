{-# LANGUAGE EmptyDataDeriving #-}
module GameTypes
  (
  Player(..),
  Action(..),
  GameTree(..),
  InformationSets(..),
  --MyAction(..)
  ) where
import Control.Monad
import Data.Map
import Data.Set
data Player = P1 | P2 | Chance deriving (Eq,Show) --Int
--data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show) --Int

class Action a where
  specificActions :: [a]

type InformationSets a = Map (Set Action a) (Set [Action a])

data GameTree a = GameNode {
  rootLabel :: Player,
  subForest :: [(Action a, Maybe GameTree a)]
}
