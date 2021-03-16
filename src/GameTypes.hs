{-# LANGUAGE EmptyDataDeriving #-}
module GameTypes
  (
  Player(..),
  Action(..),
  GameTree(..),
  InformationSets(..)
  ) where
import Control.Monad
import Data.Map
import Data.Set
--data Player = P1 | P2 | Chance deriving (Eq,Show) --Int
--data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show) --Int
data Action deriving (Eq,Ord,Show)
data Player deriving (Eq,Ord,Show)
data MyAction deriving (Eq,Ord,Show) 
data GameTree = GameNode {
  rootLabel :: Player,
  subForest :: [(Action, Maybe GameTree)]
}
data MyGameTree = MyGameNode {
  myRootLabel :: Player,
  mySubForest :: [(MyAction, Maybe MyGameTree)]
                             }
type InformationSets = Map (Set Action) (Set [Action])
