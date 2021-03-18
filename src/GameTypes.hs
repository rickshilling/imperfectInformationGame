{-# LANGUAGE EmptyDataDeriving #-}
module GameTypes
  (
  InformationSets(..),
  GameTree(..),
  ) where
import Control.Monad
import Data.Map
import Data.Set
type InformationSets action = Map (Set action) (Set [action])
data GameTree player action = GameNode {
  rootLabel :: player,
  subForest :: [(action, Maybe (GameTree player action))]
}
