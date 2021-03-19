module GameTypes
  (
  InformationSets(..),
  GameTree(..),
  History(..),
  Sigma(..)
  ) where
import Control.Monad
import Data.Map
import Data.Set

type InformationSets action = Map (Set action) (Set [action])
data GameTree player action = GameNode {
  rootLabel :: player,
  subForest :: [(action, Maybe (GameTree player action))]
}
type History action = [action]

{-
class Sigma action where
  value :: History action -> action -> Float
-}
type Sigma action = History action -> action -> Float
