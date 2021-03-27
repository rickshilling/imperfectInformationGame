module GameTypes
  (
  GameTree(..),
  History(..),
  Sigma(..),
  InformationMap(..),
  NewHistory(..),
  NewChoices(..),
  NewInformationSet(..),
  NewInformationMap(..)
  ) where
import Control.Monad
import Data.Map
import Data.Set
import qualified Data.Set as DS
data GameTree player action = GameNode {
  rootLabel :: player,
  subForest :: [(action, Maybe (GameTree player action))]
}
type History action = [action]
type InformationSet action = Set (History action)
type InformationMap action = Map (Set action) (InformationSet action)
type Sigma action = History action -> action -> Float

newtype NewHistory action = NewHistory [action] deriving (Eq, Show, Ord)

newtype NewChoices action = NewChoices {
  choices :: Set action
  } deriving (Eq, Show, Ord)

newtype NewInformationSet action = NewInformationSet {
  infoSet :: Set (NewHistory action)
  } deriving (Eq, Show, Ord)

type NewInformationMap action = Map (NewChoices action) (NewInformationSet action)

