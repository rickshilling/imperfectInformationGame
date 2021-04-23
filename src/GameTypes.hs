module GameTypes
  (
  GameTree(..),
  History(..),
  Sigma(..),
  InformationSet(..),
  InformationMap(..)
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

instance (Eq player, Eq action) => Eq (GameTree player action) where {
  GameNode r1 sf1 == GameNode r2 sf2 = (r1 == r2 && sf1 == sf2)
};


