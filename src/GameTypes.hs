module GameTypes
  (
  GameTree(..),
  History(..),
  Sigma(..),
  InformationSet(..),
  InformationMap(..),
  GameState(..),
  TreeElement(..),
  NewGameTree(..)
  ) where
import Control.Monad
import Data.Map
import Data.Set
import qualified Data.Set as DS
import Control.Monad.State hiding (State)

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

data GameState player action = GameState {
  location :: History action,
  game :: GameTree player action
}

data TreeElement player action = TreeElement {
  getPlayer  :: Maybe player,
  fromAction :: Maybe action
} 

instance (Show player, Show action) => Show (TreeElement player action) where {
  show (TreeElement p a) = show p ++ ", " ++ show a
};

data NewGameTree player action = Tree (TreeElement player action)

