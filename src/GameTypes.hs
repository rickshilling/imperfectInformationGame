module GameTypes
  (
  History(..),
  InformationSet(..),
  InformationMap(..),
  Sigma(..),
  TreeElement(..)
  ) where
import Data.Map
import Data.Set

type History action = [action]
type InformationSet action = Set (History action)
type InformationMap action = Map (Set action) (InformationSet action)
type Sigma action = History action -> action -> Float

data TreeElement player action = TreeElement {
  getPlayer  :: Maybe player,
  fromAction :: Maybe action
}

instance (Show player, Show action) => Show (TreeElement player action) where {
  show (TreeElement p a) = show p ++ ", " ++ show a
};

instance (Eq player, Eq action) => Eq (TreeElement player action) where {
  TreeElement p1 a1 == TreeElement p2 a2 = (p1 == p2) && (a1 == a2)
};

