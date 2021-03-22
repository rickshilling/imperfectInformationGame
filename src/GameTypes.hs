module GameTypes
  (
  GameTree(..),
  History(..),
  Sigma(..),
  InformationMap(..),
  NewHistory(..)
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
  choices :: Set action } deriving (Eq, Show, Ord)

newtype NewInformationSet action = NewInformationSet {
  infoSet :: Set (NewHistory action)} deriving (Eq, Show, Ord)

newtype NewInformationMap action = NewInformationMap {
  infoMap :: Map (NewChoices action) (NewInformationSet action)
  } deriving (Eq, Show, Ord)

data NewPlayer = NP1 | NP2 | NChance deriving (Eq,Show)
data NewAction = NHeads | NTails | NActionLeft | NActionRight | NForfeit deriving (Eq,Ord,Show)

nh = NewHistory [NHeads,NTails]
nc = NewChoices (DS.fromList [NHeads,NHeads])
lnh = [NewHistory [NHeads,NTails], NewHistory [NActionLeft,NActionRight]]
sng = DS.fromList lnh
pln = NewInformationSet $ DS.fromList [NewHistory [NHeads,NTails], NewHistory [NActionLeft,NActionRight]]

class MyGame action where
   h :: action

