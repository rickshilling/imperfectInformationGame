module GameExample
  (
  )where

import GameTypes
import GameFunctions
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Tree as DT
import qualified Control.Monad.State as CMS
import Data.Map.Internal.Debug

data Player = P1 | P2 | Chance deriving (Eq,Ord,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show)

gt =
  DT.Node (TreeElement (Just Chance) Nothing) [
  DT.Node (TreeElement (Just P1)     (Just Heads)) [
      DT.Node (TreeElement (Nothing) (Just ActionLeft)) [],
      DT.Node (TreeElement (Just P2) (Just ActionRight)) [
          DT.Node (TreeElement (Nothing) (Just Heads)) [],
          DT.Node (TreeElement (Nothing) (Just Forfeit)) [],
          DT.Node (TreeElement (Nothing) (Just Tails)) []
                                                         ]
                                                   ],
  DT.Node (TreeElement (Just P1)     (Just Tails)) [
      DT.Node (TreeElement (Nothing) (Just ActionLeft)) [],
      DT.Node (TreeElement (Just P2) (Just ActionRight)) [
          DT.Node (TreeElement (Nothing) (Just Heads)) [],
          DT.Node (TreeElement (Nothing) (Just Forfeit)) [],
          DT.Node (TreeElement (Nothing) (Just Tails)) []
                                                         ]
                                                   ]
                                              ]

actionsFromHeads = (_A gt [Just Heads])
playerFromHeads = (_P gt [Just Heads])
playerFromHeadsHeads = (_P gt [Just Heads,Just Heads])

infoMaps = getInfoMaps gt

_I_p1 = _I_i infoMaps (Just P1)
_I_p2 = _I_i infoMaps(Just P2)
_I_chance = _I_i infoMaps (Just Chance)
_I_nothing = _I_i infoMaps Nothing

